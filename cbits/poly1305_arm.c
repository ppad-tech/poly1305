#include <stddef.h>
#include <stdint.h>
#include <string.h>

#if defined(__aarch64__)

#include <arm_neon.h>

/*
 * Poly1305 (RFC 8439).  ARM acceleration via two paths:
 *
 *  1. A scalar 26-bit-limb kernel using 64-bit native arithmetic for
 *     the limb multiplications.  Used for setup (precomputing r^k),
 *     for messages shorter than 64 bytes, and for the < 4-block tail
 *     of longer messages.  Also serves as the stage-1 reference; the
 *     NEON kernel below was added in a separate commit.
 *
 *  2. A NEON 4-way parallel kernel (when at least 4 full blocks
 *     remain).  Layout: 5 'uint32x4_t' limb vectors hold one limb
 *     position each across 4 message blocks; matching r-power
 *     vectors hold (r^4, r^3, r^2, r^1) at the same limb position.
 *     For each output position d_j we accumulate 5 partial products
 *     across both vector halves with 'vmull'/'vmlal', then
 *     horizontally sum the 4 lanes with 'vaddvq_u64'.  The same
 *     carry-propagation pattern as the scalar path reduces back to
 *     26-bit limbs.
 *
 * Reduction rule (used in both paths): 2^130 = 5 (mod 2^130 - 5),
 * so any partial product whose 'position' exceeds 4 folds back as
 * (5 * value) into position (pos - 5).
 */

#define MASK26 0x3ffffffu

/*
 * Multiply two 130-bit values mod (2^130 - 5).  Inputs in 5x 26-bit
 * limb form, output in 5x 26-bit limb form (each output limb < 2^26
 * except possibly limb 1, which may carry a small excess absorbed
 * by the next 'mul_mod_p' or by 'normalize').
 */
static void mul_mod_p(const uint32_t a[5], const uint32_t b[5],
                      uint32_t out[5]) {
    uint64_t d0, d1, d2, d3, d4, c;

    d0 = (uint64_t)a[0]*b[0]
       + 5 * ((uint64_t)a[4]*b[1] + (uint64_t)a[3]*b[2]
            + (uint64_t)a[2]*b[3] + (uint64_t)a[1]*b[4]);
    d1 = (uint64_t)a[0]*b[1] + (uint64_t)a[1]*b[0]
       + 5 * ((uint64_t)a[4]*b[2] + (uint64_t)a[3]*b[3]
            + (uint64_t)a[2]*b[4]);
    d2 = (uint64_t)a[0]*b[2] + (uint64_t)a[1]*b[1]
       + (uint64_t)a[2]*b[0]
       + 5 * ((uint64_t)a[4]*b[3] + (uint64_t)a[3]*b[4]);
    d3 = (uint64_t)a[0]*b[3] + (uint64_t)a[1]*b[2]
       + (uint64_t)a[2]*b[1] + (uint64_t)a[3]*b[0]
       + 5 * ((uint64_t)a[4]*b[4]);
    d4 = (uint64_t)a[0]*b[4] + (uint64_t)a[1]*b[3]
       + (uint64_t)a[2]*b[2] + (uint64_t)a[3]*b[1]
       + (uint64_t)a[4]*b[0];

    c = d0 >> 26; d0 &= MASK26; d1 += c;
    c = d1 >> 26; d1 &= MASK26; d2 += c;
    c = d2 >> 26; d2 &= MASK26; d3 += c;
    c = d3 >> 26; d3 &= MASK26; d4 += c;
    c = d4 >> 26; d4 &= MASK26; d0 += c * 5;
    c = d0 >> 26; d0 &= MASK26; d1 += c;

    out[0] = (uint32_t)d0;
    out[1] = (uint32_t)d1;
    out[2] = (uint32_t)d2;
    out[3] = (uint32_t)d3;
    out[4] = (uint32_t)d4;
}

/*
 * Parse 16 little-endian bytes plus a 'hibit' value (0 or 1) at bit
 * 128 into 5 26-bit limbs.
 */
static inline void blk2limbs(const uint8_t m[16], uint32_t hibit,
                              uint32_t l[5]) {
    uint32_t t0, t1, t2, t3;
    memcpy(&t0, m,     4);
    memcpy(&t1, m + 4, 4);
    memcpy(&t2, m + 8, 4);
    memcpy(&t3, m + 12, 4);
    l[0] =  t0                                & MASK26;
    l[1] = ((t0 >> 26) | (t1 <<  6))          & MASK26;
    l[2] = ((t1 >> 20) | (t2 << 12))          & MASK26;
    l[3] = ((t2 >> 14) | (t3 << 18))          & MASK26;
    l[4] =  (t3 >>  8) | (hibit << 24);
}

/*
 * Process one full 16-byte block: h := (h + m) * r mod p, scalar
 * implementation.
 */
static inline void scalar_block(uint32_t h[5], const uint32_t r[5],
                                const uint8_t m[16], uint32_t hibit) {
    uint32_t blk[5];
    blk2limbs(m, hibit, blk);

    uint32_t hl[5];
    hl[0] = h[0] + blk[0];
    hl[1] = h[1] + blk[1];
    hl[2] = h[2] + blk[2];
    hl[3] = h[3] + blk[3];
    hl[4] = h[4] + blk[4];

    mul_mod_p(hl, r, h);
}

/*
 * 4-block NEON update.  Computes:
 *   h := (h + m_0)*r^4 + m_1*r^3 + m_2*r^2 + m_3*r^1   mod p
 *
 * where m_0..m_3 are four consecutive 16-byte input blocks (so the
 * function consumes 64 bytes).  The polynomial identity is the
 * standard Horner expansion of 4 sequential block updates.
 */
static void neon4_block(uint32_t h[5],
                        const uint32_t r1[5], const uint32_t r2[5],
                        const uint32_t r3[5], const uint32_t r4[5],
                        const uint8_t m[64]) {
    /* Limbify the four blocks. */
    uint32_t b0[5], b1[5], b2[5], b3[5];
    blk2limbs(m,      1, b0);
    blk2limbs(m + 16, 1, b1);
    blk2limbs(m + 32, 1, b2);
    blk2limbs(m + 48, 1, b3);

    /* Fold the running accumulator into the first block. */
    b0[0] += h[0]; b0[1] += h[1]; b0[2] += h[2];
    b0[3] += h[3]; b0[4] += h[4];

    /* Pack messages: mv[i] = (b0[i], b1[i], b2[i], b3[i]). */
    uint32x4_t mv0 = { b0[0], b1[0], b2[0], b3[0] };
    uint32x4_t mv1 = { b0[1], b1[1], b2[1], b3[1] };
    uint32x4_t mv2 = { b0[2], b1[2], b2[2], b3[2] };
    uint32x4_t mv3 = { b0[3], b1[3], b2[3], b3[3] };
    uint32x4_t mv4 = { b0[4], b1[4], b2[4], b3[4] };

    /* Pack r-powers: rv[i] = (r^4[i], r^3[i], r^2[i], r^1[i]).      */
    uint32x4_t rv0 = { r4[0], r3[0], r2[0], r1[0] };
    uint32x4_t rv1 = { r4[1], r3[1], r2[1], r1[1] };
    uint32x4_t rv2 = { r4[2], r3[2], r2[2], r1[2] };
    uint32x4_t rv3 = { r4[3], r3[3], r2[3], r1[3] };
    uint32x4_t rv4 = { r4[4], r3[4], r2[4], r1[4] };

    /* 5 * r-powers, for partial products whose position wraps past
     * 4 (mod 2^130 - 5 = 5).                                       */
    uint32x4_t rv1_5 = vaddq_u32(vshlq_n_u32(rv1, 2), rv1);
    uint32x4_t rv2_5 = vaddq_u32(vshlq_n_u32(rv2, 2), rv2);
    uint32x4_t rv3_5 = vaddq_u32(vshlq_n_u32(rv3, 2), rv3);
    uint32x4_t rv4_5 = vaddq_u32(vshlq_n_u32(rv4, 2), rv4);

    /*
     * Output limb j = sum_i (mv[i] * appropriate r-power for shift j-i,
     * with 5x multiplier when j - i is negative).  Per output we sum
     * across the 4 lanes ('vaddvq_u64') after collecting both vmull
     * halves.
     */
#define MUL5_LO(m_, r_) vmull_u32(vget_low_u32(m_), vget_low_u32(r_))
#define MUL5_HI(m_, r_) vmull_high_u32(m_, r_)
#define MLA_LO(acc, m_, r_) \
    vmlal_u32(acc, vget_low_u32(m_), vget_low_u32(r_))
#define MLA_HI(acc, m_, r_) vmlal_high_u32(acc, m_, r_)

    uint64x2_t l0 = MUL5_LO(mv0, rv0);
    uint64x2_t h0 = MUL5_HI(mv0, rv0);
    l0 = MLA_LO(l0, mv1, rv4_5);  h0 = MLA_HI(h0, mv1, rv4_5);
    l0 = MLA_LO(l0, mv2, rv3_5);  h0 = MLA_HI(h0, mv2, rv3_5);
    l0 = MLA_LO(l0, mv3, rv2_5);  h0 = MLA_HI(h0, mv3, rv2_5);
    l0 = MLA_LO(l0, mv4, rv1_5);  h0 = MLA_HI(h0, mv4, rv1_5);
    uint64_t d0 = vaddvq_u64(l0) + vaddvq_u64(h0);

    uint64x2_t l1 = MUL5_LO(mv0, rv1);
    uint64x2_t h1 = MUL5_HI(mv0, rv1);
    l1 = MLA_LO(l1, mv1, rv0   );  h1 = MLA_HI(h1, mv1, rv0   );
    l1 = MLA_LO(l1, mv2, rv4_5);  h1 = MLA_HI(h1, mv2, rv4_5);
    l1 = MLA_LO(l1, mv3, rv3_5);  h1 = MLA_HI(h1, mv3, rv3_5);
    l1 = MLA_LO(l1, mv4, rv2_5);  h1 = MLA_HI(h1, mv4, rv2_5);
    uint64_t d1 = vaddvq_u64(l1) + vaddvq_u64(h1);

    uint64x2_t l2 = MUL5_LO(mv0, rv2);
    uint64x2_t h2 = MUL5_HI(mv0, rv2);
    l2 = MLA_LO(l2, mv1, rv1   );  h2 = MLA_HI(h2, mv1, rv1   );
    l2 = MLA_LO(l2, mv2, rv0   );  h2 = MLA_HI(h2, mv2, rv0   );
    l2 = MLA_LO(l2, mv3, rv4_5);  h2 = MLA_HI(h2, mv3, rv4_5);
    l2 = MLA_LO(l2, mv4, rv3_5);  h2 = MLA_HI(h2, mv4, rv3_5);
    uint64_t d2 = vaddvq_u64(l2) + vaddvq_u64(h2);

    uint64x2_t l3 = MUL5_LO(mv0, rv3);
    uint64x2_t h3 = MUL5_HI(mv0, rv3);
    l3 = MLA_LO(l3, mv1, rv2   );  h3 = MLA_HI(h3, mv1, rv2   );
    l3 = MLA_LO(l3, mv2, rv1   );  h3 = MLA_HI(h3, mv2, rv1   );
    l3 = MLA_LO(l3, mv3, rv0   );  h3 = MLA_HI(h3, mv3, rv0   );
    l3 = MLA_LO(l3, mv4, rv4_5);  h3 = MLA_HI(h3, mv4, rv4_5);
    uint64_t d3 = vaddvq_u64(l3) + vaddvq_u64(h3);

    uint64x2_t l4 = MUL5_LO(mv0, rv4);
    uint64x2_t h4 = MUL5_HI(mv0, rv4);
    l4 = MLA_LO(l4, mv1, rv3);  h4 = MLA_HI(h4, mv1, rv3);
    l4 = MLA_LO(l4, mv2, rv2);  h4 = MLA_HI(h4, mv2, rv2);
    l4 = MLA_LO(l4, mv3, rv1);  h4 = MLA_HI(h4, mv3, rv1);
    l4 = MLA_LO(l4, mv4, rv0);  h4 = MLA_HI(h4, mv4, rv0);
    uint64_t d4 = vaddvq_u64(l4) + vaddvq_u64(h4);

#undef MUL5_LO
#undef MUL5_HI
#undef MLA_LO
#undef MLA_HI

    /* Carry propagation, same shape as 'mul_mod_p'. */
    uint64_t c;
    c = d0 >> 26; d0 &= MASK26; d1 += c;
    c = d1 >> 26; d1 &= MASK26; d2 += c;
    c = d2 >> 26; d2 &= MASK26; d3 += c;
    c = d3 >> 26; d3 &= MASK26; d4 += c;
    c = d4 >> 26; d4 &= MASK26; d0 += c * 5;
    c = d0 >> 26; d0 &= MASK26; d1 += c;

    h[0] = (uint32_t)d0;
    h[1] = (uint32_t)d1;
    h[2] = (uint32_t)d2;
    h[3] = (uint32_t)d3;
    h[4] = (uint32_t)d4;
}

/*
 * Compute a 16-byte Poly1305 MAC over 'msg' (length 'msglen') using
 * the 32-byte 'key'.  Writes the tag to 'mac_out'.
 */
void poly1305_mac_arm(const uint8_t key[32], const uint8_t *msg,
                      size_t msglen, uint8_t mac_out[16]) {
    /* clamp r */
    uint32_t t0, t1, t2, t3;
    memcpy(&t0, key,      4);
    memcpy(&t1, key + 4,  4);
    memcpy(&t2, key + 8,  4);
    memcpy(&t3, key + 12, 4);
    t0 &= 0x0fffffffu;
    t1 &= 0x0ffffffcu;
    t2 &= 0x0ffffffcu;
    t3 &= 0x0ffffffcu;

    uint32_t r[5];
    r[0] =  t0                          & MASK26;
    r[1] = ((t0 >> 26) | (t1 <<  6))    & MASK26;
    r[2] = ((t1 >> 20) | (t2 << 12))    & MASK26;
    r[3] = ((t2 >> 14) | (t3 << 18))    & MASK26;
    r[4] =  (t3 >>  8);

    uint32_t h[5] = { 0, 0, 0, 0, 0 };

    size_t pos = 0;

    /* NEON 4-way path: amortizing the r^2/r^3/r^4 precomputation
     * (3 scalar mul_mod_p calls) needs about 4 NEON iterations to
     * break even versus the scalar block loop, so only engage it
     * when we have at least 16 full blocks (256 bytes).            */
    if (msglen >= 256) {
        uint32_t r2[5], r3[5], r4[5];
        mul_mod_p(r,  r,  r2);
        mul_mod_p(r2, r,  r3);
        mul_mod_p(r3, r,  r4);

        while (pos + 64 <= msglen) {
            neon4_block(h, r, r2, r3, r4, msg + pos);
            pos += 64;
        }
    }

    /* Scalar tail: any remaining full blocks (< 4 of them). */
    while (pos + 16 <= msglen) {
        scalar_block(h, r, msg + pos, 1);
        pos += 16;
    }

    /* Final partial block (1..15 trailing bytes), if any. */
    if (pos < msglen) {
        size_t rem = msglen - pos;
        uint8_t pad[16] = { 0 };
        memcpy(pad, msg + pos, rem);
        pad[rem] = 1;
        scalar_block(h, r, pad, 0);
    }

    /* normalize h (mul_mod_p may leave a small excess in h[1]) */
    {
        uint32_t c;
        c = h[1] >> 26; h[1] &= MASK26; h[2] += c;
        c = h[2] >> 26; h[2] &= MASK26; h[3] += c;
        c = h[3] >> 26; h[3] &= MASK26; h[4] += c;
        c = h[4] >> 26; h[4] &= MASK26; h[0] += c * 5;
        c = h[0] >> 26; h[0] &= MASK26; h[1] += c;
    }

    /* full reduction to [0, p) via constant-time conditional
     * subtraction of p */
    uint32_t g[5];
    uint32_t c = 5;
    g[0] = h[0] + c;   c = g[0] >> 26; g[0] &= MASK26;
    g[1] = h[1] + c;   c = g[1] >> 26; g[1] &= MASK26;
    g[2] = h[2] + c;   c = g[2] >> 26; g[2] &= MASK26;
    g[3] = h[3] + c;   c = g[3] >> 26; g[3] &= MASK26;
    g[4] = h[4] + c;
    uint32_t carry = g[4] >> 26;
    g[4] &= MASK26;
    uint32_t mask = (uint32_t)0 - carry;
    h[0] = (h[0] & ~mask) | (g[0] & mask);
    h[1] = (h[1] & ~mask) | (g[1] & mask);
    h[2] = (h[2] & ~mask) | (g[2] & mask);
    h[3] = (h[3] & ~mask) | (g[3] & mask);
    h[4] = (h[4] & ~mask) | (g[4] & mask);

    /* repack 5x 26-bit limbs into 4x 32-bit limbs */
    uint32_t h0 =  h[0]        | (h[1] << 26);
    uint32_t h1 = (h[1] >>  6) | (h[2] << 20);
    uint32_t h2 = (h[2] >> 12) | (h[3] << 14);
    uint32_t h3 = (h[3] >> 18) | (h[4] <<  8);

    /* add s (high 16 bytes of key), mod 2^128 */
    uint32_t s0, s1, s2, s3;
    memcpy(&s0, key + 16, 4);
    memcpy(&s1, key + 20, 4);
    memcpy(&s2, key + 24, 4);
    memcpy(&s3, key + 28, 4);

    uint64_t a0 = (uint64_t)h0 + s0;
    uint64_t a1 = (uint64_t)h1 + s1 + (a0 >> 32);
    uint64_t a2 = (uint64_t)h2 + s2 + (a1 >> 32);
    uint64_t a3 = (uint64_t)h3 + s3 + (a2 >> 32);

    uint32_t o0 = (uint32_t)a0;
    uint32_t o1 = (uint32_t)a1;
    uint32_t o2 = (uint32_t)a2;
    uint32_t o3 = (uint32_t)a3;

    memcpy(mac_out + 0,  &o0, 4);
    memcpy(mac_out + 4,  &o1, 4);
    memcpy(mac_out + 8,  &o2, 4);
    memcpy(mac_out + 12, &o3, 4);
}

int poly1305_arm_available(void) {
    return 1;
}

#else

void poly1305_mac_arm(const uint8_t *key, const uint8_t *msg,
                      size_t msglen, uint8_t *mac_out) {
    (void)key; (void)msg; (void)msglen; (void)mac_out;
}

int poly1305_arm_available(void) {
    return 0;
}

#endif
