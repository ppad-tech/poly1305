#include <stddef.h>
#include <stdint.h>
#include <string.h>

#if defined(__aarch64__)

/*
 * Poly1305 (RFC 8439).  Stage 1 kernel: clean scalar C using the
 * 26-bit limb representation that NEON Poly1305 implementations use,
 * with 64-bit native arithmetic for the limb multiplications.  Stage
 * 2 (a separate commit) replaces the inner block loop with a NEON
 * 4-way parallel kernel.
 *
 * Layout: every 130-bit value is held as 5 uint32_t limbs of 26 bits
 * each (top limb gets the spare 2 bits).  The prime is p = 2^130 - 5,
 * so the reduction rule for any 'spilled' limb beyond position 4 is
 * 'add 5 * that limb back into the low end'.
 */

#define MASK26 0x3ffffffu

/*
 * Multiply two 130-bit values mod (2^130 - 5).  Inputs in 5x 26-bit
 * limb form, output in 5x 26-bit limb form (each output limb < 2^26
 * except possibly limb 1, which may carry a small excess absorbed by
 * the next 'mul_mod_p' or by 'normalize').
 */
static void mul_mod_p(const uint32_t a[5], const uint32_t b[5],
                      uint32_t out[5]) {
    uint64_t d0, d1, d2, d3, d4, c;

    /* 25 partial products; limbs that 'spill' beyond position 4 fold
     * back as (5 * limb) thanks to 2^130 = 5 mod p.                  */
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

    /* single-pass carry propagation */
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
 * 128 into 5 26-bit limbs.  'hibit' = 1 for a full block (the implicit
 * "+ 2^128" of Poly1305).  Partial blocks set the marker byte inside
 * the 16-byte buffer and pass 'hibit' = 0.
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
 * Compute a 16-byte Poly1305 MAC over 'msg' (length 'msglen') using
 * the 32-byte 'key'.  Writes the tag to 'mac_out'.  'key' is the
 * unclamped raw 32 bytes — clamping happens inside.
 */
void poly1305_mac_arm(const uint8_t key[32], const uint8_t *msg,
                      size_t msglen, uint8_t mac_out[16]) {
    /* clamp r (low 16 bytes of key, with specific bits cleared) */
    uint32_t t0, t1, t2, t3;
    memcpy(&t0, key,      4);
    memcpy(&t1, key + 4,  4);
    memcpy(&t2, key + 8,  4);
    memcpy(&t3, key + 12, 4);
    t0 &= 0x0fffffffu;
    t1 &= 0x0ffffffcu;
    t2 &= 0x0ffffffcu;
    t3 &= 0x0ffffffcu;

    /* r as 5 26-bit limbs */
    uint32_t r[5];
    r[0] =  t0                          & MASK26;
    r[1] = ((t0 >> 26) | (t1 <<  6))    & MASK26;
    r[2] = ((t1 >> 20) | (t2 << 12))    & MASK26;
    r[3] = ((t2 >> 14) | (t3 << 18))    & MASK26;
    r[4] =  (t3 >>  8);

    /* accumulator h starts at 0 */
    uint32_t h[5] = { 0, 0, 0, 0, 0 };

    /* process full 16-byte blocks */
    size_t pos = 0;
    while (pos + 16 <= msglen) {
        uint32_t blk[5];
        blk2limbs(msg + pos, 1, blk);

        /* h += blk (limb-wise; later carry-propagated inside mul) */
        uint32_t hl[5];
        hl[0] = h[0] + blk[0];
        hl[1] = h[1] + blk[1];
        hl[2] = h[2] + blk[2];
        hl[3] = h[3] + blk[3];
        hl[4] = h[4] + blk[4];

        /* h := (h + blk) * r mod p */
        mul_mod_p(hl, r, h);

        pos += 16;
    }

    /* final partial block (1..15 trailing bytes) */
    if (pos < msglen) {
        size_t rem = msglen - pos;
        uint8_t pad[16] = { 0 };
        memcpy(pad, msg + pos, rem);
        pad[rem] = 1;     /* RFC 8439: append "1" byte, then zero-pad */

        uint32_t blk[5];
        blk2limbs(pad, 0, blk);   /* no hibit; we set the marker inside */

        uint32_t hl[5];
        hl[0] = h[0] + blk[0];
        hl[1] = h[1] + blk[1];
        hl[2] = h[2] + blk[2];
        hl[3] = h[3] + blk[3];
        hl[4] = h[4] + blk[4];
        mul_mod_p(hl, r, h);
    }

    /* mul_mod_p leaves a small excess in h[1]; absorb it before the
     * final mod-p reduction.                                       */
    {
        uint32_t c;
        c = h[1] >> 26; h[1] &= MASK26; h[2] += c;
        c = h[2] >> 26; h[2] &= MASK26; h[3] += c;
        c = h[3] >> 26; h[3] &= MASK26; h[4] += c;
        c = h[4] >> 26; h[4] &= MASK26; h[0] += c * 5;
        c = h[0] >> 26; h[0] &= MASK26; h[1] += c;
    }

    /* full reduction to [0, p).  Compute g = h + 5; if g overflows
     * 2^130 (bit 130 set), then h >= p and we replace h with g (which
     * equals h - p mod 2^130).  Otherwise leave h alone.  Done in
     * constant time via a bitmask.                                */
    uint32_t g[5];
    uint32_t c = 5;
    g[0] = h[0] + c;   c = g[0] >> 26; g[0] &= MASK26;
    g[1] = h[1] + c;   c = g[1] >> 26; g[1] &= MASK26;
    g[2] = h[2] + c;   c = g[2] >> 26; g[2] &= MASK26;
    g[3] = h[3] + c;   c = g[3] >> 26; g[3] &= MASK26;
    g[4] = h[4] + c;
    uint32_t carry = g[4] >> 26;
    g[4] &= MASK26;
    uint32_t mask = (uint32_t)0 - carry;   /* all-1s iff carry */
    h[0] = (h[0] & ~mask) | (g[0] & mask);
    h[1] = (h[1] & ~mask) | (g[1] & mask);
    h[2] = (h[2] & ~mask) | (g[2] & mask);
    h[3] = (h[3] & ~mask) | (g[3] & mask);
    h[4] = (h[4] & ~mask) | (g[4] & mask);

    /* repack 5x 26-bit limbs into 4x 32-bit limbs (low 128 bits) */
    uint32_t h0 =  h[0]        | (h[1] << 26);
    uint32_t h1 = (h[1] >>  6) | (h[2] << 20);
    uint32_t h2 = (h[2] >> 12) | (h[3] << 14);
    uint32_t h3 = (h[3] >> 18) | (h[4] <<  8);

    /* add s (high 16 bytes of key) as 4x little-endian Word32, mod
     * 2^128 (drop the final 32-bit carry).                       */
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
