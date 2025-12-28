{
  description = "A pure Haskell Poly1305 message authentication code.";

  inputs = {
    ppad-base16 = {
      type = "git";
      url  = "git://git.ppad.tech/base16.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
    };
    ppad-fixed = {
      type = "git";
      url  = "git://git.ppad.tech/fixed.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
    };
    ppad-nixpkgs = {
      type = "git";
      url  = "git://git.ppad.tech/nixpkgs.git";
      ref  = "master";
    };
    flake-utils.follows = "ppad-nixpkgs/flake-utils";
    nixpkgs.follows = "ppad-nixpkgs/nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, ppad-nixpkgs
            , ppad-base16, ppad-fixed
            }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        lib = "ppad-poly1305";

        pkgs = import nixpkgs { inherit system; };
        hlib = pkgs.haskell.lib;
        llvm = pkgs.llvmPackages_19.llvm;

        base16 = ppad-base16.packages.${system}.default;
        base16-llvm =
          hlib.addBuildTools
            (hlib.enableCabalFlag base16 "llvm")
            [ llvm ];

        fixed = ppad-fixed.packages.${system}.default;
        fixed-llvm =
          hlib.addBuildTools
            (hlib.enableCabalFlag fixed "llvm")
            [ llvm ];

        hpkgs = pkgs.haskell.packages.ghc910.extend (new: old: {
          ppad-base16 = base16-llvm;
          ppad-fixed  = fixed-llvm;
          ${lib} = new.callCabal2nixWithOptions lib ./. "--enable-profiling" {
            ppad-fixed = new.ppad-fixed;
          };
        });

        cc    = pkgs.stdenv.cc;
        ghc   = hpkgs.ghc;
        cabal = hpkgs.cabal-install;
      in
        {
          packages.default = hpkgs.${lib};

          devShells.default = hpkgs.shellFor {
            packages = p: [
              (hlib.doBenchmark p.${lib})
            ];

            buildInputs = [
              cabal
              cc
              llvm
            ];

            doBenchmark = true;

            shellHook = ''
              PS1="[${lib}] \w$ "
              echo "entering ${system} shell, using"
              echo "cc:    $(${cc}/bin/cc --version)"
              echo "ghc:   $(${ghc}/bin/ghc --version)"
              echo "cabal: $(${cabal}/bin/cabal --version)"
              echo "llc:   $(${llvm}/bin/llc --version | head -2 | tail -1)"
            '';
          };
        }
      );
}

