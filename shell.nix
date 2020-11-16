{ nixpkgs ? import <nixpkgs> {}
, profiling ? false
, doBenchmark ? false
, doCheck ? false
}:
with nixpkgs;
(callPackage ./default.nix { inherit profiling doBenchmark doCheck; }).env.overrideAttrs (o: {
    buildInputs = with pkgs; o.buildInputs ++ [ haskellPackages.cabal-install clang gdb ]
                  ++ stdenv.lib.optionals stdenv.isLinux [ flamegraph linuxPackages.perf tinycc zig ];
  })
