{ pkgs ? import <nixpkgs> {}
, profiling ?  false
, doCheck ? true
}:
let
  odrv = with pkgs; with haskellPackages; developPackage {
    root = ./.;
    name = "CarpHask";
    modifier = lib.flip haskell.lib.overrideCabal (_: {
      buildTools = [
        cabal-install clang gdb
        ormolu hlint flamegraph ghc-prof-flamegraph
      ] ++ lib.optionals stdenv.isLinux [ linuxPackages.perf tinycc zig ];
      # Remove pkg-configDepends entirely
      enableLibraryProfiling = profiling;
      enableExecutableProfiling = profiling;
      enableSharedLibraries = false;
      enableSharedExecutables = false;
    });
  };
  drv = with pkgs; odrv.overrideAttrs (o: {
    inherit doCheck;
    buildInputs = o.buildInputs ++ [
      makeWrapper pkg-config
      SDL2 SDL2_image SDL2_mixer SDL2_ttf glfw  # moved here
    ];
    # Deduplicate the flags Nix assembled
    preBuild = ''
    export NIX_CFLAGS_COMPILE=$(echo "$NIX_CFLAGS_COMPILE" \
      | tr ' ' '\n' | awk '!seen[$0]++' | tr '\n' ' ')
    export NIX_LDFLAGS=$(echo "$NIX_LDFLAGS" \
      | tr ' ' '\n' | awk '!seen[$0]++' | tr '\n' ' ')
    '';
    postPatch = ''
      patchShebangs .
    '';
    postInstall = ''
      wrapProgram $out/bin/carp --set CARP_DIR $src --prefix PATH : ${clang}/bin
      wrapProgram $out/bin/carp-header-parse --set CARP_DIR $src --prefix PATH : ${clang}/bin
    '';
    postCheck = ''
      env CARP=dist/build/carp/carp scripts/run_carp_tests.sh -j 4
    '';
    enableParallelBuilding = true;
    });
in drv
