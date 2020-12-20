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
      pkgconfigDepends = [ SDL2 SDL2_image SDL2_mixer SDL2_ttf glfw ];
      enableLibraryProfiling = profiling;
      enableExecutableProfiling = profiling;
      enableSharedLibraries = false;
      enableSharedExecutables = false;
    });
  };
  drv = with pkgs; odrv.overrideAttrs (o: {
    inherit doCheck;
    buildInputs = o.buildInputs ++ [ makeWrapper pkgconfig ];
    postPatch = ''
      patchShebangs .
    '';
    postInstall = ''
      wrapProgram $out/bin/carp --set CARP_DIR $src --prefix PATH : ${clang}/bin
      wrapProgram $out/bin/carp-header-parse --set CARP_DIR $src --prefix PATH : ${clang}/bin
    '';
    postCheck = ''
      env CARP=dist/build/carp/carp scripts/run_carp_tests.sh
    '';
    enableParallelBuilding = true;
    });
in drv
