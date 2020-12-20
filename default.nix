{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
, doBenchmark ? false
, profiling ? false
, doCheck ? true
}:

let

  inherit (nixpkgs) pkgs;

  optionals = nixpkgs.stdenv.lib.optionals;
  linuxOnly = optionals nixpkgs.stdenv.isLinux;

  f = { mkDerivation, stdenv
      , ansi-terminal, open-browser, base, blaze-html, blaze-markup
      , cmark, containers, directory, edit-distance, filepath, hashable
      , haskeline, HUnit, mtl, optparse-applicative, parsec, process, split, text
      , darwin, glfw3, SDL2, SDL2_image, SDL2_gfx, SDL2_mixer, SDL2_ttf
      , ghc-prof-flamegraph, ormolu, hlint
      , clang , makeWrapper
      , libXext, libXcursor, libXinerama, libXi, libXrandr, libXScrnSaver, libXxf86vm, libpthreadstubs, libXdmcp, libGL

      , cabal-install, gdb , flamegraph, linuxPackages, tinycc, zig
      }:
      mkDerivation {
        pname = "CarpHask";
        version = "dev";
        src = ./.;
        inherit doCheck doBenchmark;
        isLibrary = false;
        isExecutable = true;
        enableSharedLibraries = false;
        enableSharedExecutables = false;
        enableLibraryProfiling = profiling;
        enableExecutableProfiling = profiling;
        libraryHaskellDepends = [
          ansi-terminal open-browser base blaze-html blaze-markup cmark containers
          directory edit-distance filepath haskeline mtl parsec process split
          text hashable
        ] ++ optionals profiling [ ghc-prof-flamegraph ];
        pkgconfigDepends =
          [ glfw3 SDL2 SDL2_image SDL2_gfx SDL2_mixer SDL2_ttf ]
          ++ linuxOnly [ libXext libXcursor libXinerama libXi libXrandr libXScrnSaver libXxf86vm libpthreadstubs libXdmcp libGL];
        executableHaskellDepends = [
          base containers directory haskeline optparse-applicative parsec process
          clang
        ];
        executableToolDepends = optionals pkgs.lib.inNixShell (
          [ cabal-install clang gdb ormolu hlint ]
          ++ optionals stdenv.isLinux [ flamegraph linuxPackages.perf tinycc zig ]
        );
        executableFrameworkDepends = with darwin.apple_sdk.frameworks; optionals stdenv.isDarwin [
          Carbon Cocoa IOKit CoreFoundation CoreVideo IOKit ForceFeedback
        ];
        buildDepends = [ makeWrapper ];
        postPatch = ''
          patchShebangs .
        '';
        postInstall = ''
          wrapProgram $out/bin/carp --set CARP_DIR $src --prefix PATH : ${clang}/bin
          wrapProgram $out/bin/carp-header-parse --set CARP_DIR $src --prefix PATH : ${clang}/bin
        '';
        testHaskellDepends = [ base containers HUnit ];
        testTarget = "CarpHask-test";
        postCheck = ''
          env CARP=dist/build/carp/carp scripts/run_carp_tests.sh
        '';
        enableParallelBuilding = true;
        homepage = "https://github.com/eriksvedang/Carp";
        license = stdenv.lib.licenses.asl20;
      };

  haskellPackages = if compiler == "default"
                    then pkgs.haskellPackages
                    else pkgs.haskell.packages.${compiler};
  drv = haskellPackages.callPackage f {};

in
  if pkgs.lib.inNixShell then drv.env else drv
