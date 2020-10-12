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
      , ansi-terminal, base, blaze-html, blaze-markup
      , cmark, containers, directory, edit-distance, filepath
      , haskeline, HUnit, mtl, optparse-applicative, parsec, process, split, text
      , darwin, glfw3, SDL2, SDL2_image, SDL2_gfx, SDL2_mixer, SDL2_ttf
      , ghc-prof-flamegraph
      , clang , makeWrapper

      , libXext, libXcursor, libXinerama, libXi, libXrandr, libXScrnSaver, libXxf86vm, libpthreadstubs, libXdmcp, libGL
      }:
      mkDerivation {
        pname = "CarpHask";
        version = "0.3.0.0";
        src = ./.;
        inherit doCheck doBenchmark;
        isLibrary = false;
        isExecutable = true;
        enableSharedLibraries = false;
        enableSharedExecutables = false;
        enableLibraryProfiling = profiling;
        enableExecutableProfiling = profiling;
        libraryHaskellDepends = [
          ansi-terminal base blaze-html blaze-markup cmark containers
          directory edit-distance filepath haskeline mtl parsec process split
          text
        ] ++ optionals profiling [ ghc-prof-flamegraph ];
        pkgconfigDepends =
          [ glfw3 SDL2 SDL2_image SDL2_gfx SDL2_mixer SDL2_ttf ]
          ++ linuxOnly [ libXext libXcursor libXinerama libXi libXrandr libXScrnSaver libXxf86vm libpthreadstubs libXdmcp libGL];
        executableHaskellDepends = [
          base containers directory haskeline optparse-applicative parsec process
          clang
        ];
        executableFrameworkDepends = with darwin.apple_sdk.frameworks; optionals stdenv.isDarwin [
          Carbon Cocoa IOKit CoreFoundation CoreVideo IOKit ForceFeedback
        ];
        buildDepends = [ makeWrapper ];
        postInstall = ''
            wrapProgram $out/bin/carp --set CARP_DIR $src --prefix PATH : ${clang}/bin
            wrapProgram $out/bin/carp-header-parse --set CARP_DIR $src --prefix PATH : ${clang}/bin
        '';
        testHaskellDepends = [ base containers HUnit ];
        testTarget = "CarpHask-test";
        postCheck = ''
          env CARP=dist/build/carp/carp ./run_carp_tests.sh
        '';
        enableParallelBuilding = true;
        homepage = "https://github.com/eriksvedang/Carp";
        license = stdenv.lib.licenses.asl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

in haskellPackages.callPackage f {}
