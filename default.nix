{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, ansi-terminal, base, blaze-html, blaze-markup
      , cmark, cmdargs, containers, directory, edit-distance, filepath
      , haskeline, HUnit, mtl, parsec, process, split, stdenv, text
      , darwin, glfw3, SDL2, SDL2_image, SDL2_gfx, SDL2_mixer, SDL2_ttf
      , clang , makeWrapper

      , libXext, libXcursor, libXinerama, libXi, libXrandr, libXScrnSaver, libXxf86vm
      }:
      mkDerivation {
        pname = "CarpHask";
        version = "0.3.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        enableSharedLibraries = false;
        enableSharedExecutables = false;
        enableLibraryProfiling = false;
        enableExecutableProfiling = false;
        libraryHaskellDepends = [
          ansi-terminal base blaze-html blaze-markup cmark containers
          directory edit-distance filepath haskeline mtl parsec process split
          text
        ];
        pkgconfigDepends =
          [ glfw3 SDL2 SDL2_image SDL2_gfx SDL2_mixer SDL2_ttf ]
          ++ stdenv.lib.optionals stdenv.isLinux [ libXext libXcursor libXinerama libXi libXrandr libXScrnSaver libXxf86vm ];
        executableHaskellDepends = [
          base cmdargs containers directory haskeline parsec process
          clang
        ];
        executableFrameworkDepends = with darwin.apple_sdk.frameworks; stdenv.lib.optionals stdenv.isDarwin [
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

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell
  then drv.env.overrideAttrs (o: {
    buildInputs = o.buildInputs ++ [ haskellPackages.cabal-install pkgs.clang ];
  })
  else drv
