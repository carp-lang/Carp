{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
, doBenchmark ? false
, profiling ? false
}:

let

  inherit (nixpkgs) pkgs;

  optionals = nixpkgs.stdenv.lib.optionals;
  linuxOnly = optionals nixpkgs.stdenv.isLinux;

  f = { mkDerivation, ansi-terminal, base, blaze-html, blaze-markup
      , cmark, cmdargs, containers, directory, edit-distance, filepath
      , haskeline, HUnit, mtl, parsec, process, split, stdenv, text
      , darwin, glfw3, SDL2, SDL2_image, SDL2_gfx, SDL2_mixer, SDL2_ttf
      , ghc-prof-flamegraph
      , clang , makeWrapper

      , libXext, libXcursor, libXinerama, libXi, libXrandr, libXScrnSaver, libXxf86vm, libpthreadstubs, libXdmcp, libGL
      }:
      mkDerivation {
        pname = "CarpHask";
        version = "0.3.0.0";
        src = ./.;
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
          base cmdargs containers directory haskeline parsec process
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

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell
  then drv.env.overrideAttrs (o: {
    buildInputs = with pkgs; o.buildInputs ++ [ haskellPackages.cabal-install clang gdb ]
                  ++ linuxOnly [ flamegraph linuxPackages.perf ];
  })
  else drv
