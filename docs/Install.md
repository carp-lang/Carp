# Installation

<i>Sorry about the mess in this document, it will be cleaned up and re-organized when there is a good and easy way to build the whole project.</i>

Clone this repo, then use cmake to generate the project files that you desire. Either run it in the root of the Carp project, or from a sub directory that you create called 'build' or similar. Then build the project and make sure the resulting executable is put into the 'bin' directory (cmake should arrange that automatically). An example of how to do all of this for Xcode is in the file 'xcode.sh' in the root of the project.

```cmake .```

Make sure you run the compiler (i.e. ```make``` if you generated a Makefile). Now there should be a ```Carp/bin/carp-repl``` executable.

Add the 'bin' directory to your path to enable calling the ```carp``` command. To do this, add the following to your .bashrc / .zshrc / whatever:

```export PATH=$PATH:~/Carp/bin/```

Carp is developed on OSX 10.10 but Linux works too. More platforms are coming soon, Windows is being worked on. There are a few dependencies that have to be installed:
 * pkg-config
 * libffi
 * glfw3
 * rlwrap
 
Tip: Build libffi with ```./configure --enable-static --disable-shared``` to avoid generating a dylib that might interfere with other installations of it (like one installed with brew).

You will have to tell cmake the location of 'libffi' before it can build correctly. Try using their GUI application if you have trouble, it's pretty self explanatory (first press 'Configure', then set up the paths to 'libffi', and then press 'Generate').

To make cmake find libffi you might need to add it to the `PKG_CONFIG_PATH`:
```
export PKG_CONFIG_PATH=../libffi/<ARCHITECTURE>/
```

And to be able to run 'make' you might need to add libffi to your `C_INCLUDE_PATH`:

```
export C_INCLUDE_PATH=../libffi/<ARCHITECTURE>/include
```

or for some people:

```
cmake . -DLIBFFI_INCLUDE_PATH=/opt/local/lib/libffi-3.2.1/include -DLIBFFI_LIBRARY_PATH=/opt/local/lib/
```

Replace <ARCHITECTURE> with the name of the architecture you built libffi for.

Note: 'rlwrap' is not strictly needed but makes the REPL experience much nicer, modify the '/bin/carp' script if you don't want to use it.

## Mac OS X
If 'libffi' is installed with Brew, you can find the libraries at "/usr/local/Cellar/libffi/3.0.13/lib/" and the include files at "/usr/local/opt/libffi/lib/libffi-3.0.13/include".

With macports 'libffi' is at "/opt/local/lib/libffi-3.2.1/include" and "/opt/local/lib".

## Linux
On Linux Clang must be installed (GCC support will be added later).

## Windows
