### Installation

Clone this repo, then use cmake to generate the project files that you desire. A tip is to put the cmake output into a separate directory called 'build' or similar. Then build the project and make sure the resulting executable is put into the 'bin' directory (cmake should arrange that automatically). An example of how to do all of this for Xcode is in the file 'xcode.sh' in the root of the project.

Add the 'bin' directory to your path to enable calling the ```carp``` command. To do this, add the following to your .bashrc / .zshrc / whatever:

```export PATH=$PATH:~/Carp/bin/```

Carp is currently only tested on OSX 10.10. More platforms are coming soon. There are a few dependencies that have to be installed:
 * libffi
 * glfw3
 * rlwrap
 
Tip: Build libffi with ```./configure --enable-static --disable-shared``` to avoid generating a dylib that might interfere with other installations of it (like one installed with brew).

You will have to tell cmake the location of 'libffi' before it can build correctly. Try using their GUI application if you have trouble, it's pretty self explanatory (first press 'Configure', then set up the paths to 'libffi', and then press 'Generate').

If 'libffi' is installed with Brew, you can find the include files at "/usr/local/opt/libffi/lib/libffi-3.0.13/include".

Note: 'rlwrap' is not strictly needed but makes the REPL experience much nicer, modify the '/bin/carp' script if you don't want to use it.
