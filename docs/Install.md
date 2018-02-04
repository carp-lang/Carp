# Installation

Carp is mainly developed on macOS, but it also works fine on Linux. Windows is currently not supported - but please get in touch in case you want to help out with that!

1. Make sure you have [Stack](https://docs.haskellstack.org/en/stable/README/) installed
2. Clone this repo
3. Run ```stack build``` in the root
4. ```stack install``` will install the Carp command line tool for easy access on your system
5. Make sure that the directory where stack installs executables is on your PATH, i.e: ```export PATH=~/.local/bin:$PATH```
6. Set the environment variable CARP_DIR so that it points to the root of the Carp repo. ```export CARP_DIR=~/Carp/```
