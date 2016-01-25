CFLAGS=-I/usr/local/include
LDFLAGS=-L/usr/local/lib/
LDLIBS=-lglfw3 -framework OpenGL -framework Cocoa -framework IOKit

all: gl_constants.o
	clang gl_constants.c -shared -g -o gl_constants.so $(CFLAGS) $(LDFLAGS) $(LDLIBS)

clean: rm -f *.o main
