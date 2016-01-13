CFLAGS=-I/usr/local/opt/libffi/lib/libffi-3.0.13/include
LDFLAGS=-L/usr/local/opt/libffi/lib/
LDLIBS=-lffi

all: src/main.o
	clang ./src/main.c -g -O0 -rdynamic -o ./bin/carp-repl -ldl $(CFLAGS) $(LDFLAGS) $(LDLIBS)

run:
	./bin/carp

clean: rm -f ./bin/*.o ast

