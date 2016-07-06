CFLAGS=-I/usr/local/opt/libffi/lib/libffi-3.0.13/include
LDFLAGS=-L/usr/local/opt/libffi/lib/
LDLIBS=-lffi
SOURCE_FILES=src/main.c src/obj.c src/gc.c src/obj_string.c src/reader.c src/eval.c src/env.c src/primops.c src/repl.c src/obj_array.c src/obj_conversions.c src/process.c src/bytecode.c src/match.c src/call_ffi.c src/unify.c

all: src/main.o
	clang $(SOURCE_FILES) -g -O2 -Wall -rdynamic -o ./bin/carp-repl -ldl $(CFLAGS) $(LDFLAGS) $(LDLIBS)

run:
	./bin/carp

clean:
	rm -f ./bin/*.o ast

format:
		find src/ -iname '*.[ch]' | xargs clang-format -style=file -i

