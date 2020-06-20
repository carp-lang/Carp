all: out/uselib

out/mylib.carpi out/mylib.o: mylib.carp
	carp --no-core --build-as-lib $^

out/uselib: uselib.carp out/mylib.carpi out/mylib.o
	carp --no-core -b $<

clean:
	rm -r out
