all: out/uselib

out/sublib.carpi out/sublib.o: sublib.carp
	carp --no-core --build-as-lib $^

out/mylib.carpi out/mylib.o: mylib.carp out/sublib.carpi
	carp --no-core --build-as-lib $<

out/uselib: uselib.carp out/mylib.carpi
	carp --no-core -b $<

clean:
	rm -r out
