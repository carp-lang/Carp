all: out/uselib

out/sublib.carpi out/sublib.o: sublib.carp
	carp --static-core --build-as-lib $^

out/mylib.carpi out/mylib.o: mylib.carp out/sublib.carpi
	carp --static-core --build-as-lib $<

out/uselib: uselib.carp out/mylib.carpi
	carp --static-core -b $<

clean:
	rm -r out
