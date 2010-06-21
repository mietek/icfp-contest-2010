all: circuit

circuit: Circuit.hs
	ghc --make -O2 -o circuit Circuit.hs

clean:
	rm -f circuit Circuit.hi Circuit.o Filter.hi Filter.o

extend: ExtendCar.hs
	ghc --make -O2 -o extend ExtendCar.hs
