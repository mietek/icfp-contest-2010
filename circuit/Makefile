all: circuit

circuit: circuit.hs
	ghc --make -Wall -O2 -o circuit circuit.hs

clean:
	rm -f circuit circuit.hi circuit.o
