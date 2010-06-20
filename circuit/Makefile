all: circuit

circuit: circuit.hs Types.hs Parser.hs FactComp.hs 
	ghc --make  -O2 -o circuit Types.hs
	ghc --make  -O2 -o circuit Parser.hs
	ghc --make  -O2 -o circuit FactComp.hs
	ghc --make  -O2 -o circuit circuit.hs

clean:
	rm -f circuit circuit.hi circuit.o
