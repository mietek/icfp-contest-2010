all: circuit

circuit: circuit.hs Types.hs Parser.hs FactComp.hs 
	ghc --make  -O2 Types.hs
	ghc --make  -O2 Parser.hs
	ghc --make  -O2 -o factcomp FactComp.hs
	ghc --make  -O2 -o circuit circuit.hs

clean:
	rm -f circuit circuit.hi circuit.o
