all: compile

compile:
	ghc -O3 -threaded -rtsopts --make -o gfud MainUDGF.hs
	echo "created executable gfud"

parparse:
	ghc -O2 -threaded -rtsopts --make -o parparse ParParse.hs




