all: compile

udonly:
	ghc -O3 -threaded -rtsopts --make -o udonly MainUDOnly.hs
	echo "created executable "
compile:
	ghc -O3 -threaded -rtsopts --make -o gfud MainUDGF.hs
	echo "created executable gfud"

parparse:
	ghc -O2 -threaded -rtsopts --make -o parparse ParParse.hs




