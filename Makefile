compile:
	ghc -O3 --make -o gfud MainUDGF.hs

# this presupposes setting the scene as described in doc/training-and-testing.txt
minitest:
	source minitest.sh


