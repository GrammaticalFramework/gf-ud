cd grammars
gf -make StructuresSpa.gf
echo "gr -probs=Structures.probs -number=2000 -cat=Top -depth=12 | wf -file=\"../out/1-structures-spa.gft\"" | gf -run Structures.pgf
sort -u ../out/1-structures-spa.gft >../out/2-structures-spa.gft
echo "created out/2-structures-spa.gft"
cd ..
cat out/2-structures-spa.gft | gfud -gf2ud grammars/Structures Spa Top ud >out/1-structures-spa.conllu
echo "created out/1-structures-spa.conllu"
gfud cosine-similarity-sort es_gsd-ud-train.conllu out/1-structures-spa.conllu -threshold 32 SUBTREETYPE >out/2-structures-spa.conllu
##mv out/1-structures-spa.conllu out/2-structures-spa.conllu
echo "created out/2-structures-spa.conllu"
cat out/2-structures-spa.conllu | gfud conll2pdf
cat out/2-structures-spa.conllu | gfud statistics DEPREL
echo "POS not covered:"
gfud not-covered es_gsd-ud-test.conllu out/2-structures-spa.conllu POS
echo "DEPREL not covered:"
gfud not-covered es_gsd-ud-test.conllu out/2-structures-spa.conllu DEPREL
echo "DEPREL similarity:"
gfud cosine-similarity es_gsd-ud-test.conllu out/2-structures-spa.conllu DEPREL
echo "SUBTREETYPE similarity:"
gfud cosine-similarity es_gsd-ud-test.conllu out/2-structures-spa.conllu SUBTREETYPE

