cat out/2-structures-fin.gft | gfud -gf2ud grammars/Structures Fin Top ud >out/1-structures-fin.conllu
echo "created out/1-structures-fin.conllu"
gfud cosine-similarity-sort fi_tdt-ud-train.conllu out/1-structures-fin.conllu -threshold 32 SUBTREETYPE >out/2-structures-fin.conllu
##mv out/1-structures-fin.conllu out/2-structures-fin.conllu
echo "created out/2-structures-fin.conllu"
cat out/2-structures-fin.conllu | gfud conll2pdf
cat out/2-structures-fin.conllu | gfud statistics DEPREL
echo "POS not covered:"
gfud not-covered fi_tdt-ud-test.conllu out/2-structures-fin.conllu POS
echo "DEPREL not covered:"
gfud not-covered fi_tdt-ud-test.conllu out/2-structures-fin.conllu DEPREL
echo "DEPREL similarity:"
gfud cosine-similarity fi_tdt-ud-test.conllu out/2-structures-fin.conllu DEPREL
echo "SUBTREETYPE similarity:"
gfud cosine-similarity fi_tdt-ud-test.conllu out/2-structures-fin.conllu SUBTREETYPE

