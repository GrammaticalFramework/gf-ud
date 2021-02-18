cd grammars
gf -make StructuresEng.gf
echo "gr -probs=Structures.probs -number=3000 -cat=Top -depth=12 | wf -file=\"../out/1-structures-test.gft\"" | gf -run Structures.pgf
sort -u ../out/1-structures-test.gft >../out/2-structures-test.gft
echo "created out/2-structures-test.gft"
cd ..
cat out/2-structures-test.gft | gfud -gf2ud grammars/Structures Eng Utt ud >out/2-structures-test.conllu
echo "created out/2-structures-test.conllu"
cat out/2-structures-test.conllu | gfud conll2pdf
cat out/2-structures-test.conllu | gfud statistics DEPREL
echo "POS not covered:"
gfud not-covered ud/UD_English-EWT/en_ewt-ud-test.conllu out/2-structures-test.conllu POS
echo "DEPREL not covered:"
gfud not-covered ud/UD_English-EWT/en_ewt-ud-test.conllu out/2-structures-test.conllu DEPREL
echo "DEPREL similarity:"
gfud cosine-similarity ud/UD_English-EWT/en_ewt-ud-test.conllu out/2-structures-test.conllu DEPREL
echo "SUBTREETYPE similarity:"
gfud cosine-similarity ud/UD_English-EWT/en_ewt-ud-test.conllu out/2-structures-test.conllu SUBTREETYPE

