cd grammars
gf -make InferredEng.gf
echo "gr -probs=Inferred.probs -number=500 -cat=Utt -depth=12 | wf -file=\"../out/1-inferred-test.gft\"" | gf -run Inferred.pgf
sort -u ../out/1-inferred-test.gft >../out/2-inferred-test.gft
echo "created out/2-inferred-test.gft"
cd ..
cat out/2-inferred-test.gft | gfud -gf2ud grammars/Inferred Eng Utt ud >out/2-inferred-test.conllu
echo "created out/2-inferred-test.conllu"
cat out/2-inferred-test.conllu | gfud conll2pdf
cat out/2-inferred-test.conllu | gfud statistics DEPREL
echo "POS not covered:"
gfud not-covered ud/UD_English-EWT/en_ewt-ud-test.conllu out/2-inferred-test.conllu POS
echo "DEPREL not covered:"
gfud not-covered ud/UD_English-EWT/en_ewt-ud-test.conllu out/2-inferred-test.conllu DEPREL
echo "DEPREL similarity:"
gfud cosine-similarity ud/UD_English-EWT/en_ewt-ud-test.conllu out/2-inferred-test.conllu DEPREL
echo "SUBTREETYPE similarity:"
gfud cosine-similarity ud/UD_English-EWT/en_ewt-ud-test.conllu out/2-inferred-test.conllu SUBTREETYPE

