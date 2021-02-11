cd grammars
gf -make InferredEng.gf
echo "gr -probs=Inferred.probs -number=100 -cat=Utt | wf -file=\"../out/2-inferred-test.gft\"" | gf -run Inferred.pgf
echo "created out/2-inferred-test.gft"
cd ..
cat out/2-inferred-test.gft | gfud -gf2ud grammars/Inferred Eng Utt ud >out/2-inferred-test.conllu
echo "created out/2-inferred-test.conllu"
cat out/2-inferred-test.conllu | gfud conll2pdf
cat out/2-inferred-test.conllu | gfud statistics DEPREL

