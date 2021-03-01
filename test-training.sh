cp -p out/2-structures-test.conllu out/structures.conllu

time cat out/structures.conllu | udpipe --tokenizer none --tagger none --train structures.udpipe
cat en_ewt-ud-test.conllu | udpipe --parse structures.udpipe >out/ewt-structures-udpipe.conllu
gfud eval macro LAS en_ewt-ud-test.conllu out/ewt-structures-udpipe.conllu
cat en_pud-ud-test.conllu | udpipe --parse structures.udpipe >out/pud-structures-udpipe.conllu
gfud eval macro LAS en_pud-ud-test.conllu out/pud-structures-udpipe.conllu

