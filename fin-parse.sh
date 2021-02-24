cp -p out/2-structures-fin.conllu out/structures-fin.conllu

time cat out/structures-fin.conllu | udpipe --tokenizer none --tagger none --train structures-fin.udpipe

cat fi_tdt-ud-test.conllu | udpipe --parse structures-fin.udpipe >out/fin-structures-udpipe.conllu
gfud eval macro LAS fi_tdt-ud-test.conllu out/fin-structures-udpipe.conllu

