cp -p out/2-structures-spa.conllu out/structures-spa.conllu

time cat out/structures-spa.conllu | udpipe --tokenizer none --tagger none --train structures-spa.udpipe

cat es_gsd-ud-test.conllu | udpipe --parse structures-spa.udpipe >out/spa-structures-udpipe.conllu
gfud eval macro LAS es_gsd-ud-test.conllu out/spa-structures-udpipe.conllu

