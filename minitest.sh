#!/bin/bash

echo "generating GF trees from $1 $2 $3"
echo "==================="
echo "gr -cat=$3 -number=400 -depth=8 | wf -file=out/$1.gft" | gf -run grammars/$1.pgf
head -700 out/$1.gft | sort -u >out/$1-train.gft
tail -99  out/$1.gft | sort -u >out/$1-test.gft
echo "==================="
echo "running gf2ud, generating conllu files"
echo "==================="
cat out/$1-train.gft | ./gfud -gf2ud grammars/$1 $2 $3 ud | sed '/./,$!d' >out/$1-train.conllu
cat out/$1-test.gft  | ./gfud -gf2ud grammars/$1 $2 $3 ud | sed '/./,$!d' >out/$1-test.conllu

if [ $4 = ud2gf ]
then
  echo "==================="
  echo "running ud2gf for comparison"
  cat out/$1-test.conllu | ./gfud -ud2gf grammars/$1 Eng Utt at >out/$1-test-ud2gf.gft
  cat out/$1-test.conllu | ./gfud -ud2gf grammars/$1 Eng Utt stat
  diff -y out/$1-test.gft out/$1-test-ud2gf.gft
fi

if [ $5 = malt ]
then
  echo "==================="
  echo "training and testing Malt parser"
  echo "==================="
  java -jar maltparser.jar -c test -i out/$1-train.conllu -m learn
  java -jar maltparser.jar -c test -i out/$1-test.conllu -o out/$1-out.conllu -m parse
fi
