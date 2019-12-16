echo "generating GF trees"
echo "==================="
echo "gr -cat=Utt -number=400 -depth=8 | wf -file=out/minilang.gft" | gf -run grammars/MiniLang.pgf
head -700 out/minilang.gft | sort -u >out/minilang-train.gft
tail -99  out/minilang.gft | sort -u >out/minilang-test.gft
echo "==================="
echo "running gf2ud, generating conllu files"
echo "==================="
cat out/minilang-train.gft | ./gfud -gf2ud grammars/MiniLang Eng Utt ud | sed '/./,$!d' >out/minilang-train.conllu
cat out/minilang-test.gft  | ./gfud -gf2ud grammars/MiniLang Eng Utt ud | sed '/./,$!d' >out/minilang-test.conllu
#echo "==================="
#echo "running ud2gf for comparison"
#cat out/minilang-test.conllu | ./gfud -ud2gf grammars/MiniLang Eng Utt at >out/minilang-test-ud2gf.gft
#cat out/minilang-test.conllu | ./gfud -ud2gf grammars/MiniLang Eng Utt stat
#diff -y out/minilang-test.gft out/minilang-test-ud2gf.gft
echo "==================="
echo "training and testing Malt parser"
echo "==================="
java -jar maltparser.jar -c test -i out/minilang-train.conllu -m learn
java -jar maltparser.jar -c test -i out/minilang-test.conllu -o minilang-out.conllu -m parse
