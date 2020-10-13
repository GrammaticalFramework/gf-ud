# Training and testing UD parsing with Maltparser


## Short version, with .conllu files in place:

Train maltparser with the training treebank
```
  java -jar maltparser.jar -c wordnet-parser -i data/wordnet-train.conllu -m learn
```
You should see this output, not just the beginning of it:
```
-----------------------------------------------------------------------------
                          MaltParser 1.9.2                             
-----------------------------------------------------------------------------
         MALT (Models and Algorithms for Language Technology) Group          
             Vaxjo University and Uppsala University                         
                             Sweden                                          
-----------------------------------------------------------------------------

Started: Tue Oct 13 16:33:57 CEST 2020
  Transition system    : Arc-Eager
  Parser configuration : Nivre with allow_root=true, allow_reduce=false and enforce_tree=false
  Oracle               : Arc-Eager
  Data Format          : file:////Users/aarne/GF/gf-ud/wordnet-parser/conllx.xml
.          	      1	      0s	      3MB
.          	     10	      0s	      4MB
.          	    100	      0s	      8MB
..........	   1000	      0s	     21MB
..........	   2000	      0s	     15MB
..........	   3000	      0s	     23MB
..........	   4000	      0s	     13MB
..........	   5000	      0s	     24MB
..........	   6000	      1s	     33MB
......     	   6500	      1s	     51MB
Creating Liblinear model odm0.liblinear.moo
- Read all training instances.
- Train a parser model using LibLinear.
- Optimize the memory usage
- Save the Liblinear model odm0.liblinear.moo
Learning time: 00:00:03 (3816 ms)
Finished: Tue Oct 13 16:34:01 CEST 2020
```

Test parsing with maltparser:
```
  java -jar maltparser.jar -c wordnet-parser -i data/wordnet-test.conllu -o out/wordnet-test-out.conllu -m parse
```
You should see this kind of output:
```
-----------------------------------------------------------------------------
                          MaltParser 1.9.2                             
-----------------------------------------------------------------------------
         MALT (Models and Algorithms for Language Technology) Group          
             Vaxjo University and Uppsala University                         
                             Sweden                                          
-----------------------------------------------------------------------------

Started: Tue Oct 13 16:34:41 CEST 2020
  Transition system    : Arc-Eager
  Parser configuration : Nivre with allow_root=true, allow_reduce=false and enforce_tree=false
  Data Format          : /test/conllx.xml
.          	      1	      0s	      6MB
.          	     10	      0s	      6MB
.          	    100	      0s	      9MB
.....      	    506	      0s	     15MB
Parsing time: 00:00:00 (290 ms)
Finished: Tue Oct 13 16:34:42 CEST 2020
```
Evaluate the parser
```
  gfud eval macro LAS data/wordnet-test.conllu out/wordnet-test-out.conllu
```
You should see this kind of output:
```
evaluating macro LAS data/wordnet-test.conllu out/wordnet-test-out.conllu
UDScore {udScore = 0.6565243304126713, udMatching = 506, udTotalLength = 2532, udSamesLength = 1373, udPerfectMatch = 183}
```


## Longer version, starting with building treebanks with gf2ud

Example: use the gf-wordnet grammar.

Clone gf-wordnet from
```
  https://github.com/GrammaticalFramework/gf-wordnet
```
Build a pgf grammar:
```
  gf-wordnet$ gf -make ParseEng.gf
```
Make this grammar available in ud-gf:
```
  gf-ud/grammars$ ln -s <path-to-gf-wordnet>/Parse.pgf
```
Split wordnet example treebank into training and test sets:
```
  head -6500 data/wordnet-examples.gft >data/wordnet-train.gft 
  tail -506 data/wordnet-examples.gft >data/wordnet-test.gft 
```
Convert these GF treebanks to UD treebanks
```
  cat data/wordnet-train.gft | ./gfud -gf2ud grammars/Parse Eng Phr ud | grep -v "##" | sed '/./,$!d' >out/wordnet-train.conllu
  cat data/wordnet-test.gft | ./gfud -gf2ud grammars/Parse Eng Phr ud | grep -v "##" | sed '/./,$!d' >out/wordnet-test.conllu
```

Now you have the .conllu files to start from in the "short version".
But notice that they are in out/ not in data/ since they are generated files.


