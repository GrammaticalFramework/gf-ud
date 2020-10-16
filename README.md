## gf-ud, software for dependency trees and interlingual syntax

This is a stand-alone program for working with depencency trees, either as such or in combination with GF (Grammatical Framework).
Features include:

- analysing treebanks for correctness and statistics
- visualizing trees
- computing accuracy (LAS and UAS)
- dependency parsing with DBNF (Dependency BNF) grammars
- converting GF trees to dependency trees and back.


The main target is UD (Universal Dependencies) trees, but the library 
but designed to be completely generic as for annotation scheme. 

An introduction to UD, its relation to GF, as well as DBNF, can be found in the manuscript

- A Ranta, Computational Grammar: An Interlingual Perspective,
  http://www.cse.chalmers.se/~aarne/grammarbook.pdf 


The GF conversions are based on algorithms presented in the papers

- P Kolachina and A Ranta, From Abstract Syntax to Universal Dependencies, 
  LiLT (Linguistic Issues in Language Technology) 13, 2016 ("gf2ud")
  http://csli-lilt.stanford.edu/ojs/index.php/LiLT/article/view/71/73
  (this link has been unreachable large parts of 2020)
  
- A Ranta and P Kolachina, From Universal Dependencies to Abstract Syntax,
  Proceedings of the NoDaLiDa 2017 Workshop on Universal Dependencies, 2017 ("ud2gf")
  https://www.aclweb.org/anthology/W17-0414.pdf

Both papers are available as parts of Prasanth Kolachina's PhD thesis

- Multilingual Abstractions: Abstract Syntax Trees and Universal
  Dependencies, 2019
  https://gupea.ub.gu.se/bitstream/2077/60331/1/gupea_2077_60331_1.pdf
  
as well as some earlier code in gf-contrib/ud2gf and in gf-core/src/, mostly by Aarne Ranta and Prasanth Kolachina.


## To get started

Compile the software. You need
- the Haskell compiler GHC, https://www.haskell.org/ghc/
- the PGF library from gf-core, http://www.grammaticalframework.org/


With these in place, you can simply type
```
$ make
```
You should make the executable gfud available on your path; otherwise you can call it with
```
$ ./gfud
```
In the same directory.

The executable gfud has several modes and options.
Calling it without any arguments prints out a help message.
gfud typically does not take file arguments, but reads and writes standard IO.


## Some examples of use

The below examples use small treebanks and grammars available in the grammars/ and data/ repositories.
More treebanks can be found in https://universaldependencies.org/ and also synthesized by using GF.


### Working with dependency treebanks

Check the integrity of a treebank (to make sure standard tools such as Malt parser don't fail):
```
$ gfud check-treebank <test.conllu
```
Compute statistics of features and their combinations (frequency list sorted in descending order)
```
$ gfud statistics POS DEPREL <test.conllu 
(["DET","det"],5)
(["ADV","advmod"],4)
(["NOUN","nsubj"],4)
(["VERB","root"],4)
```
Visualize a treebank by creating a LaTeX file or a pdf directly (requires pdflatex)
```
$ gfud conll2latex <test.conllu 
$ gfud conll2pdf <test.conllu 
```
Evaluate a treebank against a gold standard
```
$ gfud eval macro LAS data/en_pud-ud-test.conllu out/wn-udpipe-en_pud.conllu
evaluating macro LAS data/en_pud-ud-test.conllu out/wn-udpipe-en_pud.conllu
UDScore {udScore = 0.4673854319745826, udMatching = 1000, udTotalLength = 21183, udSamesLength = 9624, udPerfectMatch = 0}
```

### Parsing with DBNF

Parsing command-line text input:
```
$ echo "John loves Mary" | gfud dbnf grammars/English.dbnf Utt
# text = John loves Mary
# analyses = 2
# parsetree = (Utt (NP (N (PN John) (CN (N loves))) (PN Mary)))
1	John	_	PROPN	_	_	2	compound	_	_
2	loves	_	NOUN	_	_	0	root	_	_
3	Mary	_	PROPN	_	_	2	flat	_	_ç∂
```
Parsing POS tagged text, which can first be extracted from CoNLLU (ignoring the head and deprel fields):
```
$ cat test.conllu | gfud extract-pos-words 
the:<DET> cat:<NOUN>
the:<DET> black:<ADJ> cat:<NOUN>
every:<DET> cat:<NOUN> sees:<VERB> us:<PRON>
```
The result can then be piped to the parser itself:
```
$ cat test.conllu | gfud extract-pos-words | gfud dbnf grammars/English.dbnf -cut=10 -show=1
# text = the:<DET> cat:<NOUN>
# analyses = 0
# parsetree = (Chunks (Comp (NP (Det the) (CN (N cat)))))
1	the	_	DET	_	_	2	det	_	_
2	cat	_	NOUN	_	_	0	root	_	_
```
The flag -cut=10 sets a beam size (only consider the best 10 parse trees), whereas -show=1 says that only the 1 best tree is returned.


### Converting between GF and UD

Compile a GF grammar, for example:
```
  cd grammars
  ln -s gf-summerschool-2018  # a sister Git repository
  make mini
  cd ..
```
Convert GF to UD:
```
  cat test.gftrees | ./gfud -gf2ud grammars/MiniLang Eng Utt ud
  echo "the black cat sees us" | ./gfud -string2gf2ud grammars/MiniLang Eng Utt
```
Convert UD to GF:
```
  cat test.conllu | ./gfud -ud2gf grammars/MiniLang Eng Utt at
```

### Experiments with standard tools

Training a parser with a synthetic UD treebank:  https://github.com/GrammaticalFramework/gf-ud/blob/master/doc/synthetic.md


## Annotation syntax for gf2ud and ud2gf

See https://github.com/GrammaticalFramework/gf-ud/blob/master/doc/annotations.txt 


