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
The available features in this command and the next ones are the standard UD features `FORM LEMMA POS DEPREL FEATS`, as well as the combination feature `SUBTREETYPE`, which consists of the `POS+DEPREL` combinations of heads and their dependents.
The types are shown in a notation similar to GF abstract syntax types, where the argument with the `head` label is duplicated as the value, with the label found in the treebank data:
```
$ cat en_ewt-ud-train.conllu | gfud statistics SUBTREETYPE
(["DET(det) -> NOUN(head) -> NOUN(obj)"],1248)
(["ADP(case) -> DET(det) -> NOUN(head) -> NOUN(obl)"],1130)
(["DET(det) -> NOUN(head) -> NOUN(nsubj)"],956)
(["ADP(case) -> NOUN(head) -> NOUN(nmod)"],785)
```
Compare two treebanks with respect to feature combinations, by computing the cosine similarity of the two frequency lists:
```
$ gfud cosine-similarity en_ewt-ud-train.conllu wordnet-train.conllu DEPREL 
0.7071902033087594
```
Compare two treebanks with respect to coverage of features, returning those features in treebank 1 that do not occur in treebank 2:
```
$ gfud not-covered en_ewt-ud-train.conllu data/wordnet-train.conllu DEPREL 
["_"] ["acl:relcl"] ["aux:pass"] ["cc:preconj"] ["csubj"] ["csubj:pass"]
["det:predet"] ["dislocated"] ["flat:foreign"] ["goeswith"] ["list"] ["nmod:npmod"]
["nmod:tmod"] ["obl:npmod"] ["obl:tmod"] ["orphan"] ["parataxis"] ["reparandum"] ["vocative"]
```
Find subtrees that satisfy given patterns, written in a Haskell-like syntax.
A subtree consists of a head and its dependents.
Pattern matching goes recursively into each tree and finds all subtrees that match the pattern.
The complete pattern syntax is given in the `gfud` help message.
```
$ cat en_ewt-ud-train.conllu | gfud pattern-match 'AND [POS "ADV", DEPREL "xcomp"]'
# sent_id = answers-20111108111031AARG57j_ans-0006
# text = I will get a treat and try to get him in there that way, it wont work.
11	in	in	ADP	IN	_	12	case	12:case	_
12	there	there	ADV	RB	PronType=Dem	9	xcomp	9:xcomp	_

# sent_id = answers-20111108111031AARG57j_ans-0007
# text = He wont even get close to the crate.
6	close	close	ADV	JJ	Degree=Pos	5	xcomp	5:xcomp	_
7	to	to	ADP	IN	_	9	case	9:case	_
8	the	the	DET	DT	Definite=Def|PronType=Art	9	det	9:det	_
9	crate	crate	NOUN	NN	Number=Sing	6	obl	6:obl:to	SpaceAfter=No
``` 
Replace or delete subtrees that satisfy a certain pattern, or flatten trees below a given depth.
The complete syntax is given in the `gfud` help message.
The nodes of the resulting trees are renumbered so that they are still valid dependency trees.
The matching or replacement pattern can also be read from a file with the `-f` option, which is a good practice in particulat with complex replacement patterns collected under a `CHANGES` list.
Here is an example looking for main arguments of predication, from file `grammars/predicates.hst` (the suffix hst refers to "Haskell term"):
```
COMPOSE [
  FILTER_SUBTREES (DEPREL "root") (OR [
    DEPREL_ "nsubj", DEPREL "obj", DEPREL "obl", DEPREL "iobj",
    DEPREL "cop", DEPREL "aux:pass", 
    DEPREL "xcomp", DEPREL "ccomp"
    ]),
  CHANGES [
    FILTER_SUBTREES (OR [DEPREL "xcomp",DEPREL "ccomp"]) (DEPREL "mark"),
    FILTER_SUBTREES (DEPREL "obl") (DEPREL "case"),
    FLATTEN (OR [DEPREL_ "nsubj", DEPREL "obj", DEPREL"cop", DEPREL "iobj"]) 0
    ],
  CHANGES [
    REPLACE (DEPREL_ "nsubj") (AND [FORM "X", LEMMA "X"]),
    REPLACE (DEPREL "obj")  (AND [FORM "Y", LEMMA "Y"]),
    REPLACE (DEPREL "obl")  (AND [FORM "Z", LEMMA "Z"]),
    REPLACE (DEPREL "ccomp")  (AND [FORM "S", LEMMA "S"]),
    REPLACE (DEPREL "xcomp")  (AND [FORM "V", LEMMA "V"])
    ]
  ]
```
Here is a sample from an example run:
```
$ cat en_pud-ud-test.conllu | gfud pattern-replace -f grammars/predicates.hst
# sent_id = n01003013
# text = Maybe the dress code was too stuffy.
# newtext = X was stuffy
1       X       X       NOUN    NN      Number=Sing     3       nsubj   ADJUSTED        _
2       was     be      AUX     VBD     Mood=Ind|Number=Sing|Person=3|Tense=Past|VerbForm=Fin   3       cop     ADJUSTED
        _
3       stuffy  stuffy  ADJ     JJ      Degree=Pos      0       root    ADJUSTED        SpaceAfter=No

# newdoc id = n01004
# sent_id = n01004009
# text = Rather than teaching the scientific method as a separate unit, for example, students learn science content by applying it.
# newtext = for Z X learn Y
1       for     for     ADP     IN      _       2       case    ADJUSTED        _
2       Z       Z       NOUN    NN      Number=Sing     4       obl     ADJUSTED        SpaceAfter=No
3       X       X       NOUN    NNS     Number=Plur     4       nsubj   ADJUSTED        _
4       learn   learn   VERB    VBP     Mood=Ind|Tense=Pres|VerbForm=Fin        0       root    ADJUSTED        _
5       Y       Y       NOUN    NN      Number=Sing     4       obj     ADJUSTED        _
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
If the option `units` is present, the scores are shown sentence by sentence, starting from the lowest score, and differing lines marked with `|` (like in unix `diff -y`):
```
$ gfud eval micro LAS en_pud-ud-test.conllu out/wn-udpipe-en_pud.conllu units
UDScore {udScore = 0.7777777777777778, udMatching = 1, udTotalLength = 9, udSamesLength = 7, udPerfectMatch = 0}
1  The  the  DET  DT  Definite=Def|PronType=Art  2  det       1  The  the  DET  DT  Definite=Def|PronType=Art  2  det
2  consumer  consumer  NOUN  NN  Number=Sing  4  nsubj        2  consumer  consumer  NOUN  NN  Number=Sing  4  nsubj
3  can  can  AUX  MD  VerbForm=Fin  4  aux                    3  can  can  AUX  MD  VerbForm=Fin  4  aux
4  boost  boost  VERB  VB  VerbForm=Inf  0  root              4  boost  boost  VERB  VB  VerbForm=Inf  0  root
5  the  the  DET  DT  Definite=Def|PronType=Art  6  det       5  the  the  DET  DT  Definite=Def|PronType=Art  6  det
6  demand  demand  NOUN  NN  Number=Sing  4  obj              6  demand  demand  NOUN  NN  Number=Sing  4  obj
7  for  for  ADP  IN  _  8  case                              7  for  for  ADP  IN  _  8  case
8  change  change  NOUN  NN  Number=Sing  6  nmod        |    8  change  change  NOUN  NN  Number=Sing  4  obj
9  .  .  PUNCT  .  _  4  punct                           |    9  .  .  PUNCT  .  _  4  mark

```


### Parsing with DBNF

Parsing command-line text input:
```
$ echo "John loves Mary" | gfud dbnf grammars/English.dbnf S

# text = John loves Mary
# analyses = 1
# parsetree = (S (NP (PN John)) (VP (V2 loves) (NP (PN Mary))))
1	John	_	PROPN	_	_	2	nsubj	_	_
2	loves	_	VERB	_	_	0	root	_	_
3	Mary	_	PROPN	_	_	2	obj	_	_
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
  cat test.gftrees | ./gfud gf2ud grammars/MiniLang Eng Utt ud
  echo "the black cat sees us" | ./gfud -string2gf2ud grammars/MiniLang Eng Utt
```
Convert UD to GF:
```
  cat test.conllu | ./gfud ud2gf grammars/MiniLang Eng Utt at
```

### Experiments with standard tools

Training a parser with a synthetic UD treebank:  https://github.com/GrammaticalFramework/gf-ud/blob/master/doc/synthetic.md


## Annotation syntax for gf2ud and ud2gf

See https://github.com/GrammaticalFramework/gf-ud/blob/master/doc/annotations.md 


