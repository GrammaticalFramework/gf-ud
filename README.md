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
```
$ cat en_ewt-ud-train.conllu | gfud pattern-replace 'FLATTEN (OR [DEPREL "nsubj", DEPREL "obj"])za 1'
# newdoc id = weblog-typepad.com_ripples_20040407125600_ENG_20040407_125600
# sent_id = weblog-typepad.com_ripples_20040407125600_ENG_20040407_125600-0001
# text = Elena's motorcycle tour through the region around Chernobyl has revived interest in one of the most serious nuclear disasters in history.
# newtext = Elena motorcycle tour region has revived interest one .
1       Elena   Elena   PROPN   NNP     Number=Sing     3       nmod:poss       ADJUSTED        SpaceAfter=No
2       motorcycle      motorcycle      NOUN    NN      Number=Sing     3       compound        ADJUSTED        _
3       tour    tour    NOUN    NN      Number=Sing     6       nsubj   ADJUSTED        _
4       region  region  NOUN    NN      Number=Sing     3       nmod    ADJUSTED        _
5       has     have    AUX     VBZ     Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin   6       aux     ADJUSTED
        _
6       revived revive  VERB    VBN     Tense=Past|VerbForm=Part        0       root    ADJUSTED        _
7       interest        interest        NOUN    NN      Number=Sing     6       obj     ADJUSTED        _
8       one     one     NUM     CD      NumType=Card    7       nmod    ADJUSTED        _
9       .       .       PUNCT   .       _       6       punct   ADJUSTED        _
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

See https://github.com/GrammaticalFramework/gf-ud/blob/master/doc/annotations.md 


