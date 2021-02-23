# Pattern matching CoNLL trees with gfud


`pattern-match`: Find subtrees that satisfy given patterns, written in a Haskell-like syntax.
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
`pattern-replace`: Replace or delete subtrees that satisfy a certain pattern, or flatten trees below a given depth.
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
    PRUNE (OR [DEPREL_ "nsubj", DEPREL "obj", DEPREL"cop", DEPREL "iobj"]) 0
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
