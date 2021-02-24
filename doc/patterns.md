# Pattern matching CoNLL trees with gfud


## Overview

As described in the main document of gfud (its README.md), there are two functionalities using patterns on UD trees for analysing and changing them.

`pattern-match`: Find subtrees that satisfy given patterns, written in a Haskell-like syntax.
A subtree consists of a head and its dependents.
Pattern matching goes recursively into each tree and finds all subtrees that match the pattern.
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
The nodes of the resulting trees are renumbered so that they are still valid dependency trees.
The matching or replacement pattern can also be read from a file with the `-f` option, which is a good practice in particulat with complex replacement patterns collected under a `CHANGES` or `COMPOSE` list.
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

## The syntax of patterns and replacements

Here is an overview of the pattern syntax:
```
Analysis patterns, <pattern>:
   (FORM | LEMMA | POS | DEPREL | DEPREL_) <string> 
 | (FEATS | FEATS_) <features>
 | (AND | OR) [ <pattern>,* ]
 | ARG <pos> <deprel>
 | (SEQUENCE | SEQUENCE_) [ <pattern>,* ]
 | NOT <pattern>
 | (TREE | TREE_) <pattern> <pattern>*
 | (DEPTH | DEPTH_UNDER | DEPTH_OVER) <int>
 | (LENGTH | LENGTH_UNDER | LENGTH_OVER) <int>
 | TRUE
 | PROJECTIVE

Replacement patterns, <replacement>:
   (REPLACE_FORM | REPLACE_LEMMA | REPLACE_POS | REPLACE_DEPREL | REPLACE_DEPREL_) <string>
 | (REPLACE_FEATS | REPLACE_FEATS_) <features>
 | IF <pattern> <replacement>
 | UNDER <pattern> <replacement>
 | OVER <pattern> <replacement>
 | PRUNE <pattern> <int>
 | FILTER_SUBTREES <pattern> <pattern>
 | FLATTEN <pattern>
 | RETARGET <pattern> <pattern> <pattern>
 | CHANGES [ <replacement>,* ]
 | COMPOSE [ <replacement>,* ]
```
`<string>` arguments are strings in double quotes.
The entire `<pattern>` requires single quotes if read from command line, but not if read from a file (with the `-f` option).


## The semantics of pattern matching

Pattern matching operates of *trees* and their *subtrees*, which are obtained from the CoNLL descriptions in the following way:
- A subtree consists of a head and its dependents.

The nodes of a tree keep all the information that is present in the CoNLL tree, just in an explicit tree format.
This format can be inspected with the command `gfud conll2tree`, which, for instance, from
```
1	every	every	DET	Det	FORM=0	2	det	_	FUN=every_Det
2	cat	cat	NOUN	N	Number=Sing	3	nsubj	_	FUN=cat_N
3	sees	see	VERB	V2	Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin	0	root	_	FUN=see_V2
4	us	we	PRON	Pron	Case=Acc|PronType=Prs	3	obj	_	FUN=we_Pron
```
produces
```
3	sees	see	VERB	V2	Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin	0	root	_	FUN=see_V2
    2	cat	cat	NOUN	N	Number=Sing	3	nsubj	_	FUN=cat_N
        1	every	every	DET	Det	FORM=0	2	det	_	FUN=every_Det
    4	us	we	PRON	Pron	Case=Acc|PronType=Prs	3	obj	_	FUN=we_Pron
```
The command `gfud pattern-match <pattern>` goes through all trees in its input and, recursively, all their subtrees.
If it finds a match in a tree, it prints out
- the tree metadata (comments prefixed with '#')
- the subtrees that match

The simplest patterns are those that match individual words (i.e. lines in the CoNLL representation).
Thus, by reference to the example above,
- `FORM "sees"` matches the word on line 3 and thereby the entire tree
- `LEMMA "see"` matches the same tree
- `POS "NOUN"` matches line 2 and thereby the subtree containing *every cat*
- `DEPREL "obj"` matches line 4 and thereby the on-word tree *us*
- `FEATS Case=Acc|PronType=Prs` matches line 4, *us*

The underscore versions are more liberal, as it is enough to match a part:
- `FEATS_ Number=Sing` matches both 3 (the whole of the sentence) and 2 (*every cat*)
- `DEPREL_ nsubj` would also match `nsubj:pass` and any other label whose prefix (before the colon `:`) is `nsubj`

Patterns can be combined with logical operators:
- `AND [LEMMA "see", POS "VERB"]` matches any line with both these characteristics
- `OR [POS "ADJ", POS "VERB", POS "NOUN"]` matches any line with at least one of these characteristics
- `NOT (POS "VERB")` matches lines whose POS tag is not `VERB`
- `TRUE` matches all trees; it is useful as a part of some complex patterns
- `ARG "NOUN" "nsubj"` is a shorthand for `AND [POS "NOUN", DEPREL "nsubj"]`, a commonly used pattern type, since it corresponds to the  argument and value types used in `SUBTREETYPE`

All of the patterns above are tested by just matching with the heads of subtrees.
The following patterns assume a more global view:
- `DEPTH 2` matches trees of depth exactly 2 (head, children, grandchildren), where `DEPTH 0` matches trees without children
- `DEPTH_UNDER 2` matches trees with depth less than 2
- `DEPTH_OVER 2` matches trees with depth more than 2
- `LENGTH 2` matches trees that cover exactly 2 words
- `LENGTH_UNDER 2` matches trees with length less than 2
- `LENGTH_OVER 2` matches trees with length more than 2
- `SEQUENCE [POS "DET", POS "NOUN"]` matches the smallest subtrees where a `DET` and a `NOUN` occur in this order, with no intervening words
- `SEQUENCE_` is similar to `SEQUENCE`, except that the sequence does not need to be contiguous
- `TREE (POS "NOUN") [POS "VERB", POS "ADJ"]` matches trees whose root is a `NOUN` and children are a `VERB` and and `ADJ`
- `TREE_` is similar to `TREE`, except that the arguments need not be contiguous
- `PROJECTIVE` matches trees that are **projective**, i.e. whose position numbers, when sorted, form a contiguous sequence

Notice how simple the definition of projectivity is when dependency trees are seen as trees.
This simplicity of course assumes that the position numbers are always given in the strict order of words without holes or duplications.
Non-projectivity, which is probably a more interesting property, is expressed as `NOT PROJECTIVE`.
The call
```
$ cat en_ewt-ud-train.conllu | gfud pattern-match 'NOT PROJECTIVE' | grep sent_id | wc -l
93
```
finds all non-projective subtrees of the treebank processed and returns the count of those trees that have non-projective subtrees.


## The semantics of replacements

Just line `pattern-match`, `pattern-replace` traverses all trees in its input and performs the specified changes in all subtrees.
The simplest replacement is to change a feature of a single word:
- `REPLACE_FORM "color" "colour"` changes the word form `color` to `colour`
- `REPLACE_LEMMA "color" "colour"` changes the lemma `color` to `colour`
- `REPLACE_POS "PROPN" "NOUN"` changes the POS tag `PROPN` to `NOUN`
- `REPLACE_DEPREL "dobj" "obj"` changes the label `dobj` to `obj`
- `REPLACE_DEPREL_ "nsubj" "nsubj"` changes all labels of form `nsubj:...` to `nsubj`
- `REPLACE_FEATS "NUMBER=Sing" "NUMBER=Plur"` changes the `NUMBER` feature, if it appears alone
- `REPLACE_FEATS_ "NUMBER=Sing" "NUMBER=Plur"` changes the `NUMBER` feature, wherever it appears among the features

These simple replacements are applied everywhere in trees.
This behaviour can be restricted by conditions:
- `IF (FORM "United") (REPLACE_POS "PROPN" "ADJ")` changes the POS tag of the word `United`
- `UNDER (POS "VERB") (REPLACE_DEPREL "nmod" "obl")` changes `nmod` to `obl`, if immediately dominated by a `VERB`
- `OVER (DEPREL "case") (REPLACE_DEPREL "obj" "obl")` changes `obj` to `obl`, if any of the immediate subtrees has the label `case`

The following replacements apply globally to trees:
- `FILTER_SUBTREES (POS "NOUN") (OR [DEPREL "det",DEPREL "amod"])` removes all other subtrees of a `NOUN` than those labelled `det` or `amod`
- `FLATTEN (DEPREL "root")` makes all grandchildren to have the `root` as their head, thus decreasing the depth of the tree by 1
- `PRUNE (POS "NOUN") 1` drops out all subsubtrees of nouns, thus decreasing the depth of `NOUN` trees to 1
- `RETARGET (DEPREL "root") (DEPREL "case") (DEPREL "obl")` changes the head of a `case` subtree to be the `obl` subtree, if under `root`

It is often useful to perform several changes in parallel or in sequence:
- `CHANGES [X,Y,Z]` looks for possibilities to perform each of these changes, and applies the first one that matches, in the given order
- `COMPOSE [X,Y,Z]` performs the change `X` first, and then the remaining ones on the result, in the given order



