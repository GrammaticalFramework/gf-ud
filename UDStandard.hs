module Gfud.UDStandard where

------------------------------
-- for checking with UD2 standard
------------------------------

allUDPos :: [(String,String)]  -- tag, explanation
allUDPos = [
  ("ADJ", "adjective"),
  ("ADP", "adposition"),
  ("ADV", "adverb"),
  ("AUX", "auxiliary"),
  ("CCONJ", "coordinating conjunction"),
  ("DET", "determiner"),
  ("INTJ", "interjection"),
  ("NOUN", "noun"),
  ("NUM", "numeral"),
  ("PART", "particle"),
  ("PRON", "pronoun"),
  ("PROPN", "proper noun"),
  ("PUNCT", "punctuation"),
  ("SCONJ", "subordinating conjunction"),
  ("SYM", "symbol"),
  ("VERB", "verb"),
  ("X", "other")
  ]

allUDLabels :: [(String,String)]  -- dependency label, explanation
allUDLabels = [
  ("acl", "clausal modifier of noun (adjectival clause)"),
  ("advcl", "adverbial clause modifier"),
  ("advmod", "adverbial modifier"),
  ("amod", "adjectival modifier"),
  ("appos", "appositional modifier"),
  ("aux", "auxiliary"),
  ("case", "case marking"),
  ("cc", "coordinating conjunction"),
  ("ccomp", "clausal complement"),
  ("clf", "classifier"),
  ("compound", "compound"),
  ("conj", "conjunct"),
  ("cop", "copula"),
  ("csubj", "clausal subject"),
  ("dep", "unspecified dependency"),
  ("det", "determiner"),
  ("discourse", "discourse element"),
  ("dislocated", "dislocated elements"),
  ("expl", "expletive"),
  ("fixed", "fixed multiword expression"),
  ("flat", "flat multiword expression"),
  ("goeswith", "goes with"),
  ("iobj", "indirect object"),
  ("list", "list"),
  ("mark", "marker"),
  ("nmod", "nominal modifier"),
  ("nsubj", "nominal subject"),
  ("nummod", "numeric modifier"),
  ("obj", "object"),
  ("obl", "oblique nominal"),
  ("orphan", "orphan"),
  ("parataxis", "parataxis"),
  ("punct", "punctuation"),
  ("reparandum", "overridden disfluency"),
  ("root", "root"),
  ("vocative", "vocative"),
  ("xcomp", "open clausal complement")
  ]




{-
-- morphological features in en-pud: 34

Case=Acc
Case=Nom
Definite=Def
Definite=Ind
Degree=Cmp
Degree=Pos
Degree=Sup
Foreign=Yes
Gender=Fem
Gender=Masc
Gender=Neut
Mood=Ind
NumType=Card
NumType=Mult
NumType=Ord
Number=Plur
Number=Sing
Person=1
Person=2
Person=3
Polarity=Neg
Poss=Yes
PronType=Art
PronType=Dem
PronType=Int
PronType=Prs
PronType=Rel
Reflex=Yes
Tense=Past
Tense=Pres
VerbForm=Fin
VerbForm=Ger
VerbForm=Inf
VerbForm=Part

-- sv-pud 30
Abbr=Yes
Case=Acc
Case=Gen
Case=Nom
Definite=Def
Definite=Ind
Degree=Cmp
Degree=Pos
Degree=Sup
Foreign=Yes
Gender=Com
Gender=Masc
Gender=Neut
Mood=Imp
Mood=Ind
Number=Plur
Number=Sing
Polarity=Neg
Poss=Yes
PronType=Art
PronType=Ind
PronType=Int,Rel
Tense=Past
Tense=Pres
VerbForm=Fin
VerbForm=Inf
VerbForm=Part
VerbForm=Sup
Voice=Act
Voice=Pass

-- fi-pud 85
Abbr=Yes
AdpType=Post
AdpType=Prep
Case=Abe
Case=Abl
Case=Acc
Case=Ade
Case=All
Case=Com
Case=Ela
Case=Ess
Case=Gen
Case=Ill
Case=Ine
Case=Ins
Case=Nom
Case=Par
Case=Tra
Clitic=Ka
Clitic=Kaan
Clitic=Kin
Clitic=Ko
Clitic=Pa
Connegative=Yes
Degree=Cmp
Degree=Pos
Degree=Sup
Derivation=Inen
Derivation=Inen,Ttain
Derivation=Inen,Vs
Derivation=Ja
Derivation=Ja,Tar
Derivation=Lainen
Derivation=Lainen,Vs
Derivation=Llinen
Derivation=Llinen,Sti
Derivation=Llinen,Vs
Derivation=Minen
Derivation=Sti
Derivation=Sti,Ton
Derivation=Tar
Derivation=Ton
Derivation=Ton,Vs
Derivation=Ttain
Derivation=U
Derivation=Vs
Foreign=Yes
InfForm=1
InfForm=2
InfForm=3
Mood=Cnd
Mood=Imp
Mood=Ind
NumType=Card
NumType=Ord
Number=Plur
Number=Sing
Number[psor]=Plur
Number[psor]=Sing
PartForm=Agt
PartForm=Past
PartForm=Pres
Person=0
Person=1
Person=2
Person=3
Person[psor]=1 -- possessive suffix
Person[psor]=2
Person[psor]=3
Polarity=Neg
PronType=Dem
PronType=Ind
PronType=Int
PronType=Prs
PronType=Rcp
PronType=Rel
Reflex=Yes
Style=Coll
Tense=Past
Tense=Pres
VerbForm=Fin
VerbForm=Inf
VerbForm=Part
Voice=Act
Voice=Pass

-}
