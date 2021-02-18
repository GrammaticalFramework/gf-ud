concrete StructuresEng of Structures =
  GrammarEng - [
    PNeg,
    language_title_Utt
    ],
  LexiconEng,
  ExtendEng [
    CompoundN,
    GenModNP ----
    ]
    ** open SyntaxEng, (P=ParadigmsEng), (R=ResEng) in {

  lin
    PNeg = UncontractedNeg ;

  lincat
    Top = {s : Str} ;
    Punct = {s : Str} ;

  lin
    UttPunctTop utt punct = {s = utt.s ++ punct.s} ;
    UttVocativePunctTop utt voc punct = {s = utt.s ++ "," ++ (mkUtt voc).s ++ punct.s} ;
    ConjUttPunctTop conj utt punct = {s = conj.s2 ++ utt.s ++ punct.s} ;

    FullStopPunct = {s = "."} ;
    ExclMarkPunct = {s = "!"} ;
    QuestMarkPunct = {s = "?"} ;

    CompoundPN a b = b ** {s = \\c => a.s ! R.Nom ++ b.s ! c} ; -- gender from b
    FlatPN     a b = a ** {s = \\c => a.s ! R.Nom ++ b.s ! c} ; -- gender from a
    
  }
