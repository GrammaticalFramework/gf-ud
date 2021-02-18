abstract Structures =
  Grammar - [
    language_title_Utt
    ],
  Lexicon,
  Extend [
    CompoundN,
    GenModNP
    ]
    ** {

flags startcat = Top ;

cat
  Top ;
  Punct ;

fun
  UttPunctTop : Utt -> Punct -> Top ;
  UttVocativePunctTop : Utt -> NP -> Punct -> Top ;
  ConjUttPunctTop : Conj -> Utt -> Punct -> Top ;
  FullStopPunct : Punct ;
  ExclMarkPunct : Punct ;
  QuestMarkPunct : Punct ;

  CompoundPN : PN -> PN -> PN ;
  FlatPN : PN -> PN -> PN ;

}