abstract Structures =
  Grammar - [
    language_title_Utt, Slash3V3, PassV2,
    IndefArt
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

  PredPassVPSlash : NP -> VPSlash -> Cl ;
  PredPassSCVPSlash : SC -> VPSlash -> Cl ;
  UttList : Utt -> Utt -> Utt ;
  DirectComplVS : VS -> Utt -> VP ;

  ApposNP : NP -> NP -> NP ;


}