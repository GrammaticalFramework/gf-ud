concrete StructuresSpa of Structures =
  GrammarSpa - [
    PNeg,
    language_title_Utt, Slash3V3, PassV2,
    IndefArt
    ],
  LexiconSpa,
  ExtendSpa [
    CompoundN,
    GenModNP ----
    ]
    ** open SyntaxSpa, (P=ParadigmsSpa), (R=ResSpa), (E=ExtendSpa) in {

  lin
    PNeg = UncontractedNeg ;

  lincat
    Top = {s : Str} ;
    Punct = {s1,s2 : Str} ;

  lin
    UttPunctTop utt punct = {s = punct.s1 ++ utt.s ++ punct.s2} ;
    UttVocativePunctTop utt voc punct = {s = punct.s1 ++ utt.s ++ "," ++ (mkUtt voc).s ++ punct.s2} ;
    ConjUttPunctTop conj utt punct = {s = punct.s1 ++ conj.s2 ++ utt.s ++ punct.s2} ;

    FullStopPunct = {s1 = "" ; s2 = "."} ;
    ExclMarkPunct = {s1 = "¡" ; s2 = "!"} ;
    QuestMarkPunct = {s1 = "¿" ; s2 = "?"} ;

    CompoundPN a b = b ** {s = a.s ++ b.s} ; -- gender from b
    FlatPN     a b = a ** {s = a.s ++ b.s} ; -- gender from a

    PredPassVPSlash np vps = mkCl np (E.PassVPSlash vps) ;
    PredPassSCVPSlash sc vps = mkCl sc (E.PassVPSlash vps) ;

    UttList u v = {s = u.s ++ "," ++ v.s} ;
    DirectComplVS vs u = mkVP vs <lin S {s = \\_ => ":" ++ u.s} : S> ;
    ApposNP np appos = mkNP np (mkAdv (P.mkPrep ",") appos) ; 
  }
