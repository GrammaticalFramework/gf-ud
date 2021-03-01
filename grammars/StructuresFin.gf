concrete StructuresFin of Structures =
  GrammarFin - [
    PNeg,
    language_title_Utt, Slash3V3, PassV2,
    IndefArt, DefArt
    ],
  LexiconFin,
  ExtendFin [
    CompoundN,
    GenModNP ----
    ]
    ** open SyntaxFin, (P=ParadigmsFin), (R=ResFin), (E=ExtendFin) in {

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

    PredPassVPSlash np vps = mkCl np (E.PassVPSlash vps) ;
    PredPassSCVPSlash sc vps = mkCl sc (E.PassVPSlash vps) ;

    UttList u v = {s = u.s ++ "," ++ v.s} ;
    DirectComplVS vs u = mkVP vs <lin S {s = ":" ++ u.s} : S> ;
    ApposNP np appos = mkNP np (mkAdv (P.mkPrep ",") appos) ; 
  }
