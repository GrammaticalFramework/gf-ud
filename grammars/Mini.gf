abstract Mini = Grammar [
  Cl, NP, VP, VPSlash, CN, AP, Det, N, A, V2, Adv, Pron,
  PredVP, --  : NP -> VP -> S ;
  DetCN,  --  : Det -> CN -> NP ;
  AdjCN,  --  : AP -> CN -> CN ;
  PositA, --  : A -> AP ;
  UseN,   --  : N -> CN ;
  ComplSlash,
  SlashV2a,
  UsePron,
  AdvVP,  --  : VP -> Adv -> VP ; 

  every_Det,
  we_Pron
  ],

  Lexicon [
    cat_N,
    black_A,
    see_V2,
    we_Pron,
    today_Adv
    ]

** {
  flags startcat = Cl ;
  }