abstract ShallowParse =

  Backups,
  
  JustWordsWordNet,

 Phrase [
    Text,Utt,S,QS,Adv,NP,Pol,Imp,
    UttS      , -- S  -> Utt ;         -- John walks
    UttQS     , -- QS -> Utt ;         -- does John walk
    UttNP     , -- NP -> Utt ;         -- John
    UttAdv      -- Adv -> Utt ;        -- in the house
 ----   UttImpSg   -- Pol -> Imp -> Utt ; -- (do not) walk ----s
    ],

Extend [
    VPS,VPI,
    MkVPS,    --    : Temp -> Pol -> VP -> VPS ;  -- hasn't slept
----    ConjVPS,  --    : Conj -> [VPS] -> VPS ;      -- has walked and won't sleep
    PredVPS,  --    : NP   -> VPS -> S ;          -- she [has walked and won't sleep]

    MkVPI     --  : VP -> VPI ;                   -- to sleep (TODO: Ant and Pol)
----    ConjVPI   -- : Conj -> [VPI] -> VPI ;         -- to sleep and to walk
    
----    BaseVPS, ConsVPS,
----    BaseVPI, ConsVPI
---- TODO: Extend cannot form QS yet

    ],
    
 Sentence [
    S,QS,Cl,QCl,NP,Temp,Pol,VP,Imp
---    UseCl     , -- Temp -> Pol -> Cl   -> S ;  -- John has not walked
---    UseQCl    , -- Temp -> Pol -> QCl  -> QS ; -- has John walked
---    PredVP     -- NP -> VP -> Cl ;            -- John walks / John does not walk
----    ImpVP       -- VP -> Imp ;                 -- walk / do not walk
    ],
    
 Verb [
    VP,AdV,Adv,AP,Comp,V,
    UseV      , -- V   -> VP ;             -- sleep
    UseComp,
    CompAP,
    UseNP     , -- NP  -> VP ;             -- be a man ---s
    UseAdv    , -- Adv -> VP ;             -- be in the house ---s
    AdvVP    , -- VP -> Adv -> VP ;       -- sleep here
    AdVVP
    ],
    
 Noun [
    NP,CN,AP,Adv,Ord,RS,Pron,PN,Det,Numeral,N,
    DetCN     , -- Det -> CN -> NP ;       -- the man
    UsePN     , -- PN -> NP ;              -- John
    UsePron   , -- Pron -> NP ;            -- he
    MassNP    , -- CN -> NP ;              -- milk
    UseN      , -- N -> CN ;               -- house
    AdjCN,      -- AP -> CN -> CN ;        -- big house
    OrdNumeral,
    RelCN,
    AdvCN
    ],
    
 Adjective [
    AP,AdA,A,Ord,
    PositA    , -- A  -> AP ;              -- warm
    UseComparA,
    AdAP,
    AdjOrd
    ],
    
 Adverb [
    Prep,NP,Adv,Subj,S,
    PrepNP    , -- Prep -> NP -> Adv ;     -- in the house
    SubjS
    ],

 Conjunction,
 Relative,
 Question,
 Numeral,

 Tense

** {

  cat
    PP ; Punct ;

  fun


    AddNPtoVP  : VP  -> NP  -> VP ;  -- love it
    AddPPtoVP  : VP  -> PP  -> VP ;  -- talk about it
    AddStoVP   : VP  -> S   -> VP ;  -- say that it is good
    AddVPItoVP : VP  -> VPI -> VP ;  -- want to sleep
    AddVPItoAuxVP : VP  -> VPI -> VP ;  -- must sleep --- English-specific


    PrepPP : Prep -> NP -> PP ;

    QuantSgCN  : Quant -> CN -> NP ;
    QuantPlCN  : Quant -> CN -> NP ;
    CardCN     : Numeral -> CN -> NP ;
    PossSgNP   : Pron -> CN -> NP ;
    PossPlNP   : Pron -> CN -> NP ;
    
    a_Det     : Det ;                   -- indefinite singular ---s
    aPl_Det   : Det ;                   -- indefinite plural   ---s
    the_Det   : Det ;                   -- definite singular   ---s
    thePl_Det : Det ;                      -- definite plural     ---s

    PunctUttText : Utt -> Punct -> Text ;
    UttText : Utt -> Text ;
    
    fullstop_Punct : Punct ;
    comma_Punct : Punct ;
    questionmark_Punct : Punct ;
    exclmark_Punct : Punct ;

}