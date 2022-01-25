--# -path=../../grammars
concrete TestOrderingEng of TestOrdering = MiniLangEng - [PredVP] ** {
    lincat
      UDS = {s : Str} ;
    lin
       -- : VP -> NP -> Adv -> UDS ; -- [the cat]:NP [sleeps]:VP [today]:Adv
      root_nsubj_obl vp np adv = root_nsubj (AdvVP vp adv) np ;
      -- : VP -> NP -> UDS ;        -- [the cat]:NP [sleeps today]:VP
      root_nsubj vp np = UseCl TSim PPos (PredVP np vp) ;
}