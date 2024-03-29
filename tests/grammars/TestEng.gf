--# -path=../../grammars
concrete TestEng of Test = MiniLangEng - [PredVP] ** open (R=MiniResEng) in {
    lincat
      UDS = {s : Str} ;
    lin
       -- : VP -> NP -> Adv -> UDS ; -- [the cat]:NP [sleeps]:VP [today]:Adv
      root_nsubj_obl vp np adv = root_nsubj (AdvVP vp adv) np ;
      -- : VP -> NP -> UDS ;        -- [the cat]:NP [sleeps today]:VP
      root_nsubj vp np = UseCl TSim PPos (PredVP np vp) ;

      -- : Det
      someSg_Det = {s = "some" ; n = R.Sg} ;
      somePl_Det = {s = "some" ; n = R.Pl} ;
      anySg_Det = {s = "any" ; n = R.Sg} ;
      anyPl_Det = {s = "any" ; n = R.Pl} ;
}