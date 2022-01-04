--# -path=../../grammars
-- This grammar is there to test different ways to order the candidate trees.
-- For example, we want to see if one way leads to flatter trees than the other.
abstract TestOrdering = MiniLang - [PredVP] ** {
    flags startcat = UDS ;

    cat UDS ;
    fun root_nsubj_obl : VP -> NP -> Adv -> UDS ; -- [the cat]:NP [sleeps]:VP [today]:Adv
    fun root_nsubj     : VP -> NP -> UDS ;        -- [the cat]:NP [sleeps today]:VP
}