--# -path=../../grammars
-- This grammar is there to test different ways to order the candidate trees.
-- For example, we want to see if one way leads to flatter trees than the other.
abstract Test = MiniLang - [PredVP] ** {
    flags startcat = UDS ;

    cat UDS ;

    -- To test ordering of trees
    fun root_nsubj_obl : VP -> NP -> Adv -> UDS ; -- [the cat]:NP [sleeps]:VP [today]:Adv
    fun root_nsubj     : VP -> NP -> UDS ;        -- [the cat]:NP [sleeps today]:VP

    -- To test lemma/wordform in auxfuns
    fun someSg_Det, somePl_Det, anySg_Det, anyPl_Det : Det ;

    -- To test distance feature
    cat Num ;
    fun ten_Num : Num ;
    fun num2Det : Num -> Det ;

    fun ApposNum : CN -> Num -> CN ; -- Section 10

}