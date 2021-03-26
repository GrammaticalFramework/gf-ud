abstract Extract = Grammar [

-- collected from GF/lib/src/abstract/*.gf
-- the functions marked ---s are shortcuts
-- the leading comments, e.g. "-- Common", indicate the standard RGL module

---  cat
  
-- Common
    Utt ,    -- sentence, question, word...         e.g. "be quiet"

-- Cat
    Cl ,     -- declarative clause, with all tenses e.g. "she looks at this"
    VP ,     -- verb phrase                         e.g. "lives here"
    AP ,     -- adjectival phrase                   e.g. "very warm"
    CN ,     -- common noun (without determiner)    e.g. "red house"
    NP ,     -- noun phrase (subject or object)     e.g. "the red house"
    Pron ,   -- personal pronoun                    e.g. "she"
    Det ,    -- determiner phrase                   e.g. "those"
    Conj ,   -- conjunction                         e.g. "and"
    Prep ,   -- preposition, or just case           e.g. "in", dative
    V ,      -- one-place verb                      e.g. "sleep" 
    A ,      -- one-place adjective                 e.g. "warm"
    N ,      -- common noun                         e.g. "house"
    PN ,     -- proper name                         e.g. "Paris"
    Adv ,    -- adverbial phrase                    e.g. "in the house"
    
--  fun
-- Phrase
    UttNP     , -- NP -> Utt ;         -- the big house
    UttCP     , -- CN -> Utt ;         -- big house
    UttAdv    , -- Adv -> Utt ;        -- in the house
    UttVP     , -- VP -> Utt ;         -- love her

-- Sentence
    PredVP    , -- NP -> VP -> Cl ;        -- John walks / John does not walk

-- Verb
    UseV      , -- V   -> VP ;             -- sleep
    AdvVP     , -- VP -> Adv -> VP ;       -- sleep here

-- Noun
    DetCN     , -- Det -> CN -> NP ;       -- the man
    MassNP    , -- CN -> NP                -- beer
    UsePN     , -- PN -> NP ;              -- John
    UsePron   , -- Pron -> NP ;            -- he    
    UseN      , -- N -> CN ;               -- house
    AdjCN     , -- AP -> CN -> CN ;        -- big house
    AdvCN     , -- CN -> Adv -> CN ;       -- house here

-- Adjective
    PositA    , -- A  -> AP ;              -- warm

-- Structural
    and_Conj  , -- Conj ;
    or_Conj   , -- Conj ;
    
    every_Det , -- Det ;

    by8means_Prep ,  -- Prep ;
    for_Prep , -- Prep ;
    from_Prep , -- Prep ;
    in_Prep   , -- Prep ;
    on_Prep   , -- Prep ;
    to_Prep   , -- Prep ;
    with_Prep , -- Prep ;
    possess_Prep , -- Prep ;

    i_Pron     , -- Pron ;
    youSg_Pron , -- Pron ;
    he_Pron    , -- Pron ;
    she_Pron   , -- Pron ;
    it_Pron    , -- Pron ;
    we_Pron    , -- Pron ;
    youPl_Pron , -- Pron ;
    they_Pron    -- Pron ;
    ] ** {

cat
    PP ;

fun
    UttCl  : Cl  -> Utt ;          -- John walks ---s
    ComplV : V -> NP -> VP ;       -- love it  ---s

    PrepNP  : Prep -> NP -> PP ;    -- in the house
    PrepPP  : VP -> PP -> VP ;      -- live in a house
    PrepCN  : CN -> PP -> CN ;      -- house on the hill

    FlatPN : PN -> PN -> PN ;
    CompoundN : N -> N -> N ;
    
    UseAP     : AP  -> VP ;             -- be small ---s
    UseNP     : NP  -> VP ;             -- be a man ---s
    UseAdv    : Adv -> VP ;             -- be in the house ---s
    PluralNP  : CN -> NP ;              -- houses ---s

    a_Det     : Det ;                   -- indefinite singular ---s
    aPl_Det   : Det ;                   -- indefinite plural ---s
    the_Det   : Det ;                   -- definite singular   ---s
    thePl_Det : Det ;                   -- definite plural     ---s
    this_Det  : Det ;
    that_Det  : Det ;
    these_Det  : Det ;
    those_Det  : Det ;

}