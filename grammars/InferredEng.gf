concrete InferredEng of Inferred =
  CatEng,
  StructuralEng - [both7and_DConj, either7or_DConj, yes_Utt, no_Utt, language_title_Utt],
  LexiconEng **

open SyntaxEng, ExtendEng, (P = ParadigmsEng)
in {
-- grammar inferred by post-editing the result of
--  *UDAnalysis> testUDTypes 40 "ud/UD_English-EWT/en_ewt-ud-train.conllu"
--

lincat
  PP = Adv ;
  NNP = NP ; -- single-N NP, sg or pl
  AdvP = Adv ; -- adverbial phrase
  Poss = Det ; -- possessive determiner
  VPP = Adv ; ----
  Punct = {s : Str} ;

lin
  SgNNP n = mkNP n ;
  PlNNP n = mkNP aPl_Det n ;
  UsePN pn = mkNP pn ;
  PositA a = mkAP a ;
  ComparA a = comparAP a ;
  periodPunct = {s = "."} ;

oper punctUtt : Utt -> {s : Str} -> Utt = \u,p -> lin Utt {s = u.s ++ p.s} ;


lin
DetN det n = mkNP det n ;

PrepDetN prep det n = mkAdv prep (mkNP det n) ;

PrepNNP prep nnp = mkAdv prep nnp ;

Conj2NP x conj y = mkNP conj x y ;

PrepPN prep pn = mkAdv prep (mkNP pn) ;

PrepPron prep pron = mkAdv prep (mkNP pron) ;

PossN pron n = mkNP pron n ;

PrepPossN prep pron n = mkAdv prep (mkNP pron n) ;

DetAdjN det ap n = mkNP det (mkCN ap n) ;

AdAdvP x y = mkAdv (lin AdA x) y ;
FixedAdvP x y = P.mkAdv (x.s ++ y.s) ;
DetNPrep det n pp = mkNP det (mkCN n pp) ;



PrepDetAdjN prep det ap n = mkAdv prep (mkNP det (mkCN ap n)) ;

AdAP adv a = mkAP (lin AdA adv) a ;

PrepDetNPrep prep det n pp = mkAdv prep (mkNP det (mkCN n pp)) ;

SgAdjNNP ap n = mkNP (mkCN ap n) ;
PlAdjNNP ap n = mkNP aPl_Det (mkCN ap n) ;

---- GenPN pn = GenNP : PN -> Det ; -- head case 280 ; nmod:poss ; Stuart 's ; PART
CompPN x y = mkNP (mkNP x) (lin Adv (mkUtt (mkNP y))) ; --- y is head
FlatPN x y = mkNP (mkNP x) (lin Adv (mkUtt (mkNP y))) ;
InfComplV2 v2 np = lin Adv (mkUtt (mkVP v2 np)) ;



Conj2AP x conj y = mkAP conj x y ;

SgPrepAdjN prep ap n = mkAdv prep (mkNP (mkCN ap n)) ;
PlPrepAdjN prep ap n = mkAdv prep (mkNP aPl_Det (mkCN ap n)) ;


CompoundN x y = ExtendEng.CompoundN x y ;

DetCompoundCN det x y = mkNP det (ExtendEng.CompoundN x y) ;

PrepDetCompoundN prep det x y = mkAdv prep (mkNP det (ExtendEng.CompoundN x y)) ;

--- AP -> CN -> CN ; -- amod head 184 ; compound ; decent portion
--- AP -> CN -> CN ; -- amod head 67 ; obl:tmod ; several times
--- Conj -> VP -> NP -> VP ; -- cc head obj 184 ; conj ; and replace tires
Conj3NP x y conj z = mkNP conj (mkListNP x (mkListNP y z)) ;

DetNinfVP det n vp = mkNP det (mkCN (mkCN n) vp) ;
DetNppartVP det n v2 = mkNP (mkNP det n) v2 ;

----DetCNingVP : Det -> CN -> VP -> NP ; -- det head acl 59 ; nsubj ; the girl helping
ComplPrepVP vp pp = mkVP vp pp ;

-- VP -> PP -> VP ; -- head obl 61 ; acl ; run Tina
-- Card -> CN -> CN ; -- nummod head 153 ; nmod:tmod ; 610 AD
-- Card -> CN -> CN ; -- nummod head 85 ; obl:npmod ; ten minutes
-- Card -> CN -> CN ; -- nummod head 58 ; compound ; 20 hour
-- Card -> CN -> CN ; -- nummod head 40 ; obl:tmod ; 3 times
-- PN -> PN -> NP ; -- head flat 153 ; nsubj ; Michael Chestney
-- PN -> PN -> NP ; -- compound head 66 ; nsubj ; Wedding Gallery
-- Prep -> CN -> CN -> PP ; -- case compound head 150 ; nmod ; of dog introductions
-- Prep -> CN -> CN -> PP ; -- case compound head 118 ; obl ; about minute rates
-- Prep -> CN -> CN -> PP ; -- case head conj 88 ; nmod ; of designers dresses
-- Prep -> CN -> CN -> PP ; -- case head conj 53 ; obl ; in service quality
-- Prep -> PN -> PN -> PP ; -- case compound head 148 ; nmod ; in Rittenhouse Square
-- Prep -> PN -> PN -> PP ; -- case compound head 94 ; obl ; from Second Home
-- Prep -> PN -> PN -> PP ; -- case head conj 54 ; nmod ; of will grace
-- Prep -> PN -> PN -> PP ; -- case head flat 52 ; obl ; to San Antonio
-- Prep -> PN -> PN -> PP ; -- case head flat 48 ; nmod ; of San Antonio
-- Prep -> Card -> CN -> PP ; -- case nummod head 147 ; obl ; for 3 days
-- Prep -> Card -> CN -> PP ; -- case nummod head 50 ; nmod ; to 10 AM
-- Conj -> VP -> VP ; -- cc head 145 ; conj ; and try
-- NP -> VP -> VP ; -- nsubj head 140 ; acl:relcl ; she is
-- CN -> Punct -> CN ; -- head punct 134 ; root ; Help ?
-- Card -> CN -> Card ; -- head nmod:tmod 133 ; root ; 11/29/2000 AM
-- NP -> VP -> VP -> Punct -> VP ; -- nsubj head ccomp punct 132 ; root ; I guess tells .
-- NP -> VP -> VP -> Punct -> VP ; -- nsubj head ccomp punct 90 ; root ; thing is have .
-- NP -> VP -> VP -> Punct -> VP ; -- nsubj head xcomp punct 71 ; root ; They need update .
-- Card -> Punct -> CN -> Punct -> Card ; -- head punct appos punct 129 ; root ; 5 - Number .
-- CN -> CN -> NP ; -- compound head 128 ; obj ; mildew problems
-- CN -> CN -> NP ; -- compound head 74 ; nsubj ; minute price
-- CN -> CN -> NP ; -- head conj 51 ; obj ; receipt record
-- Det -> PN -> NP ; -- det head 127 ; nsubj ; that Warwick
-- Det -> PN -> NP ; -- det head 49 ; obj ; a nissan
-- Det -> CN -> CN ; -- det head 126 ; nsubj:pass ; the fence
-- Det -> CN -> CN ; -- det head 97 ; obl:npmod ; a lot
-- Det -> CN -> CN ; -- det head 91 ; obl:tmod ; this evening
-- CN -> PP -> NP ; -- head nmod 126 ; obj ; grease wheel
-- CN -> PP -> NP ; -- head nmod 43 ; nsubj ; Security hotel
-- PN -> Punct -> PN ; -- head punct 126 ; root ; Manson ?
-- Prep -> CN -> PP -> PP ; -- case head nmod 125 ; obl ; with crap hair
-- Prep -> CN -> PP -> PP ; -- case head nmod 89 ; nmod ; with directions room
-- Prep -> Card -> PP ; -- case head 124 ; obl ; circa 7
-- Prep -> Card -> PP ; -- case head 120 ; nmod ; o 1998
-- Subj -> NP -> VP -> NP -> VP ; -- mark nsubj head obj 123 ; advcl ; until they ordered dress
UttPredObj subj v obj punct = punctUtt (mkUtt (mkS (mkCl subj (P.mkV2 v) obj))) punct ;

-- Punct -> PN -> PN ; -- punct head 118 ; conj ; , Boyles
-- Prep -> Prep -> Prep ; -- head fixed 116 ; case ; due to
-- Subj -> VP -> NP -> VP ; -- mark head obj 113 ; advcl ; after checking tire
-- Subj -> VP -> NP -> VP ; -- mark head obj 58 ; acl ; of canceling membership
-- PART -> VP -> VP ; -- mark head 110 ; xcomp ; to do
-- PART -> VP -> VP ; -- mark head 51 ; acl ; to hide
-- Conj -> AP -> CN -> CN ; -- cc amod head 104 ; conj ; and lighter fare
-- Conj -> CN -> CN -> CN ; -- cc compound head 102 ; conj ; and check card
-- Det -> AP -> CN -> PP -> NP ; -- det amod head nmod 95 ; obj ; some serious doubts quality
-- Det -> AP -> CN -> PP -> NP ; -- det amod head nmod 53 ; nsubj ; an obvious problem staff
-- AP -> AP -> AP ; -- head conj 93 ; amod ; long bad
-- NP -> VP -> NP -> VP ; -- nsubj head obj 92 ; acl:relcl ; which had problems
-- Prep -> Det -> PN -> PP ; -- case det head 91 ; nmod ; at this Sear's
-- Prep -> Det -> PN -> PP ; -- case det head 80 ; obl ; on a Sunday
-- Adv -> PP -> Adv ; -- head obl 89 ; advmod ; Aside mystery
-- Card -> CN -> NP ; -- nummod head 89 ; obj ; 20 minutes
-- Det -> CN -> PART -> CN ; -- det head case 88 ; nmod:poss ; the club 's
-- VP -> NP -> VP ; -- head obj 85 ; acl ; making claims
-- VP -> NP -> VP ; -- head obj 59 ; xcomp ; installing fence
-- VP -> NP -> VP ; -- head obj 49 ; advcl ; driving SUV
-- Card -> Card -> Card ; -- compound head 83 ; nummod ; two million
-- Prep -> Det -> CN -> VP -> PP ; -- case det head acl:relcl 83 ; obl ; with the gown wear
-- Prep -> Det -> CN -> VP -> PP ; -- case det head acl 61 ; obl ; for a place take
-- Prep -> Det -> CN -> VP -> PP ; -- case det head acl:relcl 56 ; nmod ; of the one comparing
-- Pron -> AP -> CN -> NP ; -- nmod:poss amod head 80 ; obj ; your exact address
-- Pron -> AP -> CN -> NP ; -- nmod:poss amod head 43 ; nsubj ; My first visit
-- Prep -> Det -> PN -> CN -> PP ; -- case det compound head 78 ; obl ; from the Squirrels stadium
-- Prep -> Det -> PN -> CN -> PP ; -- case det compound head 62 ; nmod ; in the San area
-- PART -> VP -> PP -> VP ; -- mark head obl 78 ; xcomp ; to get work
-- Conj -> Pron -> Pron ; -- cc head 77 ; conj ; and I
-- Prep -> Det -> PN -> PN -> PP ; -- case det compound head 76 ; nmod ; of The Laundry Tub
-- Prep -> Det -> PN -> PN -> PP ; -- case det compound head 51 ; obl ; for a Mini Cooper
-- CN -> Adv -> Adv ; -- obl:npmod head 75 ; advmod ; year ago
-- Adv -> Card -> Card ; -- advmod head 74 ; nummod ; about 20
-- Prep -> Det -> AP -> CN -> PP -> PP ; -- case det amod head nmod 70 ; obl ; on the important day life
-- Prep -> Pron -> AP -> CN -> PP ; -- case nmod:poss amod head 70 ; obl ; by their own seamstresses
-- CN -> PP -> CN ; -- head nmod 69 ; appos ; Number states
-- SYM -> Card -> PP ; -- case head 66 ; nmod ; - 10
-- Conj -> Adv -> Adv ; -- cc head 62 ; conj ; and down
-- Subj -> NP -> VP -> VP ; -- mark nsubj head 62 ; advcl ; than I expected
-- AP -> CN -> PP -> NP ; -- amod head nmod 60 ; obj ; few place area
-- Prep -> AP -> PP ; -- case head 60 ; obl ; for more
-- PART -> VP -> NP -> PP -> VP ; -- mark head obj obl 60 ; xcomp ; to pull car shop
-- X -> Punct -> Card ; -- head punct 59 ; nummod ; b )
-- VP -> NP -> Punct -> VP ; -- head obj punct 58 ; root ; Imagine hotel .
-- Punct -> AP -> AP ; -- punct head 55 ; conj ; , knowledgeable
-- Pron -> CN -> CN -> NP ; -- nmod:poss compound head 55 ; obj ; my wheel lock
-- Pron -> CN -> CN -> NP ; -- nmod:poss compound head 48 ; nsubj ; my payment structure
-- Conj -> VP -> PP -> VP ; -- cc head obl 55 ; conj ; and argue customer
-- AP -> CN -> Punct -> CN ; -- amod head punct 54 ; root ; AWFUL SERVICE !
-- PN -> Card -> PN ; -- head nummod 54 ; appos ; November 22
-- NP -> AUX -> VP -> NP -> Punct -> VP ; -- nsubj aux head obj punct 53 ; root ; We were having problem .
-- Card -> PP -> Card ; -- head nmod 52 ; nummod ; 5 10
-- Pron -> VP -> NP ; -- head acl:relcl 52 ; obj ; anyone knew
-- Prep -> Adv -> PP ; -- case head 52 ; obl ; on here
-- Conj -> VP -> VP -> VP ; -- cc head xcomp 52 ; conj ; and left saying
-- Punct -> AP -> CN -> CN ; -- punct amod head 51 ; conj ; , tasty lamb
-- Prep -> PN -> Card -> PP ; -- case head nummod 50 ; obl ; in August 2008
-- Prep -> Pron -> CN -> CN -> PP ; -- case nmod:poss compound head 50 ; obl ; with our dining experience
-- Conj -> Pron -> CN -> CN ; -- cc nmod:poss head 48 ; conj ; and my son
-- NP -> AUX -> VP -> VP -> Punct -> VP ; -- nsubj aux head xcomp punct 48 ; root ; He was going operate .
-- Interj -> Punct -> Interj ; -- head punct 47 ; root ; YES !
-- Pron -> VP -> NP -> Punct -> VP ; -- expl head nsubj punct 47 ; root ; There are people !!!!!
-- Pron -> CN -> PART -> CN ; -- nmod:poss head case 46 ; nmod:poss ; their employees '
-- Det -> AP -> CN -> CN -> NP ; -- det amod compound head 46 ; obj ; some serious practice issues
-- Det -> PN -> PN -> NP ; -- det compound head 46 ; nsubj ; The Laundry Tub
-- SYM -> Card -> Punct -> CN -> Punct -> SYM ; -- head nummod punct appos punct 46 ; root ; $ m - Amount .
-- PART -> VP -> NP -> VP -> VP ; -- mark head obj advcl 46 ; xcomp ; to take time fix
-- Subj -> NP -> VP -> VP -> VP ; -- mark nsubj head xcomp 46 ; advcl ; since I had complete
-- Conj -> Det -> CN -> CN ; -- cc det head 45 ; conj ; and a half
-- Punct -> CN -> CN -> CN ; -- punct compound head 45 ; conj ; , tea backs
-- Adv -> Adv -> PP ; -- case head 45 ; nmod ; at least
-- NP -> AUX -> VP -> VP ; -- nsubj aux head 45 ; acl:relcl ; they 'd used
-- Det -> PN -> CN -> NP ; -- det compound head 44 ; obj ; this Midas store
-- PART -> VP -> VP -> VP ; -- mark head ccomp 44 ; xcomp ; to realize going
-- Conj -> PN -> PN -> PN ; -- cc compound head 43 ; conj ; and G&G Automotive
-- NP -> VP -> NP -> VP -> Punct -> VP ; -- nsubj head obj conj punct 43 ; root ; I called number listened .
-- AP -> PP -> AP ; -- head obl 42 ; amod ; better folks
-- Det -> AP -> CN -> CN ; -- det amod head 42 ; obl:tmod ; The next day
-- Conj -> VP -> NP -> PP -> VP ; -- cc head obj obl 42 ; conj ; and replace valve stenosis
-- NP -> VP -> VP -> VP ; -- nsubj head xcomp 42 ; acl:relcl ; you have do
-- Adv -> VP -> Adv ; -- head advcl 41 ; advmod ; incredibly had
-- CN -> PART -> CN ; -- head case 41 ; nmod:poss ; doctor s
-- CN -> VP -> NP ; -- head acl 41 ; obj ; claims going
-- AP -> Punct -> CN -> CN ; -- amod punct head 40 ; compound ; next - door
-- Conj -> Card -> Card ; -- cc head 40 ; conj ; or 5
-- PN -> PN -> PART -> PN ; -- head flat case 40 ; nmod:poss ; Ronald Reagan 's
-- NP -> VP -> PP -> VP ; -- nsubj head obl 40 ; acl:relcl ; he put car
-- VP -> NP -> VP -> Punct -> VP ; -- head obj xcomp punct 40 ; root ; Let us know .

}