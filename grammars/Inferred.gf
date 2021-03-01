abstract Inferred =
  Cat,
  Structural - [both7and_DConj, either7or_DConj, if_then_Conj, yes_Utt, no_Utt, language_title_Utt],
  Lexicon ** {
-- grammar inferred by post-editing the result of
--  *UDAnalysis> testUDTypes 20 "ud/UD_English-EWT/en_ewt-ud-train.conllu"
--

cat
  NN ; -- like N, but containing compounds
  PP ;
  NNP ; -- single-N NP, sg or pl
  AdvP ; -- adverbial phrase
  AdvCl ; -- clausal adv phrase
  Poss ; -- possessive determiner
  VPP ;  -- infinitival VP
  Punct ; -- punctuation mark
--- for synthesis experiments, Eng specific
  There ; -- expletive
  Be ; -- copula of existential
  NPConj ; -- conjunct with Conj as cc
  NPComma ; -- conjunct with , as punct
  APConj ;
  VPConj ;
  SConj ;

-- not seen in UD explicitly
fun

  UseN : N -> NN ; -- non-compound noun
  SgNNP : NN -> NNP ;
  PlNNP : NN -> NNP ;
  UsePN : PN -> NP ;
  UsePron : Pron -> NP ;
  SPredObj : NP -> V -> NP -> S ;
  PositA : A -> AP ;
  ComparA : A -> AP ;
  periodPunct : Punct ;
  fiveCard : Card ;
---
  be_Be : Be ;
  there_There : There ;

fun
DetN : Det -> NN -> NP ; -- det head 1248 ; obj ; a flat
-- Det -> NN -> NP ; -- det head 956 ; nsubj ; no one
-- Det -> NN -> NN ; -- det head 126 ; nsubj:pass ; the fence
-- Det -> NN -> NN ; -- det head 97 ; obl:npmod ; a lot
-- Det -> NN -> NN ; -- det head 91 ; obl:tmod ; this evening
PrepDetN : Prep -> Det -> NN -> PP ; -- case det head 1130 ; obl ; in the car
-- Prep -> Det -> NN -> PP ; -- case det head 706 ; nmod ; around this city
PrepNNP : Prep -> NNP -> PP ; -- case head 785 ; nmod ; of TIME
-- Prep -> NN -> PP ; -- case head 669 ; obl ; in time
Conj2NP : NP -> NPConj -> NP ; -- head conj 648 ;
ConjNPConj : Conj -> NP -> NPConj ; -- cc head 648 ; conj ; and dress
-- Conj -> PN -> PN ; -- cc head 283 ; conj ; and 15th
-- NN -> NN -> NP ; -- head conj 51 ; obj ; receipt record
PrepPN : Prep -> PN -> PP ; -- case head 636 ; nmod ; like Zahav
-- Prep -> PN -> PP ; -- case head 567 ; obl ; On Monday
PrepPron : Prep -> Pron -> PP ; -- case head 621 ; obl ; for me
-- Prep -> Pron -> PP ; -- case head 247 ; nmod ; about it
PossN : Pron -> NN -> NP ; -- nmod:poss head 555 ; obj ; my car
-- Pron -> NN -> NP ; -- nmod:poss head 358 ; nsubj ; its name
PrepPossN : Prep -> Pron -> NN -> PP ; -- case nmod:poss head 388 ; obl ; on my car
-- Prep -> Pron -> NN -> PP ; -- case nmod:poss head 229 ; nmod ; on my car
DetAdjN : Det -> AP -> NN -> NP ; -- det amod head 380 ; obj ; an exact ADDRESS
-- Det -> AP -> NN -> NP ; -- det amod head 192 ; nsubj ; that particular technician
AdAdvP : Adv -> Adv -> AdvP ; -- advmod head 373 ; advmod ; Once again
FixedAdvP : Adv -> Adv -> AdvP ; -- head fixed 92 ; advmod ; Of course
DetNPrep : Det -> NN -> PP -> NP ; -- det head nmod 346 ; obj ; any names designers
-- Det -> NN -> PP -> NP ; -- det head nmod 176 ; nsubj ; any sort management
-- Det -> NN -> PP -> NP ; -- det head nmod 56 ; obj ; the plunge Stuart
-- Det -> NN -> PP -> NP ; -- det head nmod 42 ; nsubj ; The employees Sear's
PrepDetAdjN : Prep -> Det -> AP -> NN -> PP ; -- case det amod head 346 ; obl ; of that unique food
-- Prep -> Det -> AP -> NN -> PP ; -- case det amod head 187 ; nmod ; of the correct tires
AdAP : Adv -> A -> AP ; -- advmod head 316 ; amod ; most important
-- Adv -> AP -> AP ; -- advmod head 42 ; xcomp ; terribly apologetic
PrepDetNPrep : Prep -> Det -> NN -> PP -> PP ; -- case det head nmod 310 ; obl ; by any stretch imagination
-- Prep -> Det -> NN -> PP -> PP ; -- case det head nmod 140 ; nmod ; about the quality work
-- Prep -> Det -> NN -> PP -> PP ; -- case det head nmod 55 ; obl ; for any restaurant Zahav
SgAdjNNP : AP -> NN -> NNP ; -- amod head 331 ; 
PlAdjNNP : AP -> NN -> NNP ; -- amod head 110 ;
-- AdjN : AP -> NN -> NP ; -- amod head =306 ; obj ; NICER owners
-- AP -> NN -> NP ; -- amod head =135 ; nsubj ; other brides
---- GenPN : PN -> Poss ; -- head case 280 ; nmod:poss ; Stuart 's ; PART
CompPN : PN -> PN -> NP ; -- compound head 276 ; compound ; RADISON WARWICK
-- PN -> PN -> NP ; -- compound head 66 ; nsubj ; Wedding Gallery
FlatPN : PN -> PN -> NP ; -- head flat 103 ; root ; Alan Grissom
-- PN -> PN -> NP ; -- head flat 153 ; nsubj ; Michael Chestney
InfComplV2 : V -> NP -> VPP ; -- head obj 275 ; xcomp ; to take approach ; PART->mark
-- PART -> V2 -> NP -> VP ; -- mark head obj 181 ; advcl ; to replace tire
-- PART -> V2 -> NP -> VP ; -- mark head obj 88 ; acl ; to fix fence
-- PART -> V2 -> NP -> VP ; -- mark head obj 67 ; xcomp ; to assist me
Conj2AP : AP -> APConj -> AP ; -- head conj 262 ;
ConjAPConj : Conj -> AP -> APConj ; -- cc head ;
-- Conj -> AP -> AP ; -- cc head 262 ; conj ; and friendly
SgPrepAdjN : Prep -> AP -> NN -> PP ; -- case amod head 345 ;
PlPrepAdjN : Prep -> AP -> NN -> PP ; -- case amod head 115 ;
-- Prep -> AP -> NN -> PP ; -- case amod head =257 ; nmod ; of chewy fat 
-- Prep -> AP -> NN -> PP ; -- case amod head =208 ; obl ; at other establishments
CompoundN : N -> N -> NN ; -- compound head 250 ; compound ; dining room
-- NN -> NN -> NP ; -- compound head 128 ; obj ; mildew problems
-- NN -> NN -> NP ; -- compound head 74 ; nsubj ; minute price
--- NN -> NN -> NN ; -- head conj 68 ; compound ; designer dress
DetCompoundN : Det -> NN -> NN -> NP ; -- det compound head 236 ; obj ; the service quote
-- Det -> NN -> NN -> NP ; -- det compound head 136 ; nsubj ; the chocolate semifreddo
PrepDetCompoundN : Prep -> Det -> NN -> NN -> PP ; -- case det compound head 233 ; obl ; with no wheel lock
-- Prep -> Det -> NN -> NN -> PP ; -- case det compound head 133 ; nmod ; of the sales ladies
---- AP -> NN -> NN ; -- amod head 184 ; compound ; decent portion
---- AP -> NN -> NN ; -- amod head 67 ; obl:tmod ; several times
Conj2S : S -> SConj -> S ; -- head conj 100 ; ----
ConjSConj : Conj -> S -> SConj ; -- cc head 800 ; ----
--- Conj -> VP -> NP -> VP ; -- cc head obj 184 ; conj ; and replace tires
CommaNPComma : NP -> NPComma ; -- head 800 ;
Conj3NP : NP -> NPComma -> NPConj -> NP ; -- head conj conj 294 ;
-- Punct -> NN -> NN ; -- punct head =176 ; conj ; , decor
-- Punct -> PN -> PN ; -- punct head =118 ; conj ; , Boyles
DetNinfVP : Det -> NN -> VPP -> NP ; -- det head acl 173 ; obj ; another attempt get
DetNppartVP : Det -> NN -> V2 -> NP ; -- det head acl:relcl 110 ; obj ; the fence installed
-- Det -> NN -> VP -> NP ; -- det head acl:relcl 63 ; nsubj ; another dress loved
----DetNNingVP : Det -> NN -> VP -> NP ; -- det head acl 59 ; nsubj ; the girl helping
ComplPrepVP : VP -> PP -> VP ; -- head obl 171 ; acl ; received location
-- VP -> PP -> VP ; -- head obl 61 ; acl ; run Tina
CardTimePP : Card -> NN -> PP ; -- nummod head 153 ; nmod:tmod ; 610 AD
---- Card -> NN -> NN ; -- nummod head 85 ; obl:npmod ; ten minutes
---- Card -> NN -> NN ; -- nummod head 58 ; compound ; 20 hour
-- Card -> NN -> NN ; -- nummod head 40 ; obl:tmod ; 3 times
PrepCompN : Prep -> NN -> NN -> PP ; -- case compound head 150 ; nmod ; of dog introductions
-- Prep -> NN -> NN -> PP ; -- case compound head 118 ; obl ; about minute rates
-- Prep -> NN -> NN -> PP ; -- case head conj 88 ; nmod ; of designers dresses
-- Prep -> NN -> NN -> PP ; -- case head conj 53 ; obl ; in service quality
PrepCompPN : Prep -> PN -> PN -> PP ; -- case compound head 148 ; nmod ; in Rittenhouse Square
-- Prep -> PN -> PN -> PP ; -- case compound head 94 ; obl ; from Second Home
-- Prep -> PN -> PN -> PP ; -- case head conj 54 ; nmod ; of will grace
-- Prep -> PN -> PN -> PP ; -- case head flat 52 ; obl ; to San Antonio
-- Prep -> PN -> PN -> PP ; -- case head flat 48 ; nmod ; of San Antonio
PrepCardN : Prep -> Card -> NN -> PP ; -- case nummod head 147 ; obl ; for 3 days
-- Prep -> Card -> NN -> PP ; -- case nummod head 50 ; nmod ; to 10 AM
---- Conj -> VP -> VP ; -- cc head 145 ; conj ; and try
---- NoPronRelV2 : NP -> V -> VP ; -- nsubj head 140 ; acl:relcl ; she is
UttN : NN -> Punct -> Utt ; -- head punct 134 ; root ; Help ?
DateUtt : Card -> NN -> Utt ; -- head nmod:tmod 133 ; root ; 11/29/2000 AM
UttPredVS : NP -> V -> S -> Punct -> Utt ; -- nsubj head ccomp punct 132 ; root ; I guess tells .
---- NP -> VP -> VP -> Punct -> VP ; -- nsubj head ccomp punct 90 ; root ; thing is have .
UttPredVV : NP -> V -> VPP -> Punct -> Utt ; -- nsubj head xcomp punct 71 ; root ; They need update .
UttCardN : Card -> Punct -> NN -> Punct -> Utt ; -- head punct appos punct 129 ; root ; 5 - Number .
DetPN : Det -> PN -> NP ; -- det head 127 ; nsubj ; that Warwick
-- Det -> PN -> NP ; -- det head 49 ; obj ; a nissan
NNPPrep : NNP -> PP -> NP ; -- head nmod 126 ; obj ; grease wheel
-- NN -> PP -> NP ; -- head nmod 43 ; nsubj ; Security hotel
UttPN : PN -> Punct -> Utt ; -- head punct 126 ; root ; Manson ?
PrepNNPPrep : Prep -> NNP -> PP -> PP ; -- case head nmod 125 ; obl ; with crap hair
-- PrepNNP : Prep -> NN -> PP -> PP ; -- case head nmod 89 ; nmod ; with directions room
PrepCard : Prep -> Card -> PP ; -- case head 124 ; obl ; circa 7
-- Prep -> Card -> PP ; -- case head 120 ; nmod ; o 1998
SubjPredObj : Subj -> NP -> V -> NP -> AdvCl ; -- mark nsubj head obj 123 ; advcl ; until they ordered dress
UttPredObj : NP -> V -> NP -> Punct -> Utt ; -- nsubj head obj punct 119 ; root ; They had room .
-- NP -> VP -> NP -> Punct -> VP ; -- nsubj head obj punct 54 ; root ; shops have owners .

---- Prep -> Prep -> Prep ; -- head fixed 116 ; case ; due to
-- Subj -> VP -> NP -> VP ; -- mark head obj 113 ; advcl ; after checking tire
-- Subj -> VP -> NP -> VP ; -- mark head obj 58 ; acl ; of canceling membership
-- PART -> VP -> VP ; -- mark head 110 ; xcomp ; to do
-- PART -> VP -> VP ; -- mark head 51 ; acl ; to hide
-- Conj -> AP -> NN -> NN ; -- cc amod head 104 ; conj ; and lighter fare
-- Conj -> NN -> NN -> NN ; -- cc compound head 102 ; conj ; and check card
-- Det -> AP -> NN -> PP -> NP ; -- det amod head nmod 95 ; obj ; some serious doubts quality
-- Det -> AP -> NN -> PP -> NP ; -- det amod head nmod 53 ; nsubj ; an obvious problem staff
-- AP -> AP -> AP ; -- head conj 93 ; amod ; long bad
-- NP -> VP -> NP -> VP ; -- nsubj head obj 92 ; acl:relcl ; which had problems
-- Prep -> Det -> PN -> PP ; -- case det head 91 ; nmod ; at this Sear's
-- Prep -> Det -> PN -> PP ; -- case det head 80 ; obl ; on a Sunday
-- Adv -> PP -> Adv ; -- head obl 89 ; advmod ; Aside mystery
-- Card -> NN -> NP ; -- nummod head 89 ; obj ; 20 minutes
-- Det -> NN -> PART -> NN ; -- det head case 88 ; nmod:poss ; the club 's
-- VP -> NP -> VP ; -- head obj 85 ; acl ; making claims
-- VP -> NP -> VP ; -- head obj 59 ; xcomp ; installing fence
-- VP -> NP -> VP ; -- head obj 49 ; advcl ; driving SUV
-- Card -> Card -> Card ; -- compound head 83 ; nummod ; two million
-- Prep -> Det -> NN -> VP -> PP ; -- case det head acl:relcl 83 ; obl ; with the gown wear
-- Prep -> Det -> NN -> VP -> PP ; -- case det head acl 61 ; obl ; for a place take
-- Prep -> Det -> NN -> VP -> PP ; -- case det head acl:relcl 56 ; nmod ; of the one comparing
-- Pron -> AP -> NN -> NP ; -- nmod:poss amod head 80 ; obj ; your exact address
-- Pron -> AP -> NN -> NP ; -- nmod:poss amod head 43 ; nsubj ; My first visit
-- Prep -> Det -> PN -> NN -> PP ; -- case det compound head 78 ; obl ; from the Squirrels stadium
-- Prep -> Det -> PN -> NN -> PP ; -- case det compound head 62 ; nmod ; in the San area
InfVPP : V -> PP -> VPP ; -- head obl 78 ; xcomp ; to get work
-- Conj -> Pron -> Pron ; -- cc head 77 ; conj ; and I
-- Prep -> Det -> PN -> PN -> PP ; -- case det compound head 76 ; nmod ; of The Laundry Tub
-- Prep -> Det -> PN -> PN -> PP ; -- case det compound head 51 ; obl ; for a Mini Cooper
-- NN -> Adv -> Adv ; -- obl:npmod head 75 ; advmod ; year ago
-- Adv -> Card -> Card ; -- advmod head 74 ; nummod ; about 20
-- Prep -> Det -> AP -> NN -> PP -> PP ; -- case det amod head nmod 70 ; obl ; on the important day life
-- Prep -> Pron -> AP -> NN -> PP ; -- case nmod:poss amod head 70 ; obl ; by their own seamstresses
-- NN -> PP -> NN ; -- head nmod 69 ; appos ; Number states
-- SYM -> Card -> PP ; -- case head 66 ; nmod ; - 10
-- Conj -> Adv -> Adv ; -- cc head 62 ; conj ; and down
-- Subj -> NP -> VP -> VP ; -- mark nsubj head 62 ; advcl ; than I expected
-- AP -> NN -> PP -> NP ; -- amod head nmod 60 ; obj ; few place area
-- Prep -> AP -> PP ; -- case head 60 ; obl ; for more
-- PART -> VP -> NP -> PP -> VP ; -- mark head obj obl 60 ; xcomp ; to pull car shop
-- X -> Punct -> Card ; -- head punct 59 ; nummod ; b )
-- VP -> NP -> Punct -> VP ; -- head obj punct 58 ; root ; Imagine hotel .
-- Punct -> AP -> AP ; -- punct head 55 ; conj ; , knowledgeable
-- Pron -> NN -> NN -> NP ; -- nmod:poss compound head 55 ; obj ; my wheel lock
-- Pron -> NN -> NN -> NP ; -- nmod:poss compound head 48 ; nsubj ; my payment structure
-- Conj -> VP -> PP -> VP ; -- cc head obl 55 ; conj ; and argue customer
-- AP -> NN -> Punct -> NN ; -- amod head punct 54 ; root ; AWFUL SERVICE !
-- PN -> Card -> PN ; -- head nummod 54 ; appos ; November 22
UttAuxPredObj : NP -> V -> NP -> Punct -> Utt ; -- nsubj head obj punct 53 ; root ; We were having problem .
-- Card -> PP -> Card ; -- head nmod 52 ; nummod ; 5 10
-- Pron -> VP -> NP ; -- head acl:relcl 52 ; obj ; anyone knew
-- Prep -> Adv -> PP ; -- case head 52 ; obl ; on here
-- Conj -> VP -> VP -> VP ; -- cc head xcomp 52 ; conj ; and left saying
-- Punct -> AP -> NN -> NN ; -- punct amod head 51 ; conj ; , tasty lamb
-- Prep -> PN -> Card -> PP ; -- case head nummod 50 ; obl ; in August 2008
-- Prep -> Pron -> NN -> NN -> PP ; -- case nmod:poss compound head 50 ; obl ; with our dining experience
-- Conj -> Pron -> NN -> NN ; -- cc nmod:poss head 48 ; conj ; and my son
-- NP -> AUX -> VP -> VP -> Punct -> VP ; -- nsubj aux head xcomp punct 48 ; root ; He was going operate .
UttInterj : Interj -> Punct -> Utt ; -- head punct 47 ; root ; YES !
UttExist : There -> Be -> NP -> Punct -> Utt ; -- expl head nsubj punct 47 ; root ; There are people !!!!!
-- Pron -> NN -> PART -> NN ; -- nmod:poss head case 46 ; nmod:poss ; their employees '
-- Det -> AP -> NN -> NN -> NP ; -- det amod compound head 46 ; obj ; some serious practice issues
-- Det -> PN -> PN -> NP ; -- det compound head 46 ; nsubj ; The Laundry Tub
-- SYM -> Card -> Punct -> NN -> Punct -> SYM ; -- head nummod punct appos punct 46 ; root ; $ m - Amount .
-- PART -> VP -> NP -> VP -> VP ; -- mark head obj advcl 46 ; xcomp ; to take time fix
-- Subj -> NP -> VP -> VP -> VP ; -- mark nsubj head xcomp 46 ; advcl ; since I had complete
-- Conj -> Det -> NN -> NN ; -- cc det head 45 ; conj ; and a half
-- Punct -> NN -> NN -> NN ; -- punct compound head 45 ; conj ; , tea backs
-- Adv -> Adv -> PP ; -- case head 45 ; nmod ; at least
-- NP -> AUX -> VP -> VP ; -- nsubj aux head 45 ; acl:relcl ; they 'd used
-- Det -> PN -> NN -> NP ; -- det compound head 44 ; obj ; this Midas store
-- PART -> VP -> VP -> VP ; -- mark head ccomp 44 ; xcomp ; to realize going
-- Conj -> PN -> PN -> PN ; -- cc compound head 43 ; conj ; and G&G Automotive
-- NP -> VP -> NP -> VP -> Punct -> VP ; -- nsubj head obj conj punct 43 ; root ; I called number listened .
-- AP -> PP -> AP ; -- head obl 42 ; amod ; better folks
-- Det -> AP -> NN -> NN ; -- det amod head 42 ; obl:tmod ; The next day
-- Conj -> VP -> NP -> PP -> VP ; -- cc head obj obl 42 ; conj ; and replace valve stenosis
-- NP -> VP -> VP -> VP ; -- nsubj head xcomp 42 ; acl:relcl ; you have do
-- Adv -> VP -> Adv ; -- head advcl 41 ; advmod ; incredibly had
-- NN -> PART -> NN ; -- head case 41 ; nmod:poss ; doctor s
-- NN -> VP -> NP ; -- head acl 41 ; obj ; claims going
-- AP -> Punct -> NN -> NN ; -- amod punct head 40 ; compound ; next - door
-- Conj -> Card -> Card ; -- cc head 40 ; conj ; or 5
-- PN -> PN -> PART -> PN ; -- head flat case 40 ; nmod:poss ; Ronald Reagan 's
-- NP -> VP -> PP -> VP ; -- nsubj head obl 40 ; acl:relcl ; he put car
-- VP -> NP -> VP -> Punct -> VP ; -- head obj xcomp punct 40 ; root ; Let us know .
-- Subj -> VP -> PP -> VP ; -- mark head obl 39 ; advcl ; as dealing bull
-- Det -> AP -> CN -> VP -> NP ; -- det amod head acl 37 ; obj ; a better job training
-- Det -> AP -> CN -> VP -> NP ; -- det amod head acl:relcl 32 ; nsubj ; The worst thing happen
-- Det -> AP -> CN -> VP -> NP ; -- det amod head acl:relcl 30 ; obj ; the particular tire used
-- Det -> AP -> CN -> VP -> NP ; -- det amod head acl 25 ; nsubj ; The other person working
-- Prep -> PN -> CN -> PP ; -- case compound head 37 ; nmod ; of Florida residents
-- Prep -> PN -> CN -> PP ; -- case compound head 31 ; obl ; for Eve dinner
-- Prep -> PN -> CN -> PP ; -- case nmod:poss head 25 ; nmod ; like Hormel chili
-- PART -> VP -> Adv -> VP ; -- mark head advmod 37 ; xcomp ; to fall off
-- Adv -> VP -> VP ; -- advmod head 36 ; amod ; privatly owned
-- CN -> AP -> AP ; -- obl:npmod head 35 ; amod ; year old
-- PN -> Punct -> PN -> PN ; -- compound punct head 35 ; compound ; Mom / Pop
UttPredAP : NP -> AP -> Punct -> Utt ; -- nsubj head punct 34 ; root ; service is fast .
-- Card -> Punct -> CN -> CN ; -- nummod punct head 34 ; compound ; three - person
-- Pron -> CN -> PP -> NP ; -- nmod:poss head nmod 34 ; obj ; his lack professionalism
-- Prep -> CN -> VP -> PP ; -- case head acl:relcl 34 ; nmod ; of work performed
-- Prep -> CN -> VP -> PP ; -- case head acl 32 ; nmod ; of tips style
-- Prep -> CN -> VP -> PP ; -- case head acl 20 ; obl ; with energy burn
-- Punct -> VP -> NP -> VP ; -- punct head obj 34 ; conj ; , found one
-- Conj -> Det -> CN -> PP -> CN ; -- cc det head nmod 33 ; conj ; and a couple accessories
-- Conj -> NP -> VP -> VP -> VP ; -- cc nsubj head ccomp 33 ; conj ; and she suggested go
-- Conj -> NP -> VP -> VP -> VP ; -- cc nsubj head xcomp 22 ; conj ; and I have say
InfComplS : V -> S -> VPP ; -- head ccomp 33 ; xcomp ; know overcharged
InfComplAdvCl : V -> AdvCl -> VPP ; -- head advcl 32 ; xcomp ; know treat
InfComplVPP : V -> VPP -> VPP ; -- head xcomp 23 ; acl ; trying get
-- AP -> Prep -> AP ; -- head fixed 32 ; case ; such as
-- PN -> CN -> CN ; -- compound head 32 ; compound ; NYC style
-- AP -> CN -> CN -> NP ; -- amod compound head 32 ; obj ; different delivery dates
-- Det -> AP -> NP ; -- det head 32 ; obj ; the later
-- Det -> VP -> CN -> NP ; -- det amod head 32 ; obj ; a shredded tire
-- PN -> CN -> NP ; -- nmod:poss head 32 ; nsubj ; Goose prices
-- PN -> CN -> NP ; -- compound head 22 ; nsubj ; Disney cruise
-- PN -> CN -> NP ; -- nmod:poss head 20 ; obj ; Liberty pools
-- Subj -> VP -> VP ; -- mark head 32 ; advcl ; by operating
-- Adv -> AUX -> NP -> Punct -> Adv ; -- head cop nsubj punct 31 ; root ; When is time ?
-- Conj -> Det -> AP -> CN -> CN ; -- cc det amod head 31 ; conj ; and a few falsehoods
-- Det -> CN -> CN -> CN ; -- det compound head 31 ; nsubj:pass ; The moving company
-- Det -> PP -> NP ; -- head nmod 31 ; obj ; some dresses
-- Det -> PP -> NP ; -- head nmod 25 ; nsubj ; all comments
-- Prep -> Det -> VP -> CN -> PP ; -- case det amod head 31 ; obl ; on a based software
-- AP -> VP -> AP ; -- head ccomp 30 ; xcomp ; sure get
-- AP -> VP -> AP ; -- head advcl 23 ; amod ; necessary attach
-- CN -> Punct -> CN -> CN ; -- compound punct head 30 ; compound ; sink / stove
-- Prep -> AP -> CN -> CN -> PP ; -- case amod compound head 30 ; nmod ; with dual shower heads
-- Prep -> AP -> CN -> CN -> PP ; -- case amod head conj 24 ; nmod ; about common courtesy service
-- Prep -> AP -> CN -> CN -> PP ; -- case amod head conj 23 ; obl ; after much yelling cussing
-- Card -> Punct -> CN -> Punct -> CN ; -- appos punct head punct 29 ; list ; 713-646-3393 ( Fax )
-- Punct -> PN -> Punct -> PN ; -- punct head punct 29 ; appos ; ' Seinfeld '
-- Prep -> Det -> AP -> CN -> CN -> PP ; -- case det amod compound head 29 ; obl ; to the same recording loop
-- Prep -> Det -> AP -> CN -> CN -> PP ; -- case det amod compound head 28 ; nmod ; with the same room key
-- Pron -> Prep -> PP ; -- head case 29 ; obl ; what about
-- Conj -> CN -> PP -> CN ; -- cc head nmod 28 ; conj ; and chest drawers
-- AP -> CN -> VP -> NP ; -- amod head acl 28 ; obj ; enough forkfuls know
-- Prep -> Det -> AP -> AP -> CN -> PP ; -- case det amod amod head 28 ; obl ; After a good few minutes
-- AUX -> NP -> VP -> NP -> Punct -> VP ; -- aux nsubj head obj punct 28 ; root ; Do we need one ?
-- NP -> AUX -> VP -> NP -> VP ; -- nsubj aux head obj 28 ; acl:relcl ; someone would open items
-- VP -> VP -> Punct -> VP ; -- head ccomp punct 28 ; root ; hope helps !!
-- Conj -> Adv -> AP -> AP ; -- cc advmod head 27 ; conj ; and more TRANSPARENT
-- AP -> Prep -> Adv ; -- head fixed 27 ; advmod ; less than
-- AP -> AP -> CN -> NP ; -- amod amod head 27 ; obj ; pure delicious extract
-- Det -> AP -> AP -> CN -> NP ; -- det amod amod head 27 ; obj ; the other rear tire
-- Prep -> Det -> PN -> PN -> PN -> PP ; -- case det compound compound head 27 ; nmod ; from the Noble driving school
-- Subj -> Subj -> Subj ; -- head fixed 27 ; mark ; so that
-- Conj -> VP -> Adv -> VP ; -- cc head advmod 27 ; conj ; and sat promptly
-- Subj -> NP -> VP -> PP -> VP ; -- mark nsubj head obl 27 ; advcl ; if I paid cash
-- Punct -> X -> X ; -- punct head 27 ; conj ; , etc
-- AP -> Punct -> AP ; -- head punct 26 ; root ; AMAZING !
-- Det -> CN -> PP -> CN ; -- det head nmod 26 ; nsubj:pass ; The range activities
-- Punct -> Conj -> CN -> CN ; -- punct cc head 26 ; conj ; , or pork
-- Det -> CN -> CN -> PP -> NP ; -- det compound head nmod 26 ; obj ; the designer name one
-- Punct -> PN -> PN -> PN ; -- punct compound head 26 ; conj ; , U. Penn
-- Punct -> PN -> PN -> PN ; -- punct head flat 22 ; conj ; , Eva Marie
-- PART -> AUX -> VP -> VP ; -- mark aux:pass head 26 ; xcomp ; to be replaced
-- Punct -> Conj -> VP -> NP -> VP ; -- punct cc head obj 26 ; conj ; , but had combo
-- Subj -> AP -> AP ; -- mark head 25 ; advcl ; than necessary
-- CN -> Card -> CN ; -- head nummod 25 ; compound ; number 1
-- Pron -> CN -> CN ; -- nmod:poss head 25 ; nsubj:pass ; my car
-- Det -> PN -> PART -> PN ; -- det head case 25 ; nmod:poss ; the Liberty 's
-- Prep -> Det -> AP -> PP ; -- case det head 25 ; obl ; on the 3rd
-- Prep -> Det -> AP -> PP ; -- case det head 20 ; nmod ; of the above
-- Card -> Punct -> Punct ; -- nummod head 25 ; punct ; 1 )
-- Subj -> NP -> AUX -> VP -> NP -> VP ; -- mark nsubj aux head obj 25 ; advcl ; if they could copy piece
-- Adv -> Adv -> VP -> Adv ; -- advmod head advcl 24 ; advmod ; so far say
-- CN -> Punct -> Card -> CN ; -- head punct appos 24 ; list ; SMS : 919819602175
-- Pron -> Det -> NP ; -- head det 24 ; nsubj ; They both
-- SYM -> Card -> SYM ; -- head nummod 24 ; compound ; $ 99
-- Conj -> NP -> VP -> NP -> VP ; -- cc nsubj head obj 24 ; conj ; and we took drive
-- CN -> PP -> Punct -> CN ; -- head nmod punct 23 ; root ; Thanks deal !
-- Pron -> AP -> NP ; -- head amod 23 ; obj ; something fantastic
-- Adv -> Prep -> Det -> CN -> PP ; -- advmod case det head 23 ; obl ; even in the economy
-- Prep -> AP -> AP -> CN -> PP ; -- case amod amod head 23 ; nmod ; at most cheap hotels
-- Prep -> AP -> CN -> PP -> PP ; -- case amod head nmod 23 ; nmod ; of many kinds things
-- Subj -> Pron -> VP -> PP ; -- case head acl:relcl 23 ; obl ; than what wanted
-- NP -> VP -> PP -> Punct -> VP ; -- nsubj head obl punct 23 ; root ; apartment belonged gang .
-- Subj -> NP -> AUX -> VP -> VP ; -- mark nsubj aux head 23 ; advcl ; as I was leaving
-- X -> X -> X ; -- head goeswith 23 ; root ; Wayne Perry@ENRON_DEVELOPMENT
-- Adv -> Adv -> Prep -> Adv ; -- head fixed fixed 22 ; cc ; as well as
-- Conj -> Det -> CN -> CN -> CN ; -- cc det compound head 22 ; conj ; and some wedding cards
-- Prep -> VP -> CN -> PP ; -- case amod head 22 ; nmod ; of forced air
-- Adv -> NP -> VP -> NP -> VP ; -- mark nsubj head obj 22 ; advcl ; when I received bill
-- NP -> AUX -> VP -> PP -> VP ; -- nsubj aux head obl 22 ; acl:relcl ; I will wear day
-- NP -> VP -> Adv -> VP ; -- nsubj head advmod 22 ; acl:relcl ; I had on
-- Punct -> VP -> VP ; -- punct head 22 ; conj ; , eat
-- Subj -> NP -> VP -> Adv -> VP ; -- mark nsubj head advmod 22 ; advcl ; If you go later
-- Subj -> Pron -> VP -> NP -> VP ; -- mark expl head nsubj 22 ; advcl ; as there are lot
-- Det -> AP -> AP ; -- det head 21 ; obl:npmod ; a little
-- NP -> AUX -> AP -> AP ; -- nsubj cop head 21 ; ccomp ; i am inoperable
-- Subj -> NP -> AUX -> AUX ; -- mark nsubj head 21 ; advcl ; as we can
-- CN -> VP -> CN ; -- head acl:relcl 21 ; appos ; Amount expects
-- CN -> PN -> PN -> NP ; -- compound head flat 21 ; nsubj ; Attorney Peter Barrett
-- NP -> AUX -> VP -> NP -> VP -> Punct -> VP ; -- nsubj aux head obj conj punct 21 ; root ; I 've toured place impressed .
-- NP -> AUX -> VP -> VP -> VP ; -- nsubj aux head xcomp 21 ; ccomp ; we were going feel
-- NP -> NP -> VP -> VP ; -- obj nsubj head 21 ; acl:relcl ; that I loved
-- Subj -> VP -> NP -> PP -> VP ; -- mark head obj obl 21 ; advcl ; by leaving can trash
-- NP -> AUX -> Adv -> AP -> Punct -> AP ; -- nsubj cop advmod head punct 20 ; root ; apartment was usually quiet .
-- CN -> PP -> VP -> CN ; -- head nmod acl:relcl 20 ; appos ; Percentage voters describe
-- Prep -> Det -> PN -> PP -> PP ; -- case det head nmod 20 ; nmod ; on the Mariner Seas
-- Prep -> Pron -> CN -> PP -> PP ; -- case nmod:poss head nmod 20 ; obl ; with your expectations place
-- NP -> AUX -> Adv -> VP -> NP -> Punct -> VP ; -- nsubj aux advmod head obj punct 20 ; root ; I have never had problems .
-- NP -> VP -> AP -> Punct -> VP ; -- nsubj head ccomp punct 20 ; root ; They said unable .
-- PART -> VP -> NP -> Adv -> VP ; -- mark head obj advmod 20 ; xcomp ; to board dogs here
-- VP -> AUX -> CN -> Punct -> VP ; -- head aux:pass nsubj:pass punct 20 ; root ; Attached is Shippers .
-- Prep -> Prep -> Det -> CN -> PP ; -- case case det head 19 ; obl ; out of the hotel
-- AUX -> NP -> VP -> VP -> Punct -> VP ; -- aux nsubj head xcomp punct 19 ; root ; was I supposed do ,
-- Adv -> Punct -> VP -> VP ; -- advmod punct head 19 ; amod ; well - funded
UttPassObl : NP -> V -> PP -> Punct -> Utt ; -- nsubj:pass *aux:pass head obl punct 19 ; root ; package were left room .
-- NP -> Adv -> VP -> NP -> Punct -> VP ; -- nsubj advmod head obj punct 19 ; root ; We also love Rolls .
-- Subj -> NP -> AUX -> VP -> VP -> VP ; -- mark nsubj aux head xcomp 19 ; advcl ; like they were going fall

}