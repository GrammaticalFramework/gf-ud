concrete InferredEng of Inferred =
  CatEng,
  StructuralEng - [both7and_DConj, either7or_DConj, if_then_Conj, yes_Utt, no_Utt, language_title_Utt],
  LexiconEng **

open SyntaxEng, ExtendEng, (P = ParadigmsEng)
in {
-- grammar inferred by post-editing the result of
--  *UDAnalysis> testUDTypes 40 "ud/UD_English-EWT/en_ewt-ud-train.conllu"
--

lincat
  NN = SyntaxEng.N ; -- non-compound nouns
  PP = Adv ;
  NNP = NP ; -- single-N NP, sg or pl
  AdvP = Adv ; -- adverbial phrase
  AdvCl = Adv ;
  Poss = Det ; -- possessive determiner
  VPP = Adv ; ----
  Punct = {s : Str} ;
---
  There = NP ;
  Be = V ;

lin
  UseN n = n ;
  SgNNP n = mkNP n ;
  PlNNP n = mkNP aPl_Det n ;
  UsePN pn = mkNP pn ;
  UsePron pron = mkNP pron ;
  SPredObj subj v obj = mkS (mkCl subj (P.mkV2 v) obj) ;
  PositA a = mkAP a ;
  ComparA a = comparAP a ;
  periodPunct = {s = "."} ;
  fiveCard = mkCard "5" ;
---
  there_There = mkNP (P.mkPN "there") ; ----
  be_Be = P.mkV "are" "was" "been" ; ----

oper punctUtt : Utt -> {s : Str} -> Utt = \u,p -> lin Utt {s = u.s ++ p.s} ;
oper ccUtt : Utt -> Utt -> Utt = \u,v -> lin Utt {s = u.s ++ v.s} ;


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
InfComplV2 v np = lin Adv (mkUtt (mkVP (P.mkV2 v) np)) ;



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

DetNinfVP det n vpp = mkNP det (mkCN (mkCN n) vpp) ;
DetNppartVP det n v2 = mkNP (mkNP det n) v2 ;

----DetCNingVP : Det -> CN -> VP -> NP ; -- det head acl 59 ; nsubj ; the girl helping
ComplPrepVP vp pp = mkVP vp pp ;

CardTimePP card n = mkAdv P.noPrep (mkNP card n) ;
---- Card -> CN -> CN ; -- nummod head 85 ; obl:npmod ; ten minutes
---- Card -> CN -> CN ; -- nummod head 58 ; compound ; 20 hour

PrepCompN prep x y = mkAdv prep (mkNP (ExtendEng.CompoundN x y)) ;



PrepCompPN prep x y = mkAdv prep (mkNP (mkNP x) (lin Adv (mkUtt (mkNP y)))) ;




PrepCardN prep card n = mkAdv prep (mkNP card n) ;

---- Conj -> VP -> VP ; -- cc head 145 ; conj ; and try
---- NoPronRelV2 : NP -> V -> VP ; -- nsubj head 140 ; acl:relcl ; she is
UttN n punct = punctUtt (mkUtt (mkCN n)) punct ;
DateUtt card n = ccUtt (mkUtt card) (mkUtt (mkCN n)) ;
UttPredVS np v s punct = punctUtt (mkUtt (mkS (mkCl np (P.mkVS v) s))) punct ;
---- NP -> VP -> VP -> Punct -> VP ; -- nsubj head ccomp punct 90 ; root ; thing is have .
UttPredVV np v vpp punct = punctUtt (mkUtt (mkS (mkCl np (mkVP (mkVP v) vpp)))) punct ;
UttCardN card p n q = ccUtt (punctUtt (mkUtt card) p) (punctUtt (mkUtt (mkCN n)) q) ; 
----DetPN : Det -> PN -> NP ; -- det head 127 ; nsubj ; that Warwick

NNPPrep nnp pp = mkNP nnp pp ;

UttPN pn punct = punctUtt (mkUtt (mkNP pn)) punct ;
PrepNNPPrep prep nnp pp = mkAdv prep (mkNP nnp pp) ;

PrepCard prep card = mkAdv prep (mkNP (mkDet card)) ;

SubjPredObj sub subj v obj = mkAdv sub (mkS (mkCl subj (P.mkV2 v) obj)) ;
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
InfVPP v pp = lin Adv (mkUtt (mkVP (mkVP v) pp)) ;
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
UttAuxPredObj subj v obj punct = punctUtt (mkUtt (mkS anteriorAnt (mkCl subj (P.mkV2 v) obj))) punct ; ---- general tense
-- Card -> PP -> Card ; -- head nmod 52 ; nummod ; 5 10
-- Pron -> VP -> NP ; -- head acl:relcl 52 ; obj ; anyone knew
-- Prep -> Adv -> PP ; -- case head 52 ; obl ; on here
-- Conj -> VP -> VP -> VP ; -- cc head xcomp 52 ; conj ; and left saying
-- Punct -> AP -> CN -> CN ; -- punct amod head 51 ; conj ; , tasty lamb
-- Prep -> PN -> Card -> PP ; -- case head nummod 50 ; obl ; in August 2008
-- Prep -> Pron -> CN -> CN -> PP ; -- case nmod:poss compound head 50 ; obl ; with our dining experience
-- Conj -> Pron -> CN -> CN ; -- cc nmod:poss head 48 ; conj ; and my son
-- NP -> AUX -> VP -> VP -> Punct -> VP ; -- nsubj aux head xcomp punct 48 ; root ; He was going operate .
UttInterj interj punct = punctUtt (mkUtt interj) punct ;
UttExist there be np punct = punctUtt (mkUtt (mkS (mkCl there (P.mkV2 be) np))) punct ;
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
UttPredAP np ap punct = punctUtt (mkUtt (mkS (mkCl np ap))) punct ;
-- Card -> Punct -> CN -> CN ; -- nummod punct head 34 ; compound ; three - person
-- Pron -> CN -> PP -> NP ; -- nmod:poss head nmod 34 ; obj ; his lack professionalism
-- Prep -> CN -> VP -> PP ; -- case head acl:relcl 34 ; nmod ; of work performed
-- Prep -> CN -> VP -> PP ; -- case head acl 32 ; nmod ; of tips style
-- Prep -> CN -> VP -> PP ; -- case head acl 20 ; obl ; with energy burn
-- Punct -> VP -> NP -> VP ; -- punct head obj 34 ; conj ; , found one
-- Conj -> Det -> CN -> PP -> CN ; -- cc det head nmod 33 ; conj ; and a couple accessories
-- Conj -> NP -> VP -> VP -> VP ; -- cc nsubj head ccomp 33 ; conj ; and she suggested go
-- Conj -> NP -> VP -> VP -> VP ; -- cc nsubj head xcomp 22 ; conj ; and I have say
InfComplS v s = lin Adv (mkUtt (mkVP (P.mkVS v) s)) ;
InfComplAdvCl v adv = lin Adv (mkUtt (mkVP (mkVP v) adv)) ;
InfComplVPP v vpp = lin Adv (mkUtt (mkVP (mkVP v) vpp)) ;
-- VP -> VP -> VP ; -- head xcomp 23 ; acl ; trying get
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
UttPassObl subj v pp punct = punctUtt (mkUtt (mkS (mkCl subj (mkVP (passiveVP (P.mkV2 v)) pp)))) punct ;
-- NP -> Adv -> VP -> NP -> Punct -> VP ; -- nsubj advmod head obj punct 19 ; root ; We also love Rolls .
-- Subj -> NP -> AUX -> VP -> VP -> VP ; -- mark nsubj aux head xcomp 19 ; advcl ; like they were going fall

}