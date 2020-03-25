resource MiniResEng = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc ;
  Person = Per1 | Per2 | Per3 ;

  Agreement = Agr Number Person ;

  -- all forms of normal Eng verbs, although not yet used in MiniGrammar
  VForm = Inf | PresSg3 | Past | PastPart | PresPart ; 

oper
  Noun : Type = {s : Number => Str} ;

  mkNoun : Str -> Str -> Noun = \sg,pl -> {
    s = table {Sg => sg ; Pl => pl}
    } ;

  regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "s") ;

  -- smart paradigm
  smartNoun : Str -> Noun = \sg -> case sg of {
    _ + ("ay"|"ey"|"oy"|"uy") => regNoun sg ;
    x + "y"                   => mkNoun sg (x + "ies") ;
    _ + ("ch"|"sh"|"s"|"o")   => mkNoun sg (sg + "es") ;
    _                         => regNoun sg
    } ;

  Adjective : Type = {s : Str} ;

  Verb : Type = {s : VForm => Str} ;

  mkVerb : (inf,pres,past,pastpart,prespart : Str) -> Verb
    = \inf,pres,past,pastpart,prespart -> {
    s = table {
      Inf => inf ;
      PresSg3 => pres ;
      Past => past ;
      PastPart => pastpart ;
      PresPart => prespart
      }
    } ;

  regVerb : (inf : Str) -> Verb = \inf ->
    mkVerb inf (inf + "s") (inf + "ed") (inf + "ed") (inf + "ing") ;

  -- regular verbs with predictable variations
  smartVerb : Str -> Verb = \inf -> case inf of {
     pl  +  ("a"|"e"|"i"|"o"|"u") + "y" => regVerb inf ;
     cr  +  "y" =>  mkVerb inf (cr + "ies") (cr + "ied") (cr + "ied") (inf + "ing") ;
     lov + "e"  => mkVerb inf (inf + "s") (lov + "ed") (lov + "ed") (lov + "ing") ;
     kis + ("s"|"sh"|"x") => mkVerb inf (inf + "es") (inf + "ed") (inf + "ed") (inf + "ing") ;
     _ => regVerb inf
     } ;

  -- normal irregular verbs e.g. drink,drank,drunk
  irregVerb : (inf,past,pastpart : Str) -> Verb =
    \inf,past,pastpart ->
      let verb = smartVerb inf
      in mkVerb inf (verb.s ! PresSg3) past pastpart (verb.s ! PresPart) ;   

  negation : Bool -> Str = \b -> case b of {True => [] ; False => "not"} ; 

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  -- generalized verb, here just "be"
 param
   GVForm = VF VForm | PresSg1 | PresPl | PastPl ;

 oper
  GVerb : Type = {
     s : GVForm => Str ;
     isAux : Bool
     } ;

  be_GVerb : GVerb = {
     s = table {
       PresSg1 => "am" ;
       PresPl  => "are" ;
       PastPl  => "were" ;
       VF vf   => (mkVerb "be" "is" "was" "been" "being").s ! vf
       } ;
     isAux = True
     } ;

  -- in VP formation, all verbs are lifted to GVerb, but morphology doesn't need to know this
   verb2gverb : Verb -> GVerb = \v -> {s =
     table {
       PresSg1 => v.s ! Inf ;
       PresPl  => v.s ! Inf ;
       PastPl  => v.s ! Past ;
       VF vf   => v.s ! vf
       } ;
     isAux = False
     } ;

}