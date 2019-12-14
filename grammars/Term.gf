abstract Term = {

flags startcat = Term ;

cat
  Term ; Factor ; Atom ; TOper ; FOper ;

fun
  OpTerm : TOper -> Term -> Factor -> Term ;
  OpFactor : FOper -> Factor -> Atom -> Atom ;
  FactorTerm : Factor -> Term ;
  AtomFactor : Atom -> Factor ;

  parenth : Term -> Atom ;

  PlusOp : TOper ;
  MinusOp : TOper ;
  TimesOp : FOper ;

  x : Atom ;



}