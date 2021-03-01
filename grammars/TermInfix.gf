concrete TermInfix of Term = {

lincat
  Term, Factor, Atom, TOper, FOper = Str ;

lin
  OpTerm op a b = a ++ op ++ b ;
  OpFactor op a b = a ++ op ++ b ;
  FactorTerm a = a ;
  AtomFactor a = a ;

  parenth t = "(" ++ t ++ ")" ;

  PlusOp = "+" ;
  MinusOp = "-" ;
  TimesOp = "*" ;

  x = "x" ;
  

}