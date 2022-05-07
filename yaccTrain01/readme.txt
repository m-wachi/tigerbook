1. install ml-yacc, ml-ulex
 # apt install ml-yacc
 # apt install ml-ulex

2. process calc.lex, calc.grm
 run following and end sml.
 # sml sources.cm

3. run Calc
 re-run sml with no parameter and  type sml interpreter followings.

  CM.make "$/ml-yacc-lib.cm";
  use "calc.grm.sig";
  use "calc.lex.sml";
  use "calc.grm.sml";
  use "calc.sml";
  Calc.parse ();

 then Calc run. type "12 + 3;", result will be printed.

