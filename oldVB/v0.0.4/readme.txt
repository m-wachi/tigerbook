command line:
  $ sml sources.cm

then smlnj interpriter
  - Oldvb01.parse "test01.vb";
  or
  - Oldvb01.doLex "test01.vb";
  or   
  - val a = Oldvb01.parse "test01.vb";
  - Oldvb01.printAll a;
  or
  - val a = Oldvb01.parse "test01.vb";
  - Oldvb2Cs01.printConvAll a;
  or
  - Oldvb2Cs01.translate01 "test01.vb";
 

