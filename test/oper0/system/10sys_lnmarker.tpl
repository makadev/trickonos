{?
  x := nil; // well.. it doesnt hurt

  /* set ln marker, since its dynamic we need to include a testfile */

  // '//' here

  System.LNMarker('#');

  INCLUDE 'sys_lntest.inc';
  
  // still '//'

  ASSERT x;

?}