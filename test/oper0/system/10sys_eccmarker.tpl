{?
  x := nil; // well.. it doesnt hurt

  /* set ecc marker, since its dynamic we need to include a testfile */

  System.ECCMarker('<!--','-->');

  INCLUDE 'sys_ecctest.inc';
  
  /* still */

  ASSERT x;

?}