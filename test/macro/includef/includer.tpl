{? 
  echoln 'inc/use 1';
  USE      'use_1.use';
  INCLUDE  'incl_1.inc';

  echoln 'mode changing reinclude 2';
  INCLUDE  'use_1.use';
  USE      'incl_1.inc';

  HALT 0;
?}