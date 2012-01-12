{?
  
  /* trimleft not much to do, basic trim implementation is fpc rtl function so
     mostly invocation check */  
  ASSERT '   '.trimleft() = '';
  ASSERT ' abc '.trimleft() = 'abc ';
  ASSERT "\tabc\t\n".trimleft() = "\tabc\t\n";
  ASSERT ''.trimleft() = '';

?}