{?
  
  /* trim not much to do, basic trim implementation is fpc rtl function so
     mostly invocation check */  
  ASSERT '   '.trim() = '';
  ASSERT ' abc '.trim() = 'abc';
  ASSERT "\tabc\t\n".trim() = "\tabc\t\n";
  ASSERT ''.trim() = '';

?}