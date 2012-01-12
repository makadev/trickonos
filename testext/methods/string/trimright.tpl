{?
  
  /* trimright not much to do, basic trim implementation is fpc rtl function so
     mostly invocation check */  
  ASSERT '   '.trimright() = '';
  ASSERT ' abc '.trimright() = ' abc';
  ASSERT "\tabc\t\n".trimright() = "\tabc\t\n";
  ASSERT ''.trimright() = '';

?}