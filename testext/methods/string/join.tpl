{?
  
  /* join empty (list) */  
  ASSERT ''.join([]) = '';
  ASSERT ''.join(['a']) = 'a';
  ASSERT ''.join(['a','b']) = 'ab';
  
  /* join delim (list) */  
  ASSERT ','.join([]) = '';
  ASSERT ','.join(['a']) = 'a';
  ASSERT ','.join(['a','b']) = 'a,b';
  
  /* join empty (vararg) */  
  ASSERT ''.join() = '';
  ASSERT ''.join('a') = 'a';
  ASSERT ''.join('a','b') = 'ab';
  
  /* join delim (vararg) */  
  ASSERT ','.join() = '';
  ASSERT ','.join('a') = 'a';
  ASSERT ','.join('a','b') = 'a,b';
?}