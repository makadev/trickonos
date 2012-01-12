{?
  
  /* split on '' */  
  ASSERT ''.split('') = [''];

  /* split on nonexisting */  
  ASSERT 'a'.split('b') = ['a'];
  ASSERT 'acd'.split('b') = ['acd'];

  /* split single delim */
  ASSERT 'a'.split('') = ['a'];
  
  /* split multiple (short split algo) */
  ASSERT 'a,b,c,d,e,f'.split(',') = ['a','b','c','d','e','f'];

  /* split multiple, last is split/first is delim (short split algo) */
  ASSERT 'a,b,c,'.split(',') = ['a','b','c',''];
  ASSERT ',a,b,c'.split(',') = ['', 'a','b','c'];
  ASSERT ',a,b,c,'.split(',') = ['','a','b','c',''];

  /* split multiple, only delim (short split algo) */
  ASSERT 'aaa'.split('a') = ['','','',''];
  ASSERT 'aaaaaa'.split('aa') = ['','','',''];

  /* split multiple (long split algo) */
  ASSERT ('1abcdefghijklmnopqrstuvwxyz---'+
          '2abcdefghijklmnopqrstuvwxyz---'+
          '3abcdefghijklmnopqrstuvwxyz---'+
          '4abcdefghijklmnopqrstuvwxyz---'+
          '5abcdefghijklmnopqrstuvwxyz---'+
          '6abcdefghijklmnopqrstuvwxyz---'+
          '7abcdefghijklmnopqrstuvwxyz---'+
          '8abcdefghijklmnopqrstuvwxyz---'+
          '9abcdefghijklmnopqrstuvwxyz---'+
          '0abcdefghijklmnopqrstuvwxyz').split('---') = 
         ['1abcdefghijklmnopqrstuvwxyz',
          '2abcdefghijklmnopqrstuvwxyz',
          '3abcdefghijklmnopqrstuvwxyz',
          '4abcdefghijklmnopqrstuvwxyz',
          '5abcdefghijklmnopqrstuvwxyz',
          '6abcdefghijklmnopqrstuvwxyz',
          '7abcdefghijklmnopqrstuvwxyz',
          '8abcdefghijklmnopqrstuvwxyz',
          '9abcdefghijklmnopqrstuvwxyz',
          '0abcdefghijklmnopqrstuvwxyz'];

  /* split multiple, last is split/first is delim (short split algo) */
  ASSERT ('---1abcdefghijklmnopqrstuvwxyz---'+
          '2abcdefghijklmnopqrstuvwxyz---'+
          '3abcdefghijklmnopqrstuvwxyz---'+
          '4abcdefghijklmnopqrstuvwxyz---'+
          '5abcdefghijklmnopqrstuvwxyz---'+
          '6abcdefghijklmnopqrstuvwxyz---'+
          '7abcdefghijklmnopqrstuvwxyz---'+
          '8abcdefghijklmnopqrstuvwxyz---'+
          '9abcdefghijklmnopqrstuvwxyz---'+
          '0abcdefghijklmnopqrstuvwxyz').split('---') = 
         ['',
          '1abcdefghijklmnopqrstuvwxyz',
          '2abcdefghijklmnopqrstuvwxyz',
          '3abcdefghijklmnopqrstuvwxyz',
          '4abcdefghijklmnopqrstuvwxyz',
          '5abcdefghijklmnopqrstuvwxyz',
          '6abcdefghijklmnopqrstuvwxyz',
          '7abcdefghijklmnopqrstuvwxyz',
          '8abcdefghijklmnopqrstuvwxyz',
          '9abcdefghijklmnopqrstuvwxyz',
          '0abcdefghijklmnopqrstuvwxyz'];

  ASSERT ('1abcdefghijklmnopqrstuvwxyz---'+
          '2abcdefghijklmnopqrstuvwxyz---'+
          '3abcdefghijklmnopqrstuvwxyz---'+
          '4abcdefghijklmnopqrstuvwxyz---'+
          '5abcdefghijklmnopqrstuvwxyz---'+
          '6abcdefghijklmnopqrstuvwxyz---'+
          '7abcdefghijklmnopqrstuvwxyz---'+
          '8abcdefghijklmnopqrstuvwxyz---'+
          '9abcdefghijklmnopqrstuvwxyz---'+
          '0abcdefghijklmnopqrstuvwxyz---').split('---') = 
         ['1abcdefghijklmnopqrstuvwxyz',
          '2abcdefghijklmnopqrstuvwxyz',
          '3abcdefghijklmnopqrstuvwxyz',
          '4abcdefghijklmnopqrstuvwxyz',
          '5abcdefghijklmnopqrstuvwxyz',
          '6abcdefghijklmnopqrstuvwxyz',
          '7abcdefghijklmnopqrstuvwxyz',
          '8abcdefghijklmnopqrstuvwxyz',
          '9abcdefghijklmnopqrstuvwxyz',
          '0abcdefghijklmnopqrstuvwxyz',
          ''];

  ASSERT ('---1abcdefghijklmnopqrstuvwxyz---'+
          '2abcdefghijklmnopqrstuvwxyz---'+
          '3abcdefghijklmnopqrstuvwxyz---'+
          '4abcdefghijklmnopqrstuvwxyz---'+
          '5abcdefghijklmnopqrstuvwxyz---'+
          '6abcdefghijklmnopqrstuvwxyz---'+
          '7abcdefghijklmnopqrstuvwxyz---'+
          '8abcdefghijklmnopqrstuvwxyz---'+
          '9abcdefghijklmnopqrstuvwxyz---'+
          '0abcdefghijklmnopqrstuvwxyz---').split('---') = 
         ['',
          '1abcdefghijklmnopqrstuvwxyz',
          '2abcdefghijklmnopqrstuvwxyz',
          '3abcdefghijklmnopqrstuvwxyz',
          '4abcdefghijklmnopqrstuvwxyz',
          '5abcdefghijklmnopqrstuvwxyz',
          '6abcdefghijklmnopqrstuvwxyz',
          '7abcdefghijklmnopqrstuvwxyz',
          '8abcdefghijklmnopqrstuvwxyz',
          '9abcdefghijklmnopqrstuvwxyz',
          '0abcdefghijklmnopqrstuvwxyz',
          ''];
?}