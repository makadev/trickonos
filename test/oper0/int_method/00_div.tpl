{? 

  /* division sign by divisor.sign xor dividend.sign */
  ASSERT 7 div 3 = 2;
  ASSERT 7 div -3 = -2;
  ASSERT -7 div 3 = -2;
  ASSERT -7 div -3 = 2;

  /* check bigger dividend, smaller dividend, 0, bigger divisor */
  ASSERT 999991 div 10 = 99999;
  ASSERT 9 div 10 = 0;
  ASSERT 0 div 10 = 0;
  ASSERT 10 div 999991 = 0;

?}