{? 
  /* mod sign by divisor */
  ASSERT 7 mod 3 = 1;
  ASSERT 7 mod -3 = -1;
  ASSERT -7 mod 3 = 1;
  ASSERT -7 mod -3 = -1;

  /* check bigger dividend, smaller dividend, 0, bigger divisor */
  ASSERT 999991 mod 10 = 1;
  ASSERT 9 mod 10 = 9;
  ASSERT 0 mod 10 = 0;
  ASSERT 10 mod 999991 = 10;
?}