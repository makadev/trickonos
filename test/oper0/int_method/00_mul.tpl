{? 
  
  /* check sign by factor1.sign xor factor2.sign */
  ASSERT 7 * 3 = 21;
  ASSERT 7 * -3 = -21;
  ASSERT -7 * 3 = -21;
  ASSERT -7 * -3 = 21;
  
  if (100*10) <> 1000 then
    Halt(1);
  end;

  if (2147483647*2+1) <> -1 then
    Halt(1);
  end;
?}