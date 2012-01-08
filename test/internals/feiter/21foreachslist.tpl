{?

  y := [1,2,3,4,5];

  xa := 1;
  foreach a in y do
    ASSERT xa = a;
    xb := 1;
    foreach b in y do
      ASSERT xa = a;
      ASSERT xb = b;
      xb := b+1;
    end;
    xa := xa+1;
  end;
  
  ASSERT xa = 6;
  ASSERT xb = 6;
?}