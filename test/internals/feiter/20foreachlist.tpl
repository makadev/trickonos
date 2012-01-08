{?

  y := 1;
  foreach x in [1,2,3] do
    ASSERT x=y;
    y := y+1;
  end;

  ASSERT y=4;
?}