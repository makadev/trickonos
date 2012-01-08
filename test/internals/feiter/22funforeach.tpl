{?

  function println(varargs:=[]);
  begin
    y := 1;
    foreach x in varargs do
      ASSERT x=y;
      y := y + 1;
    end;
  end;

  println(1,2,3,4,5,6);
  ASSERT y=7;

?}