{?

  x := [1,2,3];
  y := x.Iterator();

  /* Iterate hart -> 4 next calls, 3 vals */  
  if y.Next() then
    writeln y.Current;
  else
    Halt(1);
  end;

  if y.Next() then
    writeln y.Current;
  else
    Halt(1);
  end;

  if y.Next() then
    writeln y.Current;
  else
    Halt(1);
  end;

  if y.Next() then
    Halt(1);
  end;
  
  /* check locklift, simply try appending something */
  x.Append(4);

?}