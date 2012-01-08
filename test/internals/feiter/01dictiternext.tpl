{?

  x := {key1:'1',key2:'2',key3:'3'};
  y := x.Iterator();

  /* Iterate hart -> 4 next calls, 3 vals/keys */  
  if y.Next() then
    writeln y.CurrentKey;
    writeln y.CurrentValue;
  else
    Halt(1);
  end;

  if y.Next() then
    writeln y.CurrentKey;
    writeln y.CurrentValue;
  else
    Halt(1);
  end;

  if y.Next() then
    writeln y.CurrentKey;
    writeln y.CurrentValue;
  else
    Halt(1);
  end;

  if y.Next() then
    Halt(1);
  end;
  
  /* check locklift, simply try adding something */
  x.someotherkey := '999';

?}