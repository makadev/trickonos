{?
  x := 1;
  d := {};
  cnt := 100000;
  i := 1;
  
  writeln "filling Dict";
  
  repeat
    d[ x.tostr() ] := x;
    x := x + i;
  until x > cnt;

  writeln "done filling, now checking";
  
  x := 1; 
  repeat
    if d[ x.tostr() ] <> x then
      Halt(1);
    end;
    x := x + i;
  until x > cnt;

  writeln "done, all clear.";
  
?}
