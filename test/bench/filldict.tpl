{?
  x := 1;
  d := {};
  
  writeln "filling Dict";
  
  repeat
    d[ x.tostr() ] := x;
    x := x + 1;
  until x > 100000;

  writeln "done filling, now checking";
  
  x := 1; 
  repeat
    if d[ x.tostr() ] <> x then
      Halt(1);
    end;
    x := x + 1;
  until x > 100000;

  writeln "done, all clear.";
  
?}
