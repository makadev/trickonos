{?
  function cntnum;
    var x,y,z;
    y := 1;
    z := 1000000;

    x := y;
    repeat
      x := x + y;
    until x > z;
  end;
  
  cntnum();
  
?}
