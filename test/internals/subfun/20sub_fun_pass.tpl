{?

  y := 10;

  function fun_creator(x,y := nil);  // lower func
    function Result(x := x, y := y); // top func, at time of assembly, x/y are not known to top func
      ASSERT(x = nil);               // so global access code is generated -> x = nil, y = 20
      ASSERT(y = 10);                // >non closure behaviour<
      Result := 1;
    end;
  end;
 
  ASSERT fun_creator(20)() = 1;
 
?}