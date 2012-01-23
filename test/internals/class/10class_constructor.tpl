{?

 /* check constructor calling with args and
    constructor side self calling and
    self attribute set/get */

class someclass
  function method2(y);
  begin
    ASSERT self is Class_someclass;
    ASSERT nil = Result;
    self.f := y;
  end;

  function method(x);
    writeln 'method hit';
    self.y := x;
    Result := 10;
    self.method2('blubb');
  end;

  function Create( somearg );
    /* result = self */
    writeln 'constructor hit';
    write 'self is ';
    writeln self;
    write 'result is ';
    writeln result;
    ASSERT result = self;
    ASSERT self.method(20) = 10;
    ASSERT self.y = 20;
    self.x := somearg;
  end;  
end;

write 'instancing cls ';
writeln someclass;
m := someclass.Create(30);

ASSERT m.y = 20;
ASSERT m.x = 30;
ASSERT m.f = 'blubb';

m.z := 40;
m.y := 10;


write 'calling ';
writeln m;
m.Create(50);

ASSERT m.y = 20;
ASSERT m.x = 50;
ASSERT m.z = 40;

?}