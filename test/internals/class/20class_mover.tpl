{?

 /* check: last method name overrides,
           and while at it.. check recursion  */

class someclass
  function method;
    ASSERT false;
  end;
   
  function method;
    if self.x < 10 then
      self.x := self.x + 1;
      self.method();
    end;
    Result := self.x;
  end;
  
  function create;
  begin
    self.x := 1;
  end;
end;

m := someclass.Create();

ASSERT m.x = 1;

ASSERT m.method() = 10;
ASSERT m.x = 10;

?}