{? 

function name(x,notpassed:=9999,varargs:=19);
begin
  if x <> 10 then
    Halt(1);
  end;
  if varargs <> 19 then
    Halt(1);
  end;
  if notpassed <> 9999 then
    Halt(1);
  end;
  Result := notpassed;
end;

if name(10) <> 9999 then
  Halt(1);
end;

?}