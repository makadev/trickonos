{? 

function name(notpassed:=9999,varargs:=19);
begin
  if varargs <> 19 then
    Halt(1);
  end;
  if notpassed <> 9999 then
    Halt(1);
  end;
  Result := notpassed;
end;

if name() <> 9999 then
  Halt(1);
end;

?}