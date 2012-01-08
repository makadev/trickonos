{? 

function name(notpassed:=9999,varargs);
begin
  if varargs <> nil then
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