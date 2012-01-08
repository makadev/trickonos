{? 

function name(notpassed:=9999);
begin
  if notpassed <> 9999 then
    Halt(1);
  end;
  Result := notpassed;
end;

if name() <> 9999 then
  Halt(1);
end;

?}