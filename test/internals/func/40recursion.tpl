{? 

x := 1;

function name;
begin
  x := x+1;
  if x < 100 then
    Result := name();
  else
    Result := 9999;
  end;
end;

if name() <> 9999 then
  Halt(1);
end;

?}