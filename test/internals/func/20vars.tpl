{? 

function name;
var x,y,z;
begin
  x := 9999;
  y := z;
  z := x;
  if z <> 9999 then
    Halt(1);
  end;
  if x <> 9999 then
    Halt(1);
  end;
  if y <> nil then
    Halt(1);
  end;
end;

if name() <> nil then
  Halt(1);
end;

?}