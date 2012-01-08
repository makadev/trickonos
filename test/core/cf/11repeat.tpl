{? 

x := true;
repeat
  if x then
    x := false;
  else
    x := true;
  end;
until x;

if x then
  Halt(0);
else
  Halt(1);
end;

?}