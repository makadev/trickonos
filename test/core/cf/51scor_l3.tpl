{? 

/* nil or true -> true */

x := nil or true;

if x then
else
  Halt(1);
end;

/* true or nil -> true */

x := true or nil;

if x then
else
  Halt(1);
end;

/* nil or false -> false */

x := nil or false;

if x then
  Halt(1);
end;


?}