{? 

/* <nil> and true -> <nil> */

x := nil and true;

if x then
  Halt(1);
end;

/* true and x -> x */

x := true and nil;

if x then
  Halt(1);
end;

/* <nil> and false -> nil */

x := nil and false;

if x then
  Halt(1);
end;


?}