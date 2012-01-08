{? 

/* None Part */

/* <nil> and 1 -> nil */

x := nil and 1;

if x then
  Halt(1);
end;

/* Boolean Part */

/* true and 1 -> 1 
   assuming "+" operation is without fault here  */

x := true and 1;
x := x+1;

/* false and 1 -> false */

x := false and 1;

if x then
  Halt(1);
end;

/* no other checks since 
    these are operations: 
      x and y -> x.and(y) iff x<>Boolean/None */

?}