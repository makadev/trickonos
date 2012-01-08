{? 

/* None Part */

/* <nil> or 1 -> 1 */

x := nil or 1;

if x then
else
  Halt(1);
end;

/* Boolean Part */

/* true or 1 -> true
   assuming "=" operation is without fault here  */

x := true or 1;
if x = true then
else
  Halt(1);
end;

/* false or 1 -> 1 */

x := false or 1;

if x then
else
  Halt(1);
end;

/* no other checks since 
    these are operations: 
      x or y -> x.or(y) iff x<>Boolean/None */

?}