{? 

/* REF */

x := { a: 'b',  b: 'c' };

if x <> x then
  Halt(1);
end; 

if x < x then
  Halt(1);
end; 

if x > x then
  Halt(1);
end; 

if x <= x then
else
  Halt(1);
end; 

if x >= x then
else
  Halt(1);
end; 

if x = x then
else
  Halt(1);
end; 

?}