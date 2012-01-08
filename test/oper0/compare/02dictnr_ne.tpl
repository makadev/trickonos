{? 

/* NO REF, NON EMPTY */

if {a:"b",b:"c"} <> {a:"b",b:"c"} then
  Halt(1);
end; 

if {a:"b",b:"c"} < {a:"b",b:"c"} then
  Halt(1);
end; 

if {a:"b",b:"c"} > {a:"b",b:"c"} then
  Halt(1);
end; 

if {a:"b",b:"c"} <= {a:"b",b:"c"} then
else
  Halt(1);
end; 

if {a:"b",b:"c"} >= {a:"b",b:"c"} then
else
  Halt(1);
end; 

if {a:"b",b:"c"} = {a:"b",b:"c"} then
else
  Halt(1);
end; 

?}