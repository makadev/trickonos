{? 

/* NO REF, NON EMPTY */

if 'test' <> 'test' then
  Halt(1);
end; 

if 'test' < 'test' then
  Halt(1);
end; 

if 'test' > 'test' then
  Halt(1);
end; 

if 'test' <= 'test' then
else
  Halt(1);
end; 

if 'test' >= 'test' then
else
  Halt(1);
end; 

if 'test' = 'test' then
else
  Halt(1);
end; 

?}