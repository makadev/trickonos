{? 
  if (2147483647 << 1) <> -2 then
    Halt(1);
  end;

  if (%1111 << 1) <> %11110 then
    Halt(1);
  end;
?}