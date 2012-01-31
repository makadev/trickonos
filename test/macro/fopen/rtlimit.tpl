{?

  x := 1;
  repeat
    x := x+1;
    opendir '.';
  until x > 64;

  Halt(0);

?}