{?

 /* check methodcalling without args/passing
    and global var access */

somevar := 1;

class someclass
  function method;
    writeln 'called';
    somevar := somevar + 1;
  end;  
end;

write 'instancing cls ';
writeln someclass;
m := someclass.Create();

write 'calling method ';
writeln m;
m.MethOd();

ASSERT somevar = 2;

?}