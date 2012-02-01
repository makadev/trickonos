{?

  /* check for flat trace mode */
  if System.Instances() = System.Instances() then
    Halt(0);
  end;

  /* startup with <n> instances */
  x := System.Instances();

  /* now call gc */
  System.GCollect();

  y := System.Instances();
  
  /* gc call returns <y := System.Instances()> 
     -> right expr evaluated first, 
        everything else is in stab, so y should be x+1
        because <x := System.Instances();> created a new instance after
        processing the call */
        
  if (x+1) <> y then
    writeln 'Instances before gc call: '+x.tostr();
    writeln 'Instances after gc call: '+y.tostr();
    Halt(1);
  end;
?}