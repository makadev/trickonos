{?

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
    writeln 'Instances before gc call: '+x.tostring();
    writeln 'Instances after gc call: '+y.tostring();
    Halt(1);
  end;
?}