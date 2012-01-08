{?
 
  function stackprint;
  var i,j,ms;
  begin
    ms := System.MachineState;
    i := ms.Frames;
    j := 0;
    writeln "STACKPRINT START";
    writeln "  PRINTING "+i.ToStr()+' FRAMES: ';
    repeat
      write "  sp-"+j.ToStr()+" ";
      write ms.RelFrameType(j);
      write " at ";
      write ms.RelFrameShortName(j);
      write ".l";
      write ms.RelFrameLine(j);
      write ".c";
      write ms.RelFrameColumn(j);
      write " (";
      write ms.RelFrameLongName(j);
      writeln ")";
      j := j+1;
    until j >= i;
    writeln "STACKPRINT END";
  end;

  function recurse10_stackprint(x);
  begin
    if x < 10 then
     recurse10_stackprint(x+1);
    else
      stackprint();
    end;
  end;

  recurse10_stackprint(0);
?}