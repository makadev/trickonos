{?
 
  function stackprint;
  var i,j;
  begin
    i := System.MachineState.Frames;
    j := 0;
    writeln "STACKPRINT START";
    writeln "  PRINTING "+i.ToStr()+' FRAMES: ';
    repeat
      write "  ";
      write System.MachineState.RelFrameType(j);
      write " at ";
      write System.MachineState.RelFrameShortName(j);
      write ".l";
      write System.MachineState.RelFrameLine(j);
      write ".c";
      write System.MachineState.RelFrameColumn(j);
      write " (";
      write System.MachineState.RelFrameLongName(j);
      writeln ")";
      j := j+1;
    until j >= i;
    writeln "STACKPRINT END";
  end;

  stackprint();
?}