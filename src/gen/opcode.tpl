{?
  USE 'rel://opcode/opcode.use';
  
  USE 'rel://opcode/machine.def';

  USE 'rel://opcode/bcloader.def';
  USE 'rel://opcode/echo.def';
  USE 'rel://opcode/output.def';

  USE 'rel://opcode/soload.def';
  USE 'rel://opcode/sooper.def';
  USE 'rel://opcode/socompare.def';
  USE 'rel://opcode/sospec.def';
  
  /* gen opcode index */


  OPEN 'ins_enum.inc';

    foreach x in opcodelst do
      writeln '{DOC>> '+x.docstring+' @br';
      writeln 'OPSTR: '+x.opstring;
      writeln '}';
      writeln 'isc_' + x.opname + ',';
    end;
  
  CLOSE; 

  /* gen validator enums */
  
  foreach k:v in OpcodeCType do
    OPEN 'ins_type_'+v.lowercase()+'.inc';
      c := 0;
      foreach x in opcodelst do
        if x.optype = v then
          if c > 0 then
            writeln ',';
          end;
          write 'isc_' + x.opname;
          c := c+1;
        end;
      end;
    CLOSE; 
  end;
  
  /* gen asm output */

  OPEN 'ins_asmout.inc';
    foreach x in opcodelst do
      writeln 'isc_' + x.opname + ': ' + "Result := '"+x.opstring+"';";
    end;
  CLOSE; 
  
  /* gen dispatcher field */

  OPEN 'ins_disp.inc';
    foreach x in opcodelst do
      writeln '@vmop_' + x.opname + ',';
    end;
  CLOSE; 

  /* proc int */

  OPEN 'ins_disp_proc.inc';
    foreach x in opcodelst do
      writeln 'procedure vmop_' + x.opname + ';';
    end;
  CLOSE; 
  
?}