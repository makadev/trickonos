{?
  USE "ctab.use";
  USE "cindex";
  USE "mindex";
  
  /* write constant index */
  
?}
type
  TConstant_Index = ( {?

  nextval := false;
  foreach k:v in constr.con do
    if nextval then
      write ",";
    else
      nextval := true;
    end;
    ?}
    {? write 'ci_' + k.lowercase();
  end; ?} 
  );

{?
  /* write constant strings */
 ?}
  
const
  C_String_Constant_Table: array[TConstant_Index] of String = ( {?

  nextval := false;
  foreach k:v in constr.con do
    if nextval then
      write ",";
    else
      nextval := true;
    end;
    ?}
    {? write "'"+ v +"' " + '{ci_' + k.lowercase() +'}';
  end; ?} 
  );

{?
  /* write message index */
  
?}
type
  TMessage_Index = ( {?

  nextval := false;
  foreach k:v in resourcestr.con do
    if nextval then
      write ",";
    else
      nextval := true;
    end;
    ?}
    {? write 'mi_' + k.lowercase();
  end; ?} 
  );

{?
  /* write resource string constants */
 ?}

resourcestring {?
  foreach k:v in resourcestr.con do
    ?}
    {? write 'rst_' + k.lowercase() + " = '"+ v +"'; ";
  end; ?}
