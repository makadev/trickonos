
type
  TConstant_Index = ( 
    ci_dm_setmember,
    ci_dm_getmember,
    ci_dm_setindex,
    ci_dm_getindex,
    ci_dm_add,
    ci_dm_sub,
    ci_dm_mul,
    ci_dm_div,
    ci_dm_mod,
    ci_dm_shl,
    ci_dm_shr,
    ci_dm_rol,
    ci_dm_ror,
    ci_dm_and,
    ci_dm_or,
    ci_dm_xor,
    ci_dm_neg,
    ci_dm_abs,
    ci_dm_not,
    ci_dm_call,
    ci_dm_create,
    ci_name_system,
    ci_name_iterator,
    ci_name_next,
    ci_name_current,
    ci_name_currentkey,
    ci_name_currentvalue,
    ci_name_self,
    ci_name_result,
    ci_name_varargs 
  );


  
const
  C_String_Constant_Table: array[TConstant_Index] of String = ( 
    'SETMEMBER' {ci_dm_setmember},
    'GETMEMBER' {ci_dm_getmember},
    'SETINDEX' {ci_dm_setindex},
    'GETINDEX' {ci_dm_getindex},
    'ADD' {ci_dm_add},
    'SUB' {ci_dm_sub},
    'MUL' {ci_dm_mul},
    'OP_DIV' {ci_dm_div},
    'OP_MOD' {ci_dm_mod},
    'OP_SHL' {ci_dm_shl},
    'OP_SHR' {ci_dm_shr},
    'OP_ROL' {ci_dm_rol},
    'OP_ROR' {ci_dm_ror},
    'OP_AND' {ci_dm_and},
    'OP_OR' {ci_dm_or},
    'OP_XOR' {ci_dm_xor},
    'OP_NEG' {ci_dm_neg},
    'OP_ABS' {ci_dm_abs},
    'OP_NOT' {ci_dm_not},
    'CALL' {ci_dm_call},
    'CREATE' {ci_dm_create},
    'SYSTEM' {ci_name_system},
    'ITERATOR' {ci_name_iterator},
    'NEXT' {ci_name_next},
    'CURRENT' {ci_name_current},
    'CURRENTKEY' {ci_name_currentkey},
    'CURRENTVALUE' {ci_name_currentvalue},
    'SELF' {ci_name_self},
    'RESULT' {ci_name_result},
    'VARARGS' {ci_name_varargs} 
  );


type
  TMessage_Index = ( 
    mi_cl_arg_limit,
    mi_cl_sbuf_limit,
    mi_cl_colitem_limit,
    mi_cl_objtrace_limit,
    mi_cl_tplstack_limit,
    mi_cl_objstack_limit,
    mi_cl_crecurse_limit,
    mi_cl_bcloader_limit 
  );



resourcestring 
    rst_cl_arg_limit = 'Hit SO Call Argument Limit'; 
    rst_cl_sbuf_limit = 'Hit SO Shortbuffer Limit'; 
    rst_cl_colitem_limit = 'Hit SO Collection Limit'; 
    rst_cl_objtrace_limit = 'Hit SO Object Limit'; 
    rst_cl_tplstack_limit = 'Hit SO Inclusion/Call Limit'; 
    rst_cl_objstack_limit = 'Hit SO Stack Limit'; 
    rst_cl_crecurse_limit = 'Hit Compiler Recursion Depth Limit'; 
    rst_cl_bcloader_limit = 'Hit Bytecode Loader Limit'; 
