{DOC>> halt machine @br
OPSTR: HALT; TOSOPS 1: TOS=obj;
}
isc_halt_ign,
{DOC>> no operation @br
OPSTR: NOP;
}
isc_nop_ign,
{DOC>> dup tos <operand> times @br
OPSTR: DUP; OP NUM ('+IntToStr(operand)+'); TOSOPS 1: TOS=obj; PUSHES <STATE DEP>;
}
isc_dup_opern,
{DOC>> pop <operand> times @br
OPSTR: POP; OP NUM ('+IntToStr(operand)+'); POPS <STATE DEP>;
}
isc_pop_opern,
{DOC>> jmp @ addr @br
OPSTR: JMP; OP IP+('+IntToStr(operand)+');
}
isc_jmp_addr,
{DOC>> jmp @ addr if TOS=true @br
OPSTR: JMP_TRUE; OP IP+('+IntToStr(operand)+'); TOSOPS 1: TOS=obj;
}
isc_jmp_true_addr,
{DOC>> jmp @ addr if TOS=false or nil @br
OPSTR: JMP_FALSE_NIL; OP IP+('+IntToStr(operand)+'); TOSOPS 1: TOS=obj;
}
isc_jmp_false_nil_addr,
{DOC>> jmp @ addr if TOS=false or nil, pop before jump @br
OPSTR: JMPPOP_FALSE_NIL; OP IP+('+IntToStr(operand)+'); TOSOPS 1: TOS=obj; POPS 1;
}
isc_jmppop_false_nil_addr,
{DOC>> jmp @ addr if TOS-1=false or nil, pop 2, push @br
OPSTR: TOS1_FALSE_NIL_ROT_POP_JMP; OP IP+('+IntToStr(operand)+'); TOSOPS 1: TOS=obj; PUSHES <STATE DEP>; POPS <STATE DEP>;
}
isc_tos1_false_nil_rot_pop_jmp_addr,
{DOC>> jmp @ addr if TOS-1=true or nil, pop 2, push @br
OPSTR: TOS1_TRUE_ROT_POP_JMP; OP IP+('+IntToStr(operand)+'); TOSOPS 1: TOS=obj; PUSHES <STATE DEP>; POPS <STATE DEP>;
}
isc_tos1_true_rot_pop_jmp_addr,
{DOC>> set TOS in ENV[stab[oper]] @br
OPSTR: SETENV; OP STAB['+IntToStr(operand)+']; TOSOPS 1: TOS=obj;
}
isc_setenv_stab,
{DOC>> push ENV[stab[oper]] @br
OPSTR: GETENV; OP STAB['+IntToStr(operand)+']; PUSHES 1;
}
isc_getenv_stab,
{DOC>> move TOS into slot (fp relative stack addr) @br
OPSTR: FPREL_SET; OP SLOT['+IntToStr(operand)+'];
}
isc_fprel_set_slot,
{DOC>> push slot @br
OPSTR: FPREL_LOAD; OP SLOT['+IntToStr(operand)+'];
}
isc_fprel_load_slot,
{DOC>> pop current fp, return with result at slot @br
OPSTR: RET; OP SLOT['+IntToStr(operand)+']; PUSHES 1; POPS <STATE DEP>;
}
isc_ret_slot,
{DOC>> include template on tos @br
OPSTR: INCLUDE; TOSOPS 1: TOS=string; POPS 1;
}
isc_include_ign,
{DOC>> include template from stab (bootstrap) @br
OPSTR: INCLUDE; OP STAB['+IntToStr(operand)+'];
}
isc_include_stab,
{DOC>> include template on tos (USE MODE) @br
OPSTR: USE; TOSOPS 1: TOS=string; POPS 1;
}
isc_use_ign,
{DOC>> write TOS @br
OPSTR: ECHO; TOSOPS 1: TOS=obj; POPS 1;
}
isc_echo_ign,
{DOC>> write TOS, write newline @br
OPSTR: ECHOLN; TOSOPS 1: TOS=obj; POPS 1;
}
isc_echoln_ign,
{DOC>> write newline @br
OPSTR: ECHONL;
}
isc_echonl_ign,
{DOC>> format string with elements in list @br
OPSTR: ECHOFMT; TOSOPS 2: TOS-1=string, TOS=list; POPS 2;
}
isc_echofmt_ign,
{DOC>> substitude string with elements from dict @br
OPSTR: ECHOSUBST; TOSOPS 2: TOS-1=string, TOS=dict; POPS 2;
}
isc_echosubst_ign,
{DOC>> open file TOS (create) @br
OPSTR: OUTPUT_OPEN; TOSOPS 1: TOS=string; POPS 1;
}
isc_output_open_ign,
{DOC>> reopen file TOS (create or open append) @br
OPSTR: OUTPUT_REOPEN; TOSOPS 1: TOS=string; POPS 1;
}
isc_output_reopen_ign,
{DOC>> open directory TOS (set/push new path for outputs) @br
OPSTR: OUTPUT_PATH; TOSOPS 1: TOS=string; POPS 1;
}
isc_output_path_ign,
{DOC>> close (pop last directory or file entry from outstack) @br
OPSTR: OUTPUT_CLOSE;
}
isc_output_close_ign,
{DOC>> push nil @br
OPSTR: SOLOAD_NIL; PUSHES 1;
}
isc_soload_nil_ign,
{DOC>> push true @br
OPSTR: SOLOAD_TRUE; PUSHES 1;
}
isc_soload_true_ign,
{DOC>> push false @br
OPSTR: SOLOAD_FALSE; PUSHES 1;
}
isc_soload_false_ign,
{DOC>> push empty list @br
OPSTR: SOLOAD_LIST; PUSHES 1;
}
isc_soload_list_ign,
{DOC>> push empty dict @br
OPSTR: SOLOAD_DICT; PUSHES 1;
}
isc_soload_dict_ign,
{DOC>> load (vmint size) int from oper @br
OPSTR: SOLOAD_INT; OP INT ('+IntToStr(operand)+'); PUSHES 1;
}
isc_soload_int_operi,
{DOC>> load q number hex int from stab @br
OPSTR: SOLOAD_INT; OP STAB['+IntToStr(operand)+']; PUSHES 1;
}
isc_soload_int_stab,
{DOC>> load string from stab @br
OPSTR: SOLOAD_STRING; OP STAB['+IntToStr(operand)+']; PUSHES 1;
}
isc_soload_string_stab,
{DOC>> call setmember on TOS-1 @br
OPSTR: SETM; OP STAB['+IntToStr(operand)+']; TOSOPS 2: TOS-1=obj, TOS=obj; PUSHES 1; POPS 2;
}
isc_setm_stab,
{DOC>> call getmember on TOS @br
OPSTR: GETM; OP STAB['+IntToStr(operand)+']; TOSOPS 1: TOS=obj; PUSHES 1; POPS 1;
}
isc_getm_stab,
{DOC>> call setindex on TOS-2 @br
OPSTR: SETI; TOSOPS 3: TOS-2=obj, TOS-1=index, TOS=obj; PUSHES 1; POPS 3;
}
isc_seti_ign,
{DOC>> call getmember on TOS-1 @br
OPSTR: GETI; TOSOPS 2: TOS-1=obj, TOS=index; PUSHES 1; POPS 2;
}
isc_geti_ign,
{DOC>> call call obj with oper args (obj is at sp-args) @br
OPSTR: CALLCALL; OP NUM ('+IntToStr(operand)+'); TOSOPS 2: TOS-1=obj, TOS=multiarg; PUSHES 1; POPS <STATE DEP>;
}
isc_callcall_opern,
{DOC>> call method on obj with oper args, methodname on tos @br
OPSTR: CALLM; OP NUM ('+IntToStr(operand)+'); TOSOPS 3: TOS-2=obj, TOS-1=multiarg, TOS=name; PUSHES 1; POPS <STATE DEP>;
}
isc_callm_opern,
{DOC>> call ABS on TOS @br
OPSTR: SOUNOP_ABS; TOSOPS 1: TOS=obj; PUSHES 1; POPS 1;
}
isc_sounop_abs_ign,
{DOC>> call NEG on TOS @br
OPSTR: SOUNOP_NEG; TOSOPS 1: TOS=obj; PUSHES 1; POPS 1;
}
isc_sounop_neg_ign,
{DOC>> call NOT on TOS @br
OPSTR: SOUNOP_NOT; TOSOPS 1: TOS=obj; PUSHES 1; POPS 1;
}
isc_sounop_not_ign,
{DOC>> call ADD on TOS-1 with arg TOS @br
OPSTR: SOBINOP_ADD; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_sobinop_add_ign,
{DOC>> call SUB on TOS-1 with arg TOS @br
OPSTR: SOBINOP_SUB; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_sobinop_sub_ign,
{DOC>> call MUL on TOS-1 with arg TOS @br
OPSTR: SOBINOP_MUL; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_sobinop_mul_ign,
{DOC>> call DIV on TOS-1 with arg TOS @br
OPSTR: SOBINOP_DIV; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_sobinop_div_ign,
{DOC>> call MOD on TOS-1 with arg TOS @br
OPSTR: SOBINOP_MOD; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_sobinop_mod_ign,
{DOC>> call SHL on TOS-1 with arg TOS @br
OPSTR: SOBINOP_SHL; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_sobinop_shl_ign,
{DOC>> call SHR on TOS-1 with arg TOS @br
OPSTR: SOBINOP_SHR; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_sobinop_shr_ign,
{DOC>> call ROL on TOS-1 with arg TOS @br
OPSTR: SOBINOP_ROL; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_sobinop_rol_ign,
{DOC>> call ROR on TOS-1 with arg TOS @br
OPSTR: SOBINOP_ROR; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_sobinop_ror_ign,
{DOC>> call XOR on TOS-1 with arg TOS @br
OPSTR: SOBINOP_XOR; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_sobinop_xor_ign,
{DOC>> call AND on TOS-1 with arg TOS @br
OPSTR: SOBINOP_AND; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_sobinop_and_ign,
{DOC>> call OR on TOS-1 with arg TOS @br
OPSTR: SOBINOP_OR; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_sobinop_or_ign,
{DOC>> compare TOS-1 < TOS, push result @br
OPSTR: SOCMP_LT; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_socmp_lt_ign,
{DOC>> compare TOS-1 > TOS, push result @br
OPSTR: SOCMP_GT; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_socmp_gt_ign,
{DOC>> compare TOS-1 <= TOS, push result @br
OPSTR: SOCMP_LE; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_socmp_le_ign,
{DOC>> compare TOS-1 >= TOS, push result @br
OPSTR: SOCMP_GE; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_socmp_ge_ign,
{DOC>> compare TOS-1 = TOS, push result @br
OPSTR: SOCMP_EQU; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_socmp_equ_ign,
{DOC>> compare TOS-1 <> TOS, push result @br
OPSTR: SOCMP_NEQ; TOSOPS 2: TOS-1=obj, TOS=operand; PUSHES 1; POPS 2;
}
isc_socmp_neq_ign,
{DOC>> append <oper> args to list (list is at TOS-opers) @br
OPSTR: LISTAPPEND; OP NUM ('+IntToStr(operand)+'); PUSHES 1; POPS <STATE DEP>;
}
isc_listappend_opern,
{DOC>> typecheck TOS with typename from stab, push result (true/false) @br
OPSTR: TYPECHECK; OP STAB['+IntToStr(operand)+']; TOSOPS 1: TOS=obj; PUSHES 1; POPS 1;
}
isc_typecheck_stab,
{DOC>> declare function @ ip addr, push function @br
OPSTR: DECL_FUN; OP IP+('+IntToStr(operand)+'); TOSOPS 5: TOS-4=argfix, TOS-3=argnum, TOS-2=slotnum, TOS-1=canvar, TOS=name; PUSHES 1; POPS 5;
}
isc_decl_fun_addr,
{DOC>> create class object, push class @br
OPSTR: DECL_CLASS; OP STAB['+IntToStr(operand)+']; PUSHES 1;
}
isc_decl_class_stab,
{DOC>> declare method with name from stab in class object, pop function @br
OPSTR: DECL_METHOD; OP STAB['+IntToStr(operand)+']; TOSOPS 2: TOS-1=class, TOS=function; POPS 1;
}
isc_decl_method_stab,
