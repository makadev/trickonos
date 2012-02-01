{   Unit for virtual machine operations (opcode handler)

    Copyright (C) 2011-2012  Matthias Karbe

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; see COPYING.txt.
    if not, see <http://www.gnu.org/licenses/> or
    write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

unit vmrun;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, commontl, eomsg, vmstate, ccache, fpath, compload, opcode,
  solnull, socore, cons, corealg, ucfs, outstack, ffpa;


type
  TVMOpcodeHandler = procedure;

procedure vmop_invalid;
procedure vmop_ni;
procedure vmop_nop_ign;

{$I ins_disp_proc.inc}

const
  VMOpcodeHandler: array[TInsOpcodeCode] of TVMOpcodeHandler =
    (
     // isc_nop_ign
     @vmop_nop_ign,
{$I ins_disp.inc}
     // isc_invalid
     @vmop_invalid
  );

implementation

procedure vmop_nop_ign;
begin
  // nop
end;

procedure vmop_invalid;
begin
  put_debug('INVALID OPCODE');
  put_internalerror(2011120555);
end;

procedure vmop_ni;
begin
  put_error('NOT IMPLEMENTED OPCODE '+IntToStr(Ord(template_ip_opcode)));
  put_internalerror(0);
end;

(*******************************************************************************
 Simple Opcode Part.
   The following Opcodes simply change the vmstate in atomic way.
   That means, pushing, jumping, halting and so on.
   They dont do Script Objects Calls (except basic and hardcoded stuff)
 ******************************************************************************)

procedure vmop_halt_ign;
{set machine halt and exitcode (will be taken as exitcode for main application).
 also try printing something if "abort" is used}
begin
  if runtimestack_get(0) = so_true then
    begin
      put_debug('HALT');
      machine_halt(0);
    end
  else if runtimestack_get(0) = so_nil then
    begin
      put_error('ABORT');
      machine_halt(1);
    end
  else if runtimestack_get(0) = so_false then
    begin
      put_error('ABORT with: false');
      machine_halt(1);
    end
  else if runtimestack_get(0)^.IsType(so_string_class) then
    begin
      put_error('ABORT with Message: '+so_string_get_utf8(runtimestack_get(0)));
      machine_halt(1);
    end
  else if runtimestack_get(0)^.IsType(so_integer_class) then
    begin
      if so_integer_fits(runtimestack_get(0)) then
        machine_halt(Byte(so_integer_get(runtimestack_get(0),true)))
      else
        machine_halt(1);
      if machine_exitcode > 0 then
        put_error('ABORT with Code: '+so_integer_string(runtimestack_get(0)))
      else
        put_debug('HALT');
    end
  else
    begin
      machine_halt(1);
      put_error('ABORT');
    end;
end;

procedure vmop_put_stab;
{load stab entry and output it}
begin
  OutputWrite(ucfs_to_utf8string(template_stabentry(template_ip_operand,false)));
end;

{ Template/Use Inclusion }

procedure vmop_include_stab;
var cref: PCodeReference;
begin
  cref := LoadTemplate(true,ucfs_to_utf8string(template_stabentry(template_ip_operand,false)));
  if Length(cref^.pbcode^.image) > 0 then
    templatestack_push(cref,0,false)
  else
    put_warning('empty template inclusion, ignored');
end;

procedure vmop_include_ign;
var cref: PCodeReference;
begin
  if not runtimestack_get(0)^.IsType(so_string_class) then
    put_critical('Include expected String, got '+ so_type_name(runtimestack_get(0)));
  cref := LoadTemplate(true,so_string_get_utf8(runtimestack_get(0)));
  runtimestack_pop(1);
  if Length(cref^.pbcode^.image) > 0 then
    templatestack_push(cref,0,false)
  else
    put_info('empty template inclusion, ignored');
end;

procedure vmop_use_ign;
var cref: PCodeReference;
begin
  if not runtimestack_get(0)^.IsType(so_string_class) then
    put_critical('Include expected String, got '+ so_type_name(runtimestack_get(0)));
  cref := LoadTemplate(false,so_string_get_utf8(runtimestack_get(0)));
  runtimestack_pop(1);
  if Length(cref^.pbcode^.image) > 0 then
    templatestack_push(cref,0,false)
  else
    put_warning('empty use inclusion, ignored');
end;

{ loading values }

procedure vmop_soload_nil_ign;
begin
  runtimestack_push(so_nil);
end;

procedure vmop_soload_true_ign;
begin
  runtimestack_push(so_true);
end;

procedure vmop_soload_false_ign;
begin
  runtimestack_push(so_false);
end;

procedure vmop_soload_list_ign;
begin
  runtimestack_push(so_list_init);
end;

procedure vmop_soload_dict_ign;
begin
  runtimestack_push(so_dict_init);
end;

procedure vmop_soload_int_stab;
var tmpi: PTFM_Integer;
begin
  tmpi := tfm_from_hex(ucfs_to_utf8string(template_stabentry(template_ip_operand,false)),-1);
  if not Assigned(tmpi) then
    put_internalerror(12020100);
  runtimestack_push(so_integer_init_tfm(tmpi));
end;

procedure vmop_soload_string_stab;
begin
  runtimestack_push(
    so_string_init_ucfs(
      template_stabentry(template_ip_operand,true),false));
end;

procedure vmop_soload_int_operi;
begin
  runtimestack_push(so_integer_init(template_ip_operand));
end;

{template internal jumps,
  -> use template_ip_setrel(addr,true) which sets a jumpfix such that
     loading next instruction wont simple take the next, but the one we
     jumped.
     also it checks for rel addr in range of image and simply adds the rel addr.}

procedure vmop_jmp_addr;
begin
  template_ip_setrel(template_ip_operand,true);
end;

procedure vmop_jmp_true_addr;
begin
  if runtimestack_get(0) = so_true then
    template_ip_setrel(template_ip_operand,true);
end;

procedure vmop_jmp_false_nil_addr;
begin
  if (runtimestack_get(0) = so_false) or
     (runtimestack_get(0) = so_nil) then
    template_ip_setrel(template_ip_operand,true);
end;

procedure vmop_jmppop_false_nil_addr;
begin
  if (runtimestack_get(0) = so_false) or
     (runtimestack_get(0) = so_nil) then
    begin
      runtimestack_pop(1);
      template_ip_setrel(template_ip_operand,true);
    end;
end;

procedure vmop_tos1_true_rot_pop_jmp_addr;
begin
  if runtimestack_get(1) = so_true then
    begin
      runtimestack_moved(0,1);
      runtimestack_pop(1);
      template_ip_setrel(template_ip_operand,true);
    end;
end;

procedure vmop_tos1_false_nil_rot_pop_jmp_addr;
begin
  if (runtimestack_get(1) = so_false) or
     (runtimestack_get(1) = so_nil) then
    begin
      runtimestack_moved(0,1);
      runtimestack_pop(1);
      template_ip_setrel(template_ip_operand,true);
    end;
end;

{set or get env entry}

procedure vmop_getenv_stab;
begin
  globalenv_load(template_stabentry(template_ip_operand,false));
end;

procedure vmop_setenv_stab;
begin
  globalenv_set_tos(template_stabentry(template_ip_operand,false));
end;

{pop nr elements from object stack}

procedure vmop_pop_opern;
begin
  runtimestack_pop(template_ip_operand);
end;

{typecheck}

procedure vmop_typecheck_stab;
{hardcoded typecheck call, checks TOS against TOS.Cls.TypeQuery}
begin
  if ucfs_compare_a7(template_stabentry(template_ip_operand,false),
      Upcase(runtimestack_get(0)^.GetTypeCls^.TypeQuery(runtimestack_get(0)))) = 0 then
    runtimestack_push(so_true)
  else
    runtimestack_push(so_false);

  runtimestack_moved(0,1);
  runtimestack_pop(1);
end;

{listappend}

procedure vmop_listappend_opern;
{hardcoded list append. Appends nrops Args to the List @relstack(args)}
begin
  if runtimestack_get(template_ip_operand)^.GetTypeCls <> so_list_class then
    put_internalerror(2011120990);

  {hardcoded list method call, LIST does not have any overrides}
  check_so_maxargs(template_ip_operand);
  runtimestack_push(
    so_list_append(runtimestack_get(template_ip_operand),
      runtimestack_getargf(template_ip_operand-1),template_ip_operand));

  runtimestack_moved(0,template_ip_operand+1);
  runtimestack_pop(template_ip_operand+1);
end;

{echo out (write*)}

procedure vmop_echo_ign;
begin
  OutputWrite(so_any_flatstring(runtimestack_get(0)));
  runtimestack_pop(1);
end;

procedure vmop_echonl_ign;
begin
  OutputWrite('',true);
end;

procedure vmop_echoln_ign;
begin
  OutputWrite(so_any_flatstring(runtimestack_get(0)),true);
  runtimestack_pop(1);
end;

procedure vmop_echofmt_ign;
begin
{$WARNING implement me}
  // format string ... -> string,list
  put_internalerror(0);
end;

procedure vmop_echosubst_ign;
{echo call, probably removed in future}
var substr: String;
    subr: String;
    pdict, res: PSOInstance;
    i,j,l: Integer;
begin
  if (runtimestack_get(1)^.GetTypeCls = so_string_class) and
     (runtimestack_get(0)^.GetTypeCls = so_dict_class) then
    begin
      substr := so_string_get_utf8(runtimestack_get(1));
      if Length(substr) > 0 then
        begin
          {$NOTE outsource}
          pdict := runtimestack_get(0);
          // substitute string ... -> string,dict
          i := 1;
          l := 0;
          while i <= Length(substr) do
            begin
              if (substr[i] = ':') and
                 ((i+2) <= Length(substr)) then
                begin
                  j := i+1;
                  while (j <= Length(substr)) and
                        (substr[j] <> ':') do
                    Inc(j,1);
                  if j > Length(substr) then
                    break;
                  if j <> i+1 then
                    begin
                      {copy leftof l+1..i-1}
                      if (l+1) <= (i-1) then
                        OutputWrite(Copy(substr,l+1,(i-1)-l));
                      l := j;
                      {copy i+1<subr>j-1}
                      subr := Copy(substr,i+1,(j-1)-i);
                      res := so_dict_get_member(pdict,subr);
                      OutputWrite(so_any_flatstring(res));
                      i := j+1;
                    end
                  else
                    begin
                      {copy leftof l+1..i-1}
                      if (l+1) <= (i-1) then
                        OutputWrite(Copy(substr,l+1,(i-1)-l));
                      {skip ::, do :}
                      Inc(i,2);
                      l := i-1;
                      OutputWrite(':');
                    end;
                end
              else
                Inc(i,1);
            end;
          {write stuff from l..Length(substr)}
          if (l+1) <= (Length(substr)) then
            OutputWrite(Copy(substr,l+1,Length(substr)-l));
        end;
      runtimestack_pop(2);
    end
  else
    runtimestack_push(so_error_init('EchoSubst expected String,Dict Pair'));
end;

procedure vmop_decl_fun_addr;
{
  stack layout:
    TOS  = canvararg
    TOS-1  = slotn
    TOS-2  = argn
    TOS-3  = argf
}
var funobj: PSOInstance;
    slotn,argn,argf: Integer;
    canv: Boolean;
begin
  if (runtimestack_get(0)^.GetTypeCls <> so_boolean_class) or
     (runtimestack_get(1)^.GetTypeCls <> so_integer_class) or
     (runtimestack_get(2)^.GetTypeCls <> so_integer_class) or
     (runtimestack_get(3)^.GetTypeCls <> so_integer_class) then
    put_internalerror(2011121160);
  canv := runtimestack_get(0) = so_true;
  slotn := so_integer_get(runtimestack_get(1),false);
  argn := so_integer_get(runtimestack_get(2),false);
  argf := so_integer_get(runtimestack_get(3),false);
  funobj := so_function_init(templatestack_tos,
                             argf,argn,slotn,template_ip+template_ip_operand,canv);
  runtimestack_push(funobj);
  runtimestack_moved(0,4);
  runtimestack_pop(4);
end;

procedure vmop_fprel_set_slot;
begin
  runtimestack_moved(0,runtimestack_fprel_to_sp(template_ip_operand));
end;

procedure vmop_fprel_load_slot;
begin
  runtimestack_push(
    runtimestack_get(runtimestack_fprel_to_sp(template_ip_operand)));
  // need explicit incref, since neither get nor push incref (but po and ops will decref!)
  runtimestack_get(0)^.IncRef;
end;

procedure vmop_ret_slot;
begin
  {simply mv result slot @ FP+1 (where the function object resides)
   and clean up TOS..FP+2}
  runtimestack_moved(
    runtimestack_fprel_to_sp(template_ip_operand),
    runtimestack_fprel_to_sp(1));
  runtimestack_pop(runtimestack_fprel_to_sp(1));
  templatestack_pop;
end;

procedure vmop_dup_opern;
var nr: Integer;
begin
  for nr := template_ip_operand downto 1 do
    begin
      runtimestack_push(so_nil); // create place
      runtimestack_moved(1,0);   // copy with correct reference counting
    end;
end;

{compare set}

procedure vmop_socmp_lt_ign;
begin
  if runtimestack_get(1)^.GetTypeCls^.Compare(
        runtimestack_get(1),
        runtimestack_get(0)) = socmp_isLess then
    runtimestack_push(so_true)
  else
    runtimestack_push(so_false);
  runtimestack_moved(0,2);
  runtimestack_pop(2);
end;

procedure vmop_socmp_gt_ign;
begin
  if runtimestack_get(1)^.GetTypeCls^.Compare(
        runtimestack_get(1),
        runtimestack_get(0)) = socmp_isGreater then
    runtimestack_push(so_true)
  else
    runtimestack_push(so_false);
  runtimestack_moved(0,2);
  runtimestack_pop(2);
end;

procedure vmop_socmp_le_ign;
begin
  if runtimestack_get(1)^.GetTypeCls^.Compare(
        runtimestack_get(1),
        runtimestack_get(0)) in [socmp_isEqual,socmp_isLess] then
    runtimestack_push(so_true)
  else
    runtimestack_push(so_false);
  runtimestack_moved(0,2);
  runtimestack_pop(2);
end;

procedure vmop_socmp_ge_ign;
begin
  if runtimestack_get(1)^.GetTypeCls^.Compare(
        runtimestack_get(1),
        runtimestack_get(0)) in [socmp_isEqual,socmp_isGreater] then
    runtimestack_push(so_true)
  else
    runtimestack_push(so_false);
  runtimestack_moved(0,2);
  runtimestack_pop(2);
end;

procedure vmop_socmp_equ_ign;
begin
  if runtimestack_get(1)^.GetTypeCls^.Compare(runtimestack_get(1),runtimestack_get(0)) = socmp_isEqual then
    runtimestack_push(so_true)
  else
    runtimestack_push(so_false);
  runtimestack_moved(0,2);
  runtimestack_pop(2);
end;

procedure vmop_socmp_neq_ign;
begin
  if runtimestack_get(1)^.GetTypeCls^.Compare(runtimestack_get(1),runtimestack_get(0)) <> socmp_isEqual then
    runtimestack_push(so_true)
  else
    runtimestack_push(so_false);
  runtimestack_moved(0,2);
  runtimestack_pop(2);
end;

procedure vmop_decl_class_stab;
{create class and push on tos}
begin
  runtimestack_push(
    so_class_create(ucfs_to_utf8string(template_stabentry(template_ip_operand,false)),
                    templatestack_tos,
                    template_ip));
end;

procedure vmop_decl_method_stab;
{TOS-1 = Class, TOS = function obj
  -> pop function and register with stab name
  -> leave class}
begin
  if runtimestack_get(0)^.IsType(so_function_class) and
     runtimestack_get(1)^.IsType(so_class_class) then
    begin
      so_class_methodreg(runtimestack_get(1),
                         runtimestack_get(0),
                         template_stabentry(template_ip_operand,false));
      runtimestack_pop(1);
    end
  else
    put_internalerror(12012310);
end;

{output set}

procedure vmop_output_open_ign;
begin
  if runtimestack_get(0)^.IsType(so_string_class) then
    outstack_push(so_string_get_utf8(runtimestack_get(0)),opfm_open)
  else
    put_critical('Expected String');
  runtimestack_pop(1);
end;

procedure vmop_output_reopen_ign;
begin
  if runtimestack_get(0)^.IsType(so_string_class) then
    outstack_push(so_string_get_utf8(runtimestack_get(0)),opfm_reopen)
  else
    put_critical('Expected String');
  runtimestack_pop(1);
end;

procedure vmop_output_path_ign;
begin
  if runtimestack_get(0)^.IsType(so_string_class) then
    outstack_push(so_string_get_utf8(runtimestack_get(0)),opfm_path)
  else
    put_critical('Expected String');
  runtimestack_pop(1);
end;

procedure vmop_output_close_ign;
begin
  if not outstack_pop then
    put_warning('No Outfile, Ignored Close.');
end;

(*******************************************************************************
 SO Opcode Part.
   The following Opcodes involve built-in/script object calling.
   The Machine must decide wether to call some built-in handler or
   setup the stack and do call translation. (meaning, the script object
   call is translated into a function object call and setup such that
   the function object handles stuff).

   They are all MethodCalls. MethodName, Argument number, Called Object and
   Arguments are implied by the opcode.
   Basically, this checks for a certain Interface
   (like sotif_baseOperType, which implies that several Operations can be called
    directly. (they do not check the MethodOverride! its either built-in gives
    result or fail)).
   For the default Interface sotif_baseType there are 2 possiblites.

   Either MethodCallOverride returns a Function Object, which is to be used
   as Method Call or MethodCall returns some value.

   In general, the stack layout is always the same.
     SP-args   (object -> soself)
     SP-args+1 (arg_0)
     ...
     SP-0      (arg_(args))

     if either MethodCallOverride returns a Function Object or <object/soself>
     itself is a Function Object, it is used for stack frame setup and argument
     checks. Thats all, no magic.
 ******************************************************************************)

procedure do_so_method_call( hashkey: MachineWord; mname: PUCFS32String; args: Integer);
{ Do a named method call. }
var res,fres,mres,obj: PSOInstance;
    objcls: PSOTypeCls;
    ci: TMethodCallInfo;
    coi: TMethodOverrideCallInfo;
begin
  obj := runtimestack_get(args);
  objcls := obj^.GetTypeCls;

  {check for method override}
  if not Assigned(objcls^.MethodCallOverride) then
    res := nil
  else
    begin
      with coi do
        begin
          hashk := hashkey;
          name := mname;
          soself := obj;
        end;
      res := objcls^.MethodCallOverride(@coi);
    end;

  if not Assigned(res) then
    begin
      {call method handler on objcls}
      mres := nil;
      if Assigned(objcls^.MethodCall) then
        begin
          with ci do
            begin
              hashk := hashkey;
              name := mname;
              soself := obj;
              if args > 0 then
                soargs := runtimestack_getargf(args-1)
              else
                soargs := nil;
              argnum := args;
            end;
          mres := objcls^.MethodCall(@ci)
        end;
      {check (method handler exists or) method result}
      if Assigned(mres) then
        begin
          {clear frame, push result}
          runtimestack_push(mres);
          runtimestack_moved(0,args+1);
          runtimestack_pop(args+1);
        end
      else
        begin
          {no method or handling -> default error}
          runtimestack_push(init_operation_error(obj,mname));
        end;
    end
  else
    begin
      {methodcalloverride returned something, check if its an
        -> function object for call (needs setup and call)
        -> instance object on creation (needs check for constructor)}
      ASSERT( (res^.GetTypeCls = so_function_class) or
              (res^.GetTypeCls = so_instance_class) );
      {check for instance -> constructor call}
      if res^.IsType(so_instance_class) then
        begin
          {check for constructor call}
          fres := so_class_getconstructor(so_instance_getclass(res));
          if Assigned(fres) then
            begin
              {trick: replace class with instance, since class is not used
                      on stack after creation and bound in the instance itself}
              runtimestack_push(res);
              runtimestack_moved(0,args+1);
              runtimestack_pop(1);
              res := fres;
              {from here, everything like function object
                res=constructor will be setup as method call on instance instead
                of method call on class}
            end
          else
            begin
              {no constructor, check args=0, replace class with instance
               and thats all (no actual call)}
              if args = 0 then
                begin
                  runtimestack_push(res);
                  runtimestack_moved(0,1);
                  runtimestack_pop(1);
                end
              else
                begin
                  {args given, but no constructor -> fail}
                  runtimestack_push(init_invargnum_error(res,args,ci_us(ci_dm_create,false)));
                end;
              exit;
            end;
        end;

      {now we have either function object or constructor (also function) -> setup call}
      with ci do
        begin
          hashk := DEFAULT_METHOD_DirectCall_Hash;
          name := ci_us(ci_dm_call,false);
          soself := res;
          soargs := nil; // <- function object resets fp so args is invalid anyway
          argnum := args;
        end;
      ASSERT( res^.IsType(so_function_class) );
      ASSERT( Assigned(res^.GetTypeCls^.MethodCall) ); // -> function type needs methodcall

      {setup the call}
      fres := res^.GetTypeCls^.MethodCall(@ci);

      res^.DecRef; // <- deref, since function object increfed with MethodCallOverride
      if Assigned(fres) then
        begin
          if fres^.GetTypeCls <> so_error_class then
            put_internalerror(2011122051); // either error or nil for correct setup!
          {something was wrong on setup -> push error}
          runtimestack_push(fres);
        end;
    end;
end;

{call unary operation on so object}

procedure vmop_sounop_abs_ign;
begin
  do_so_method_call(DEFAULT_METHOD_UnOpAbs_Hash,ci_us(ci_dm_abs,false),0);
end;

procedure vmop_sounop_neg_ign;
begin
  do_so_method_call(DEFAULT_METHOD_UnOpNeg_Hash,ci_us(ci_dm_neg,false),0);
end;

procedure vmop_sounop_not_ign;
begin
  do_so_method_call(DEFAULT_METHOD_UnOpNot_Hash,ci_us(ci_dm_not,false),0);
end;

{call binary operation on so object}

procedure vmop_sobinop_add_ign;
begin
  do_so_method_call(DEFAULT_METHOD_BinOpAdd_Hash,ci_us(ci_dm_add,false),1);
end;

procedure vmop_sobinop_sub_ign;
begin
  do_so_method_call(DEFAULT_METHOD_BinOpSub_Hash,ci_us(ci_dm_sub,false),1);
end;

procedure vmop_sobinop_mul_ign;
begin
  do_so_method_call(DEFAULT_METHOD_BinOpMul_Hash,ci_us(ci_dm_mul,false),1);
end;

procedure vmop_sobinop_div_ign;
begin
  do_so_method_call(DEFAULT_METHOD_BinOpDiv_Hash,ci_us(ci_dm_div,false),1);
end;

procedure vmop_sobinop_mod_ign;
begin
  do_so_method_call(DEFAULT_METHOD_BinOpMod_Hash,ci_us(ci_dm_mod,false),1);
end;

procedure vmop_sobinop_shl_ign;
begin
  do_so_method_call(DEFAULT_METHOD_BinOpShl_Hash,ci_us(ci_dm_shl,false),1);
end;

procedure vmop_sobinop_shr_ign;
begin
  do_so_method_call(DEFAULT_METHOD_BinOpShr_Hash,ci_us(ci_dm_shr,false),1);
end;

procedure vmop_sobinop_rol_ign;
begin
  do_so_method_call(DEFAULT_METHOD_BinOpRol_Hash,ci_us(ci_dm_rol,false),1);
end;

procedure vmop_sobinop_ror_ign;
begin
  do_so_method_call(DEFAULT_METHOD_BinOpRor_Hash,ci_us(ci_dm_ror,false),1);
end;

procedure vmop_sobinop_and_ign;
begin
  do_so_method_call(DEFAULT_METHOD_BinOpAnd_Hash,ci_us(ci_dm_and,false),1);
end;

procedure vmop_sobinop_or_ign;
begin
  do_so_method_call(DEFAULT_METHOD_BinOpOr_Hash,ci_us(ci_dm_or,false),1);
end;

procedure vmop_sobinop_xor_ign;
begin
  do_so_method_call(DEFAULT_METHOD_BinOpXor_Hash,ci_us(ci_dm_xor,false),1);
end;

{special ops}

procedure vmop_setm_stab;
begin
  {need fix, currently its: <caller,value>, we need <caller,name,value> -> push stab entry
   and switch}
  runtimestack_push(so_string_init_ucfs(template_stabentry(template_ip_operand,true),false));
  runtimestack_switch(0,1);
  do_so_method_call(DEFAULT_METHOD_SetMember_Hash,ci_us(ci_dm_setmember,false),2);
end;

procedure vmop_getm_stab;
begin
  {push stab entry}
  runtimestack_push(so_string_init_ucfs(template_stabentry(template_ip_operand,true),false));
  do_so_method_call(DEFAULT_METHOD_GetMember_Hash,ci_us(ci_dm_getmember,false),1);
end;

procedure vmop_seti_ign;
begin
  do_so_method_call(DEFAULT_METHOD_SetIndex_Hash,ci_us(ci_dm_setindex,false),2);
end;

procedure vmop_geti_ign;
begin
  do_so_method_call(DEFAULT_METHOD_GetIndex_Hash,ci_us(ci_dm_getindex,false),1);
end;

{calls}

procedure vmop_callm_opern;
var name: PUCFS32String;
begin
  {simple, no interface check since every so object has a methodcall
   interface}
  ASSERT( runtimestack_get(0)^.GetTypeCls = so_string_class ); // invalid stack layout
  check_so_maxargs(template_ip_operand);
{$NOTE this should come from stab somehow}
  name := so_string_get_ucfs(runtimestack_get(0),true);
  runtimestack_pop(1);
  ASSERT(ucfs_length(name) > 0);
{$IFDEF DEBUG}
  try
    do_so_method_call(mas3hash_sigma(name),name,template_ip_operand);
  finally
    ucfs_release(name);
  end;
{$ELSE}
  do_so_method_call(mas3hash_sigma(name),name,template_ip_operand);
  ucfs_release(name);
{$ENDIF}
end;

procedure vmop_callcall_opern;
begin
  check_so_maxargs(template_ip_operand);
  do_so_method_call(DEFAULT_METHOD_DirectCall_Hash,ci_us(ci_dm_call,false),template_ip_operand);
end;

end.

