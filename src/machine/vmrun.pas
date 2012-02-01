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

procedure vmop_m_nop_ign;
procedure vmop_m_haltabort_ign;
procedure vmop_m_puts_stab;
procedure vmop_m_include;
procedure vmop_m_load_type;
procedure vmop_m_jumpaddr;
procedure vmop_m_envmod;
procedure vmop_m_pop_nr;
procedure vmop_object_operation_unary;
procedure vmop_object_operation_binary;
procedure vmop_object_operation_setget;
procedure vmop_object_typecheck;
procedure vmop_listappend;
procedure vmop_m_echo;
procedure vmop_m_calltranslate;
procedure vmop_m_declfun;
procedure vmop_m_fprel;
procedure vmop_m_returnclean;
procedure vmop_m_stackdup;
procedure vmop_compare;
procedure vmop_class_create;
procedure vmop_method_decl;
procedure vmop_openout;

const
  VMOpcodeHandler: array[TInsOpcodeCode] of TVMOpcodeHandler =
    (
     // isc_m_nop_ign
     @vmop_m_nop_ign,
     // isc_m_stackdup_nr,
     @vmop_m_stackdup,
     // isc_m_echo,
     @vmop_m_echo,
     // isc_m_pop_nr
     @vmop_m_pop_nr,
     // isc_m_include_stab,
     @vmop_m_include,
     // isc_m_include_ign,
     @vmop_m_include,
     // isc_m_halt_ign,
     @vmop_m_haltabort_ign,
     // isc_m_puts_stab,
     @vmop_m_puts_stab,
     // isc_m_jmp_addr,
     @vmop_m_jumpaddr,
     // isc_m_jmp_true_addr,
     @vmop_m_jumpaddr,
     // isc_m_jmp_false_nil_addr,
     @vmop_m_jumpaddr,
     // isc_m_jmppop_false_nil_addr,
     @vmop_m_jumpaddr,
     // isc_m_tos1_false_nil_rot_pop_jmp_addr,
     @vmop_m_jumpaddr,
     // isc_m_tos1_true_rot_pop_jmp_addr,
     @vmop_m_jumpaddr,
     // isc_m_load_type,
     @vmop_m_load_type,
     // isc_m_setenv_stab,
     @vmop_m_envmod,
     // isc_m_getenv_stab,
     @vmop_m_envmod,

     // isc_o_setm_stab,
     @vmop_object_operation_setget,
     // isc_o_getm_stab,
     @vmop_object_operation_setget,
     // isc_o_seti_ign,
     @vmop_object_operation_setget,
     // isc_o_geti_ign,
     @vmop_object_operation_setget,
     // isc_o_callcall_nrops,
     @vmop_m_calltranslate,
     // isc_o_callm_nrops,
     @vmop_m_calltranslate,

     // isc_o_binaryop,
     @vmop_object_operation_binary,

     // isc_o_unaryop,
     @vmop_object_operation_unary,

     // isc_o_listappend_nr,
     @vmop_listappend,

     // isc_o_typecheck_stab,
     @vmop_object_typecheck,

     // isc_m_load_int_stab,
     @vmop_m_load_type,

     // isc_m_load_int_oper,
     @vmop_m_load_type,

     // isc_m_load_string_stab,
     @vmop_m_load_type,

     // isc_m_fprel_set_slot,
     @vmop_m_fprel,

     // isc_m_fprel_load_slot,
     @vmop_m_fprel,

     // isc_m_ret_slot,
     @vmop_m_returnclean,

     // isc_m_decl_fun_addr,
     @vmop_m_declfun,

     // isc_o_compare,
     @vmop_compare,

     // isc_m_class_stab,
     @vmop_class_create,

     // isc_m_decl_method_stab,
     @vmop_method_decl,

     // isc_m_outputop,
     @vmop_openout,

     // isc_invalid
     @vmop_invalid
  );

implementation

(*******************************************************************************
 Simple Opcode Part.
   The following Opcodes simply change the vmstate in atomic way.
   That means, pushing, jumping, halting and so on.
   They dont do Script Objects Calls (except basic and hardcoded stuff)
 ******************************************************************************)

procedure vmop_m_nop_ign;
{yeah.. a must have for every machine :D}
begin
  // nop
end;

procedure vmop_m_haltabort_ign;
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

procedure vmop_m_puts_stab;
{load stab entry and output it}
begin
  OutputWrite(ucfs_to_utf8string(template_stabentry(template_ip_operand,false)));
end;

procedure vmop_m_include;
{include a template,
 either using a stab entry as filename or
 a string from stack}
var cref: PCodeReference;
begin
  case template_ip_opcode of
    isc_m_include:
      begin
        if not runtimestack_get(0)^.IsType(so_string_class) then
          put_critical('Include expected String, got '+ so_type_name(runtimestack_get(0)));
        cref := LoadTemplate(TInsMIncludeMode(template_ip_operand),so_string_get_utf8(runtimestack_get(0)));
        runtimestack_pop(1);
      end;
    isc_m_include_stab:
      begin
        cref := LoadTemplate(mincl_include,ucfs_to_utf8string(template_stabentry(template_ip_operand,false)));
      end;
    else
      put_internalerror(2011120703);
  end;
  if Length(cref^.pbcode^.image) > 0 then
    templatestack_push(cref,0,false)
  else
    if TInsMIncludeMode(template_ip_operand) <> mincl_use then
      put_info('empty template inclusion, ignored');
end;

procedure vmop_m_load_type;
{load a base type, empty or from stab}
var tmpi: PTFM_Integer;
begin
  case template_ip_opcode of
    isc_m_load_type:
      begin
        case TInsSOLoad(template_ip_operand) of
          soload_nil:
            runtimestack_push(so_nil);
          soload_true:
            runtimestack_push(so_true);
          soload_false:
            runtimestack_push(so_false);
          soload_list:
            runtimestack_push(so_list_init);
          soload_dict:
            runtimestack_push(so_dict_init);
          else
            put_internalerror(2011120920);
        end;
      end;
    isc_m_load_int_stab:
      begin
        tmpi := tfm_from_hex(ucfs_to_utf8string(template_stabentry(template_ip_operand,false)),-1);
        if not Assigned(tmpi) then
          put_internalerror(12020100);
        runtimestack_push(so_integer_init_tfm(tmpi));
      end;
    isc_m_load_string_stab:
      begin
        runtimestack_push(
          so_string_init_ucfs(
            template_stabentry(template_ip_operand,true),false));
      end;
    isc_m_load_int_oper:
      begin
        runtimestack_push(so_integer_init(template_ip_operand));
      end
    else
      put_internalerror(2011120701);
  end;
end;

procedure vmop_m_jumpaddr;
{template internal jumps,
  -> use template_ip_setrel(addr,true) which sets a jumpfix such that
     loading next instruction wont simple take the next, but the one we
     jumped.
     also it checks for rel addr in range of image and simply adds the rel addr.}
begin
  case template_ip_opcode of
    isc_m_jmp_addr:
      begin
        template_ip_setrel(template_ip_operand,true);
      end;
    isc_m_jmp_true_addr:
      begin
        if runtimestack_get(0) = so_true then
          template_ip_setrel(template_ip_operand,true);
      end;
    isc_m_jmp_false_nil_addr:
      begin
        if (runtimestack_get(0) = so_false) or
           (runtimestack_get(0) = so_nil) then
          template_ip_setrel(template_ip_operand,true);
      end;
    isc_m_jmppop_false_nil_addr:
      begin
        if (runtimestack_get(0) = so_false) or
           (runtimestack_get(0) = so_nil) then
          begin
            runtimestack_pop(1);
            template_ip_setrel(template_ip_operand,true);
          end;
      end;
    isc_m_tos1_true_rot_pop_jmp_addr:
      begin
        if runtimestack_get(1) = so_true then
          begin
            runtimestack_moved(0,1);
            runtimestack_pop(1);
            template_ip_setrel(template_ip_operand,true);
          end;
      end;
    isc_m_tos1_false_nil_rot_pop_jmp_addr:
      begin
        if (runtimestack_get(1) = so_false) or
           (runtimestack_get(1) = so_nil) then
          begin
            runtimestack_moved(0,1);
            runtimestack_pop(1);
            template_ip_setrel(template_ip_operand,true);
          end;
      end;
    else
      put_internalerror(2011120702);
  end;
end;

procedure vmop_m_envmod;
{set or get env entry}
begin
  case template_ip_opcode of
    isc_m_getenv_stab:
      begin
        globalenv_load(template_stabentry(template_ip_operand,false));
      end;
    isc_m_setenv_stab:
      begin
        globalenv_set_tos(template_stabentry(template_ip_operand,false));
      end;
    else
      put_internalerror(2011120704);
  end;
end;

procedure vmop_m_pop_nr;
{pop nr elements from object stack}
begin
  if template_ip_opcode <> isc_m_pop_nr then
    put_internalerror(2011120910);
  runtimestack_pop(template_ip_operand);
end;

procedure vmop_object_typecheck;
{hardcoded typecheck call, checks TOS against TOS.Cls.TypeQuery}
begin
  {check typenames}
  if ucfs_compare_a7(template_stabentry(template_ip_operand,false),
      Upcase(runtimestack_get(0)^.GetTypeCls^.TypeQuery(runtimestack_get(0)))) = 0 then
    runtimestack_push(so_true)
  else
    runtimestack_push(so_false);

  runtimestack_moved(0,1);
  runtimestack_pop(1);
end;

procedure vmop_listappend;
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

procedure vmop_m_echo;
{echo call, probably removed in future}
var substr: String;
    subr: String;
    pdict, res: PSOInstance;
    i,j,l: Integer;
begin
  case TInsMEchoOp(template_ip_operand) of
      mecho_echo:
        begin
          OutputWrite(so_any_flatstring(runtimestack_get(0)));
          runtimestack_pop(1);
        end;
      mecho_echonl:
        begin
          OutputWrite('',true);
        end;
      mecho_echoln:
        begin
          OutputWrite(so_any_flatstring(runtimestack_get(0)),true);
          runtimestack_pop(1);
        end;
      mecho_echofmt:
        begin
          // format string ... -> string,list
          put_internalerror(0);
        end;
      mecho_echosubst:
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
    else
      put_internalerror(2011120995);
  end;
end;

procedure vmop_m_declfun;
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

procedure vmop_m_fprel;
begin
  case template_ip_opcode of
    isc_m_fprel_set_slot:
      runtimestack_moved(0,runtimestack_fprel_to_sp(template_ip_operand));
    isc_m_fprel_load_slot:
      begin
        runtimestack_push(
          runtimestack_get(runtimestack_fprel_to_sp(template_ip_operand)));
        // need explicit incref, since neither get nor push incref (but po and ops will decref!)
        runtimestack_get(0)^.IncRef;
      end;
    else
      put_internalerror(2011121170);
  end;
end;

procedure vmop_m_returnclean;
begin
  {simply mv result slot @ FP+1 (where the function object resides)
   and clean up TOS..FP+2}
  runtimestack_moved(
    runtimestack_fprel_to_sp(template_ip_operand),
    runtimestack_fprel_to_sp(1));
  runtimestack_pop(runtimestack_fprel_to_sp(1));
  templatestack_pop;
end;

procedure vmop_m_stackdup;
var nr: Integer;
begin
  for nr := template_ip_operand downto 1 do
    begin
      runtimestack_push(so_nil); // create place
      runtimestack_moved(1,0);   // copy with correct reference counting
    end;
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

procedure vmop_compare;
begin
  case TInsSOCompareOp(template_ip_operand) of
    socompare_lt:
      if runtimestack_get(1)^.GetTypeCls^.Compare(
            runtimestack_get(1),
            runtimestack_get(0)) = socmp_isLess then
        runtimestack_push(so_true)
      else
        runtimestack_push(so_false);
    socompare_gt:
      if runtimestack_get(1)^.GetTypeCls^.Compare(
            runtimestack_get(1),
            runtimestack_get(0)) = socmp_isGreater then
        runtimestack_push(so_true)
      else
        runtimestack_push(so_false);
    socompare_le:
      if runtimestack_get(1)^.GetTypeCls^.Compare(
            runtimestack_get(1),
            runtimestack_get(0)) in [socmp_isEqual,socmp_isLess] then
        runtimestack_push(so_true)
      else
        runtimestack_push(so_false);
    socompare_ge:
      if runtimestack_get(1)^.GetTypeCls^.Compare(
            runtimestack_get(1),
            runtimestack_get(0)) in [socmp_isEqual,socmp_isGreater] then
        runtimestack_push(so_true)
      else
        runtimestack_push(so_false);
    socompare_equ:
      if runtimestack_get(1)^.GetTypeCls^.Compare(runtimestack_get(1),runtimestack_get(0)) = socmp_isEqual then
        runtimestack_push(so_true)
      else
        runtimestack_push(so_false);
    socompare_neq:
        if runtimestack_get(1)^.GetTypeCls^.Compare(runtimestack_get(1),runtimestack_get(0)) <> socmp_isEqual then
          runtimestack_push(so_true)
        else
          runtimestack_push(so_false);
    else
      put_internalerror(2011122040);
  end;
  runtimestack_moved(0,2);
  runtimestack_pop(2);
end;

procedure vmop_class_create;
{create class and push on tos}
begin
  runtimestack_push(
    so_class_create(ucfs_to_utf8string(template_stabentry(template_ip_operand,false)),
                    templatestack_tos,
                    template_ip));
end;

procedure vmop_method_decl;
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

procedure vmop_openout;
begin
  case TInsMOutputMode(template_ip_operand) of
    mout_open:
      begin
        if runtimestack_get(0)^.IsType(so_string_class) then
          outstack_push(so_string_get_utf8(runtimestack_get(0)),opfm_open)
        else
          put_critical('Expected String');
        runtimestack_pop(1);
      end;

    mout_reopen:
      begin
        if runtimestack_get(0)^.IsType(so_string_class) then
          outstack_push(so_string_get_utf8(runtimestack_get(0)),opfm_reopen)
        else
          put_critical('Expected String');
        runtimestack_pop(1);
      end;

    mout_path:
      begin
        if runtimestack_get(0)^.IsType(so_string_class) then
          outstack_push(so_string_get_utf8(runtimestack_get(0)),opfm_path)
        else
          put_critical('Expected String');
        runtimestack_pop(1);
      end;

    mout_close:
      begin
        if not outstack_pop then
          put_warning('No Outfile, Ignored Close.');
      end;
    else
      put_internalerror(12013100);
  end;
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

procedure vmop_object_operation_unary;
{call unary operation on so object}
begin
  case TInsSOUnaryOp(template_ip_operand) of
    sounop_abs: do_so_method_call(DEFAULT_METHOD_UnOpAbs_Hash,ci_us(ci_dm_abs,false),0);
    sounop_neg: do_so_method_call(DEFAULT_METHOD_UnOpNeg_Hash,ci_us(ci_dm_neg,false),0);
    sounop_not: do_so_method_call(DEFAULT_METHOD_UnOpNot_Hash,ci_us(ci_dm_not,false),0);
    else
      put_internalerror(2011122060);
  end;
end;

procedure vmop_object_operation_binary;
{call binary operation on so object}
begin
  case TInsSOBinaryOp(template_ip_operand) of
    sobinop_add: do_so_method_call(DEFAULT_METHOD_BinOpAdd_Hash,ci_us(ci_dm_add,false),1);
    sobinop_sub: do_so_method_call(DEFAULT_METHOD_BinOpSub_Hash,ci_us(ci_dm_sub,false),1);
    sobinop_mul: do_so_method_call(DEFAULT_METHOD_BinOpMul_Hash,ci_us(ci_dm_mul,false),1);
    sobinop_div: do_so_method_call(DEFAULT_METHOD_BinOpDiv_Hash,ci_us(ci_dm_div,false),1);
    sobinop_mod: do_so_method_call(DEFAULT_METHOD_BinOpMod_Hash,ci_us(ci_dm_mod,false),1);
    sobinop_shl: do_so_method_call(DEFAULT_METHOD_BinOpShl_Hash,ci_us(ci_dm_shl,false),1);
    sobinop_shr: do_so_method_call(DEFAULT_METHOD_BinOpShr_Hash,ci_us(ci_dm_shr,false),1);
    sobinop_rol: do_so_method_call(DEFAULT_METHOD_BinOpRol_Hash,ci_us(ci_dm_rol,false),1);
    sobinop_ror: do_so_method_call(DEFAULT_METHOD_BinOpRor_Hash,ci_us(ci_dm_ror,false),1);
    sobinop_and: do_so_method_call(DEFAULT_METHOD_BinOpAnd_Hash,ci_us(ci_dm_and,false),1);
    sobinop_or: do_so_method_call(DEFAULT_METHOD_BinOpOr_Hash,ci_us(ci_dm_or,false),1);
    sobinop_xor: do_so_method_call(DEFAULT_METHOD_BinOpXor_Hash,ci_us(ci_dm_xor,false),1);
    else
      put_internalerror(2011122061);
  end;
end;

procedure vmop_object_operation_setget;
begin
  case template_ip_opcode of
    isc_o_setm_stab:
      begin
        {need fix, currently its: <caller,value>, we need <caller,name,value> -> push stab entry
         and switch}
        runtimestack_push(so_string_init_ucfs(template_stabentry(template_ip_operand,true),false));
        runtimestack_switch(0,1);
        do_so_method_call(DEFAULT_METHOD_SetMember_Hash,ci_us(ci_dm_setmember,false),2);
      end;
    isc_o_getm_stab:
      begin
        {push stab entry}
        runtimestack_push(so_string_init_ucfs(template_stabentry(template_ip_operand,true),false));
        do_so_method_call(DEFAULT_METHOD_GetMember_Hash,ci_us(ci_dm_getmember,false),1);
      end;
    isc_o_seti_ign: do_so_method_call(DEFAULT_METHOD_SetIndex_Hash,ci_us(ci_dm_setindex,false),2);
    isc_o_geti_ign: do_so_method_call(DEFAULT_METHOD_GetIndex_Hash,ci_us(ci_dm_getindex,false),1);
    else
      put_internalerror(2011120970);
  end;
end;

procedure vmop_m_calltranslate;
var name: PUCFS32String;
begin
  case template_ip_opcode of
    isc_o_callm_nrops:
      begin
        {simple, no interface check since every so object has a methodcall
         interface}
        ASSERT( runtimestack_get(0)^.GetTypeCls = so_string_class ); // invalid stack layout
        check_so_maxargs(template_ip_operand);
        {$WARNING this should come from stab somehow}
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
    isc_o_callcall_nrops:
      begin
        check_so_maxargs(template_ip_operand);
        do_so_method_call(DEFAULT_METHOD_DirectCall_Hash,ci_us(ci_dm_call,false),template_ip_operand);
      end;
    else
      put_internalerror(2011121110);
  end;
end;

end.

