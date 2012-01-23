{   Unit for the virtual machine state

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

unit vmstate;

{$mode objfpc}{$H+}

interface

uses SysUtils, commontl, eomsg, ccache, opcode, socore, solnull, fpath;

(**
   NOTE.
     Bind Instances as early as possible to either Stack or Env (or temporary
     rt structures) since anything not bound will be gone on Collector Calls.
     So, either stop Collector Calls or make Creation..Binding
     "as atomic as possible".
   NOTE END.
 **)

(*******************************************************************************
  TRACED OBJECT ENVIRONMENT
 ******************************************************************************)

{set in global env, without using/objstack modification
  -> old val will be decrefed if existend
  -> i will be increfed twice due to stack behaviour
     (one ref for set, one ref for result arg)
  -> fixref DecRefs once for nonexisting result if true}
procedure globalenv_set( const s: String; i: PSOInstance; fixref: Boolean );
{set in global env, setenv(s,TOS), no objstack modification
  -> effectively GlobalEnvSet(s,i,true)}
procedure globalenv_set_tos( const s: String ); inline;

{load from global env, push on stack}
procedure globalenv_load( const s: String ); inline;

procedure init_globalenv;
procedure fini_globalenv;

(*******************************************************************************
  TRACED OBJECT STACK
 ******************************************************************************)

{push as TOS, does not Incref (since assumed that every result is already
 increfed for being result)}
procedure runtimestack_push( i: PSOInstance );

{grow the stackframe}
procedure runtimestack_framegrow( num: PtrInt );

{nil slot (decref and clean) and dec stackpointer}
procedure runtimestack_pop( n: PtrInt );

{move SP-sourcei at SP-desti,
  replaced object (SP-desti) is decrefed,
  duplicated (SP-sourcei) is increfed}
procedure runtimestack_moved( sourcei, desti: PtrInt ); inline;

{switch SP-sourcei with SP-desti}
procedure runtimestack_switch( sourcei, desti: PtrInt ); inline;

{get SP-oi, wont incref! so take care of the refcount}
function runtimestack_get( oi: PtrInt ): PSOInstance; inline;

{get SP-oi as argument field, used for SO calls}
function runtimestack_getargf( oi: PtrInt ): PSOMethodVarArgs; inline;

{return SP-oi}
function runtimestack_relptr( oi: PtrInt ): PtrInt; inline;

{return transform (FP+fpi) to SP-result for current SP}
function runtimestack_fprel_to_sp( fpi: PtrInt ): PtrInt; inline;

procedure init_runtimestack;
procedure fini_runtimestack;

(*******************************************************************************
  EXECUTION STATE (Position in Current Template)
 ******************************************************************************)

function template_ip: VMInt; inline;
procedure template_ip_set( aip: VMInt );
procedure template_ip_setrel( aip: VMInt; setjumpfix: Boolean ); inline;
function template_ip_next: Boolean; inline;
function template_stabentry( sidx: VMInt ): String;

function template_ip_opcode: TInsOpcodeCode; inline;
function template_ip_operand: VMInt; inline;
function template_ip_origline: VMInt; inline;
function template_ip_origcolumn: VMInt; inline;
function template_ip_shortpos: String; inline;
function template_ip_longpos: String; inline;

procedure init_template_ip;

(*******************************************************************************
  TEMPLATE FILE STACK / Execution State
 ******************************************************************************)

procedure init_template_stack;
procedure fini_template_stack;
function templatestack_checkok: Boolean; inline;
function templatestack_sp: VMInt; inline;
procedure templatestack_push( code: PCodeReference; aip: VMInt; isjmp: Boolean );
procedure templatestack_pop;
procedure templatestack_fix_framepointer( fp: VMInt );
function templatestack_tos: PCodeReference; inline;
function templatestack_fp: PtrInt; inline;

(*******************************************************************************
  HALT / ABORT STATE
 ******************************************************************************)

procedure init_halt_state;
function machine_halted: Boolean; inline;
function machine_aborted: Boolean; inline;
procedure machine_halt( code: Byte ); inline;
function machine_exitcode: Byte; inline;

(*******************************************************************************
  init & fin
 ******************************************************************************)

procedure machine_init;
procedure machine_fini;

implementation

uses appstart;

(*******************************************************************************
  TRACED OBJECT ENVIRONMENT
 ******************************************************************************)

var
  rtenv: PSOInstance;

procedure globalenv_set(const s: String; i: PSOInstance; fixref: Boolean);
var j: PSOInstance;
begin
  if s <> 'SYSTEM' then
    begin
      j := so_rtenv_set_member(rtenv,s,i,fixref);
      if i <> j then
        begin
          if j^.GetTypeCls = so_error_class then
            put_debug(so_error_get(j));
          put_internalerror(2011120530);
        end;
    end
  else
    put_critical('System Object is Protected - stop');
end;

procedure globalenv_set_tos(const s: String);
begin
  //WriteLn(StdErr,'envset ',s);
  globalenv_set(s,runtimestack_get(0),true);
end;

procedure globalenv_load(const s: String);
begin
  //WriteLn(StdErr,'evnload ',s);
  runtimestack_push(so_rtenv_get_member(rtenv,s));
end;

procedure init_globalenv;
begin
  rtenv := InitInstance(so_rtenv_class);
  so_rtenv_set_member(rtenv,'SYSTEM',so_system,true); // dont care about refs, system is immortal
end;

procedure fini_globalenv;
begin
  // KillImmortal(rtenv);
  rtenv := nil;
end;

(*******************************************************************************
  TRACED OBJECT STACK
 ******************************************************************************)

var
  rtstack_ptr: PtrInt;
  rtstack: PSOInstance;

procedure runtimestack_push(i: PSOInstance);
begin
  ASSERT(i<>nil);
  if i^.GetTypeCls = so_error_class then
   begin
     put_error('ABORT with Error: '+so_error_get(i));
     machine_halt(1);
   end;
  Inc(rtstack_ptr,1);
  check_vm_maxrtstack(rtstack_ptr);
  //WriteLn(StdErr,'rtstackpush ',i^.clsptr^.soclassname,' @',rtstack_ptr);
  if so_rtstack_getlength(rtstack) <= rtstack_ptr then
    so_rtstack_setlength(rtstack,
      so_rtstack_getlength(rtstack)+CL_LinearRegrow_Fast);
  so_rtstack_set(rtstack,i,rtstack_ptr);
end;

procedure runtimestack_framegrow(num: PtrInt);
begin
  {no cleaning (init with so_nil) needed on grow since
     1. pop cleans
     2. so_rtstack preinits on internal grow with nil (not so_nil) and returns
        so_nil on access to a nil slot}
  ASSERT(num>=0);
  Inc(rtstack_ptr,num);
  check_vm_maxrtstack(rtstack_ptr);
  if so_rtstack_getlength(rtstack) <= rtstack_ptr then
    so_rtstack_setlength(rtstack,
      so_rtstack_getlength(rtstack)+CL_LinearRegrow_Fast);
end;

procedure runtimestack_pop(n: PtrInt);
var i: PtrInt;
begin
  ASSERT(n>0);
  ASSERT((rtstack_ptr-n) >= -1);

  {release objects by assigning nil (derefs objects on stack)}
  for i := 0 to n-1 do
    begin
      //WriteLn(StdErr,'rtstackpop ',rtstack_ptr-i);
      so_rtstack_set(rtstack,nil,rtstack_ptr-i);
    end;
  {dec stack ptr and check for stack shrinking}
  Dec(rtstack_ptr,n);
  if rtstack_ptr <= (so_rtstack_getlength(rtstack)-(CL_LinearRegrow_Fast*2)) then
    so_rtstack_setlength(rtstack,
      so_rtstack_getlength(rtstack)-CL_LinearRegrow_Fast);
end;

procedure runtimestack_moved(sourcei, desti: PtrInt);
begin
  ASSERT((sourcei >= 0) and ((rtstack_ptr-sourcei) >= 0));
  ASSERT((desti >= 0) and ((rtstack_ptr-desti) >= 0));
  so_rtstack_move(rtstack,rtstack_ptr-sourcei,rtstack_ptr-desti);
end;

procedure runtimestack_switch(sourcei, desti: PtrInt);
begin
  ASSERT((sourcei >= 0) and ((rtstack_ptr-sourcei) >= 0));
  ASSERT((desti >= 0) and ((rtstack_ptr-desti) >= 0));
  so_rtstack_switch(rtstack,rtstack_ptr-sourcei,rtstack_ptr-desti);
end;

function runtimestack_get(oi: PtrInt): PSOInstance;
begin
  ASSERT((oi >= 0) and ((rtstack_ptr-oi) >= 0));
  ASSERT( so_rtstack_get(rtstack,rtstack_ptr-oi)^.GetRefs > 0  );
  Result := so_rtstack_get(rtstack,rtstack_ptr-oi);
end;

function runtimestack_getargf(oi: PtrInt): PSOMethodVarArgs;
begin
  ASSERT((oi >= 0) and ((rtstack_ptr-oi) >= 0));
  Result := so_rtstack_getargf(rtstack,rtstack_ptr-oi);
end;

function runtimestack_relptr(oi: PtrInt): PtrInt;
begin
  Result := rtstack_ptr-oi;
end;

function runtimestack_fprel_to_sp(fpi: PtrInt): PtrInt;
begin
  Result := rtstack_ptr-(templatestack_fp+fpi);
end;

procedure init_runtimestack;
begin
  rtstack := InitInstance(so_rtstack_class);
  so_rtstack_setlength(rtstack,CL_LinearRegrow_Fast);
  rtstack_ptr := -1;
end;

procedure fini_runtimestack;
begin
  // the following should be done, when rtstack is allowed in objects
  // -> (not only vm stack)
  // KillImmortal(rtstack);
  rtstack := nil;
end;

(*******************************************************************************
  EXECUTION STATE (Position in Current Template)
 ******************************************************************************)

var
  jmpfix: Boolean;
  tpl_ip: VMInt;

procedure init_template_ip;
begin
  jmpfix := false;
end;

function template_ip: VMInt;
begin
  Result := tpl_ip;
end;

procedure template_ip_set(aip: VMInt);
begin
  if tpl_ip < 0 then
    put_internalerror(2011121200);
  tpl_ip := aip;
  if tpl_ip > High(templatestack_tos^.pbcode^.image) then
    put_internalerror(2011120520);
  eomsg.set_currentlineinfo(
    templatestack_tos^.pbcode^.image[tpl_ip].GetLine,
    templatestack_tos^.pbcode^.image[tpl_ip].GetColumn
  );
end;

procedure template_ip_setrel(aip: VMInt; setjumpfix: Boolean );
begin
  template_ip_set(tpl_ip+aip);
  jmpfix := setjumpfix;
end;

function template_ip_next: Boolean;
var h: VMInt;
begin
  if not jmpfix then
    begin
      h := High(templatestack_tos^.pbcode^.image);
      Result := tpl_ip < h;
      if tpl_ip <= h then
        Inc(tpl_ip,1);
      if tpl_ip <= h then
        eomsg.set_currentlineinfo(
          templatestack_tos^.pbcode^.image[tpl_ip].GetLine,
          templatestack_tos^.pbcode^.image[tpl_ip].GetColumn
        );
    end
  else
    begin
      Result := true;
      jmpfix := false;
    end;
end;

function template_stabentry(sidx: VMInt): String;
begin
  if (sidx >= 0) and
     (sidx <= High(templatestack_tos^.pbcode^.stab)) then
    Result := templatestack_tos^.pbcode^.stab[sidx]
  else
    put_internalerror(2011120521);
end;

function template_ip_opcode: TInsOpcodeCode;
begin
  Result := templatestack_tos^.pbcode^.image[tpl_ip].GetOpcode;
end;

function template_ip_operand: VMInt;
begin
  Result := templatestack_tos^.pbcode^.image[tpl_ip].GetOperand;
end;

function template_ip_origline: VMInt;
begin
  Result := templatestack_tos^.pbcode^.image[tpl_ip].GetLine;
end;

function template_ip_origcolumn: VMInt;
begin
  Result := templatestack_tos^.pbcode^.image[tpl_ip].GetColumn;
end;

function template_ip_shortpos: String;
begin
  WriteStr(Result,templatestack_tos^.shortname,
    '.l',template_ip_origline,
    '.c',template_ip_origcolumn);
end;

function template_ip_longpos: String;
begin
  WriteStr(Result,templatestack_tos^.fullname,
    '.l',template_ip_origline,
    '.c',template_ip_origcolumn);
end;

(*******************************************************************************
  TEMPLATE FILE STACK / Execution State
 ******************************************************************************)

type
  TTemplateAddr = record
    code: PCodeReference;
    ip: VMInt;
    fp: VMInt;
  end;

var
  tpl_tos: PCodeReference;
  tpl_stackptr: VMInt;
  tpl_stack: array of TTemplateAddr;

procedure init_template_stack;
begin
  SetLength(tpl_stack,CL_LinearRegrow_Medium);
  tpl_stackptr := -1;
end;

procedure fini_template_stack;
begin
  SetLength(tpl_stack,0);
end;

function templatestack_checkok: Boolean;
begin
  Result := tpl_stackptr >= 0;
end;

function templatestack_sp: VMInt;
begin
  Result := tpl_stackptr;
end;

procedure templatestack_push(code: PCodeReference; aip: VMInt; isjmp: Boolean);
begin
  if templatestack_checkok then
    tpl_stack[tpl_stackptr].ip := template_ip;
  Inc(tpl_stackptr,1);
  if tpl_stackptr > High(tpl_stack) then
    SetLength(tpl_stack,Length(tpl_stack)+CL_LinearRegrow_Medium);
  tpl_tos := code;
  tpl_stack[tpl_stackptr].code := code;
  tpl_stack[tpl_stackptr].ip := aip;
  tpl_stack[tpl_stackptr].fp := rtstack_ptr;
  check_vm_maxtplstack(tpl_stackptr);
  eomsg.set_currentfile(code^.shortname);
  fpath_rel_replace(tpl_tos^.statfpath);
  if isjmp then
    begin
      template_ip_set(0);
      template_ip_setrel(aip,true);
    end
  else
    template_ip_set(aip);
  jmpfix := true;
end;

procedure templatestack_pop;
var fp: PtrInt;
begin
  if templatestack_checkok then
    begin
      {reset framepointer}
      fp := tpl_stack[tpl_stackptr].fp;
      {dec tpl_stack}
      dec(tpl_stackptr,1);
      if templatestack_checkok then
        begin
          tpl_tos := tpl_stack[tpl_stackptr].code;
          eomsg.set_currentfile(tpl_tos^.shortname);
          fpath_rel_replace(tpl_tos^.statfpath);
          template_ip_set(tpl_stack[tpl_stackptr].ip);
          if tpl_stackptr <= (Length(tpl_stack)-(CL_LinearRegrow_Medium*2)) then
            SetLength(tpl_stack,Length(tpl_stack)-CL_LinearRegrow_Medium);
          {check correctness of framepointer, as there are only 2 things
           that could make the templatestack pop.
           also check the instruction.
           1. include is done - rtstack_ptr = fp,
              instruction at ip is in CTPLIncludeOpcodes
           2. function call return, ret corrected the stack and
               rtstack_ptr = fp+1, since there is a return value,
              instruction at ip is in CTPLCallOpcodes}
          if template_ip_opcode in CTPLIncludeOpcodes then
            begin
              if fp <> rtstack_ptr then
                begin
                  put_error('SP is '+IntToStr(rtstack_ptr)+', should be '+IntToStr(fp));
                  put_internalerror(2011121114);
                end
            end
          else
            begin
              if (fp+1) <> rtstack_ptr  then
                begin
                  put_error('SP is '+IntToStr(rtstack_ptr)+', should be '+IntToStr(fp+1));
                  put_internalerror(2011121113);
                end;
            end;
        end
      else
        put_internalerror(2011120523); // <- bootblock popped? shouldn't happen either
    end
  else
    put_internalerror(2011120522); // <-- shouldn't happen
end;

procedure templatestack_fix_framepointer(fp: VMInt);
begin
  if (tpl_stackptr > 0) and
     (tpl_stack[tpl_stackptr-1].fp > fp) then
    put_internalerror(2011121120);
  tpl_stack[tpl_stackptr].fp := fp;
end;

function templatestack_tos: PCodeReference;
begin
  Result := tpl_tos;
end;

function templatestack_fp: PtrInt;
begin
  Result := tpl_stack[tpl_stackptr].fp;
end;

(*******************************************************************************
  HALT / ABORT STATE
 ******************************************************************************)

var
  MachineExitCode: Byte;
  MachineAborted: Boolean;
  MachineHalted: Boolean;

procedure init_halt_state;
begin
  MachineExitCode := High(Byte);
  MachineAborted := false;
  MachineHalted := false;
end;

function machine_halted: Boolean;
begin
  Result := MachineHalted;
end;

function machine_aborted: Boolean;
begin
  Result := MachineAborted;
end;

procedure machine_halt(code: Byte);
begin
  MachineExitCode := code;
  MachineHalted := true;
  MachineAborted := code <> 0;
end;

function machine_exitcode: Byte;
begin
  Result := MachineExitCode;
end;

(*******************************************************************************
  VMState IntroSpection
 ******************************************************************************)

function _MachineState_Meth_RelFrameType( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
{return Frame Creation Type (either call/inclusion/current [current for last frame])}
begin
  if (argnum = 1) and
     (soargs^[0]^.GetTypeCls = so_integer_class) and
     so_integer_fits(soargs^[0]) and
     (so_integer_get(soargs^[0],true) >= 0) and
     (so_integer_get(soargs^[0],true) <= tpl_stackptr) then
    begin
      if so_integer_get(soargs^[0],true) <= 0 then
        Result := so_string_init('CURRENT')
      else
        begin
          if tpl_stack[tpl_stackptr-so_integer_get(soargs^[0],true)].code^.pbcode^.image[
            tpl_stack[tpl_stackptr-so_integer_get(soargs^[0],true)].ip ].GetOpcode in CTPLIncludeOpcodes then
            Result := so_string_init('INCLUSION')
          else
            Result := so_string_init('CALL');
        end;
    end
  else
    Result := nil;
end;

function _MachineState_Meth_RelFrameShortName( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if (argnum = 1) and
     (soargs^[0]^.GetTypeCls = so_integer_class) and
     so_integer_fits(soargs^[0]) and
     (so_integer_get(soargs^[0],true) >= 0) and
     (so_integer_get(soargs^[0],true) <= tpl_stackptr) then
    begin
      Result := so_string_init(tpl_stack[tpl_stackptr-so_integer_get(soargs^[0],true)].code^.shortname);
    end
  else
    Result := nil;
end;

function _MachineState_Meth_RelFrameLongName( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if (argnum = 1) and
     (soargs^[0]^.GetTypeCls = so_integer_class) and
     so_integer_fits(soargs^[0]) and
     (so_integer_get(soargs^[0],true) >= 0) and
     (so_integer_get(soargs^[0],true) <= tpl_stackptr) then
    begin
      Result := so_string_init(tpl_stack[tpl_stackptr-so_integer_get(soargs^[0],true)].code^.fullname);
    end
  else
    Result := nil;
end;

function _MachineState_Meth_RelFrameLine( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if (argnum = 1) and
     (soargs^[0]^.GetTypeCls = so_integer_class) and
     so_integer_fits(soargs^[0]) and
     (so_integer_get(soargs^[0],true) >= 0) and
     (so_integer_get(soargs^[0],true) <= tpl_stackptr) then
    begin
      Result := so_integer_init(tpl_stack[tpl_stackptr-so_integer_get(soargs^[0],true)].code^.pbcode^.image[
                 tpl_stack[tpl_stackptr-so_integer_get(soargs^[0],true)].ip ].GetLine);
    end
  else
    Result := nil;
end;

function _MachineState_Meth_RelFrameColumn( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if (argnum = 1) and
     (soargs^[0]^.GetTypeCls = so_integer_class) and
     so_integer_fits(soargs^[0]) and
     (so_integer_get(soargs^[0],true) >= 0) and
     (so_integer_get(soargs^[0],true) <= tpl_stackptr) then
    begin
      Result := so_integer_init(tpl_stack[tpl_stackptr-so_integer_get(soargs^[0],true)].code^.pbcode^.image[
                 tpl_stack[tpl_stackptr-so_integer_get(soargs^[0],true)].ip ].GetColumn);
    end
  else
    Result := nil;
end;

function _MachineState_Attr_StackFrames( const aname: String; soself: PSOInstance; setter: PSOInstance ): PSOInstance;
begin
  if not Assigned(setter) then
    Result := so_integer_init( tpl_stackptr+1 )
  else
    Result := nil;
end;

procedure StateIntroSpectionStartup( var disptab: TDispTables; out data: Pointer );
begin
  RegisterSubSystemAttribute(disptab,'Frames',@_MachineState_Attr_StackFrames);
  RegisterSubSystemMethod(disptab,'RelFrameType',@_MachineState_Meth_RelFrameType);
  RegisterSubSystemMethod(disptab,'RelFrameShortName',@_MachineState_Meth_RelFrameShortName);
  RegisterSubSystemMethod(disptab,'RelFrameLongName',@_MachineState_Meth_RelFrameLongName);
  RegisterSubSystemMethod(disptab,'RelFrameLine',@_MachineState_Meth_RelFrameLine);
  RegisterSubSystemMethod(disptab,'RelFrameColumn',@_MachineState_Meth_RelFrameColumn);
  data := nil;
end;

procedure RegisterStateIntroSpection;
begin
  solnull.RegisterSubSystemHandler('MachineState',@StateIntroSpectionStartup,nil,false);
end;

(*******************************************************************************
  init & fin
 ******************************************************************************)

procedure machine_init;
begin
  socore_init;
  RegisterLevel0Types;
  init_halt_state;
  init_template_ip;
  init_template_stack;
  init_runtimestack;
  init_globalenv;
  appstart.RegisterInit(@RegisterStateIntroSpection);
end;

procedure machine_fini;
begin
  fini_globalenv;
  fini_runtimestack;
  fini_template_stack;
  socore_done;
end;

initialization
  tpl_tos := nil;

end.

