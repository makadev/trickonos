{   Unit for virtual machine opcodes

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

unit opcode;

{$MODE OBJFPC}{$H+}

interface

uses Sysutils, commontl, eomsg;

type
  {inclusion mode}
  TInsMIncludeMode = (
    mincl_invalid = 0,

    mincl_include,
    mincl_use,

    mincl_terminator
  );

  {empty type loading}
  TInsSOLoad = (
    soload_invalid = 0,

    soload_nil,
    soload_true,
    soload_false,
    soload_list,
    soload_dict,

    soload_terminator
  );

  {echo operations}
  TInsMEchoOp = (
    mecho_invalid = 0,

    mecho_echo,
    mecho_echoln,
    mecho_echonl,
    mecho_echofmt,
    mecho_echosubst,

    mecho_terminator
  );

  {unary operations with 0 fixed arg (self not accounted), no vararg}
  TInsSOUnaryOp = (
    sounop_invalid = 0,

    sounop_abs,
    sounop_neg,
    sounop_not,

    sounop_terminator
  );

  {binary operations with 1 fixed arg (self not accounted), no vararg}
  TInsSOBinaryOp = (
    sobinop_invalid = 0,

    sobinop_add,
    sobinop_sub,
    sobinop_mul,
    sobinop_div,
    sobinop_mod,
    sobinop_shl,
    sobinop_shr,
    sobinop_rol,
    sobinop_ror,
    sobinop_xor,

    sobinop_and,
    sobinop_or,

    sobinop_terminator
  );

  TInsSOCompareOp = (
    socompare_invalid = 0,

    socompare_lt,
    socompare_gt,
    socompare_le,
    socompare_ge,
    socompare_equ,
    socompare_neq,

    socompare_terminator
  );

  TInsOpcodeCode = (
    isc_m_nop_ign = 0,

    {duplicate TOS}
    isc_m_stackdup_nr,

    {write}
    isc_m_echo,

    {pop nr elements from objectstack, nr >= 1 !}
    isc_m_pop_nr,

    {include on stab (needed for bootstrapping)}
    isc_m_include_stab,

    {include on TOS}
    isc_m_include,

    {halt machine with exitcode 0}
    isc_m_halt_ign,

    {puts -> output handler}
    isc_m_puts_stab,

    {jmp at addr}
    isc_m_jmp_addr,

    {jmp at addr, when TOS:BOOL and TOS=true}
    isc_m_jmp_true_addr,
    {jmp at addr, when TOS:BOOL and TOS=false or nil}
    isc_m_jmp_false_nil_addr,

    {pop,jmp at addr, when TOS:BOOL and TOS=false or nil}
    isc_m_jmppop_false_nil_addr,

    {jmp at addr, pop 2, push prev. TOS, when TOS-1 = false or nil}
    isc_m_tos1_false_nil_rot_pop_jmp_addr,
    {jmp at addr, pop 2, push prev. TOS, when TOS-1 = true}
    isc_m_tos1_true_rot_pop_jmp_addr,

    {load initial type -> push new TOS}
    isc_m_load_type,

    {call env.set(stab[operand],TOS) -> pop 1, push new TOS}
    isc_m_setenv_stab,
    {call env.get(stab[operand]) -> push new TOS}
    isc_m_getenv_stab,

    {call setmember(stab[operand], TOS-1, TOS) -> pop 2, push new TOS}
    isc_o_setm_stab,
    {call getmember(stab[operand], TOS) -> pop 2, push new TOS}
    isc_o_getm_stab,
    {call setindex(TOS-2, TOS-1, TOS) -> pop 3, push new TOS}
    isc_o_seti_ign,
    {call getindex(TOS-1, TOS) -> pop 2, push new TOS}
    isc_o_geti_ign,
    {calling ops, taking n Operands (TOS-n..TOS) -> pop n, push new TOS}
    isc_o_callcall_nrops,
    isc_o_callm_nrops,

    {call method binop(TOS-1, TOS) -> pop 2, push new TOS}
    isc_o_binaryop,

    {call method unop(TOS) -> pop 1, push new TOS}
    isc_o_unaryop,

    {do list append, special as its not a root method}
    isc_o_listappend_nr,

    {special, check classname and push true or false}
    isc_o_typecheck_stab,

    {load&convert stab entry to int -> push new TOS}
    isc_m_load_int_stab,

    {load&convert integer from operand (only 32bit!)}
    isc_m_load_int_oper,

    {load&convert stab entry to string -> push new TOS}
    isc_m_load_string_stab,

    {move TOS at FP+Operand (slotset)}
    isc_m_fprel_set_slot,

    {push FP+Operand (slotget)}
    isc_m_fprel_load_slot,

    {return, clean frame and make slot TOS (result)}
    isc_m_ret_slot,

    {declare a function with code entrypoint addr,
      TOS-4=ARGF,TOS-3=ARGN,TOS-2=SLOTN,TOS-1=CVAR,TOS=Name(string)}
    isc_m_decl_fun_addr,

    {compare call (builtin)}
    isc_o_compare,

    isc_invalid
  );

type
  PInstructionPackage = ^TInstructionPackage;
  TInstructionPackage = packed object
    private
      opcode: VMInt;
      operand: VMInt;
      origline: VMInt;
      origcolumn: VMInt;
    protected
      procedure Sanitize;
    public
      procedure Invalidate;
      function IsValid( ASanitize: Boolean = true ): Boolean; inline;

      function Decode( const buf ): Boolean;
      
      {opcode}
      function GetOpcode: TInsOpcodeCode; inline;
      procedure SetOpcode( opc: TInsOpcodeCode ); inline;
      
      {operand}
      procedure ClearOperand; inline;

      function GetOperand: VMInt; inline;
      procedure SetOperand( i: VMInt ); inline;

      {lineinfo}
      function GetLine: VMInt; inline;
      function GetColumn: VMInt; inline;
      procedure SetLineInfo( l,c: VMInt ); inline;

      {for debugging n stuff}
      function ASMCode: String;
  end;

const
  // for code/frame stack return checks,
  // Opcodes that change the code stack but not the frame
  CTPLIncludeOpcodes = [isc_m_include,isc_m_include_stab];
  // for code/frame stack return checks,
  // Opcodes that can change the code stack and the frame (1 result value)
  CTPLCallOpcodes = [
    isc_o_callcall_nrops,
    isc_o_callm_nrops];

implementation

function TInstructionPackage.IsValid(ASanitize: Boolean): Boolean;
begin
  if ASanitize then
    Sanitize;
  Result := opcode < Ord(isc_invalid);
end;

procedure TInstructionPackage.Invalidate;
begin
  opcode := Ord(isc_invalid);
  operand := 0;
end;

procedure TInstructionPackage.Sanitize;
begin
  if opcode >= Ord(isc_invalid) then
    begin
      Invalidate;
      Exit;
    end;

  case TInsOpcodeCode(opcode) of
    isc_m_echo: if (operand <= Ord(mecho_invalid)) or
                   (operand >= Ord(mecho_terminator)) then Invalidate;

    isc_m_nop_ign,
    isc_m_halt_ign,
    isc_o_seti_ign,
    isc_o_geti_ign: if operand <> 0 then Invalidate;

    isc_m_pop_nr:  if operand <= 0 then Invalidate;

    isc_o_compare: if (operand <= Ord(socompare_invalid)) or
                      (operand >= Ord(socompare_terminator)) then Invalidate;

    isc_o_unaryop: if (operand <= Ord(sounop_invalid)) or
                      (operand >= Ord(sounop_terminator)) then Invalidate;

    isc_o_binaryop: if (operand <= Ord(sobinop_invalid)) or
                       (operand >= Ord(sobinop_terminator)) then Invalidate;

    isc_m_load_type: if (operand <= Ord(soload_invalid)) or
                        (operand >= Ord(soload_terminator)) then Invalidate;

    isc_m_include: if (operand <= Ord(mincl_invalid)) or
                      (operand >= Ord(mincl_terminator)) then Invalidate;

    isc_m_fprel_set_slot,
    isc_m_fprel_load_slot,
    isc_m_ret_slot: if operand > CL_SO_MaxFrame then Invalidate;

    isc_m_decl_fun_addr,
    isc_m_jmp_addr,
    isc_m_jmp_true_addr,
    isc_m_jmp_false_nil_addr,
    isc_m_jmppop_false_nil_addr,
    isc_m_tos1_false_nil_rot_pop_jmp_addr,
    isc_m_tos1_true_rot_pop_jmp_addr: if operand = 0 then Invalidate; {no to microloops}

    isc_m_stackdup_nr: if (operand <= 0) or
                          (operand > 16) then Invalidate; {actually, 1 should be enough}

    isc_o_listappend_nr,
    isc_o_typecheck_stab,
    isc_m_puts_stab,
    isc_m_setenv_stab,
    isc_m_getenv_stab,
    isc_o_setm_stab,
    isc_o_getm_stab,
    isc_o_callcall_nrops,
    isc_o_callm_nrops,
    isc_m_load_int_stab,
    isc_m_load_string_stab,
    isc_m_include_stab: if operand < 0 then Invalidate; {31bit operand [32bit positive]}

    isc_m_load_int_oper: {32bit operand};
    else
      put_internalerror(2011120402);
  end;
end;

function TInstructionPackage.Decode(const buf): Boolean;
begin
  Move(buf,self.opcode,SizeOf(TInstructionPackage));
  Result := IsValid;
end;

function TInstructionPackage.GetOpcode: TInsOpcodeCode;
begin
  Result := TInsOpcodeCode(opcode);
end;

procedure TInstructionPackage.SetOpcode(opc: TInsOpcodeCode);
begin
  opcode := Ord(opc);
end;

procedure TInstructionPackage.ClearOperand;
begin
  operand := 0;
end;

function TInstructionPackage.GetOperand: VMInt;
begin
  Result := operand;
end;

procedure TInstructionPackage.SetOperand(i: VMInt);
begin
  operand := i;
end;

function TInstructionPackage.GetLine: VMInt;
begin
  Result := origline;
end;

function TInstructionPackage.GetColumn: VMInt;
begin
  Result := origcolumn;
end;

procedure TInstructionPackage.SetLineInfo(l, c: VMInt);
begin
  origline := l;
  origcolumn := c;
end;

function TInstructionPackage.ASMCode: String;
begin
  case TInsOpcodeCode(opcode) of
    isc_invalid: Result := 'Invalid Opcode';
    isc_m_stackdup_nr:
      Result := 'DUP TOS x'+IntToStr(operand);
    isc_m_echo:
      begin
        Result := 'ECHO';
        case TInsMEchoOp(operand) of
          mecho_echo:
            Result := Result + '.Write(TOS), pop 1';
          mecho_echonl:
            Result := Result + '.WriteLn()';
          mecho_echoln:
            Result := Result + '.WriteLn(TOS), pop 1';
          mecho_echofmt:
            Result := Result + '.Format(TOS,TOS-1), pop 2';
          mecho_echosubst:
            Result := Result + '.Substitute(TOS,TOS-1), pop 2';
          else
            Result := Result + '.Invalid';
        end;
      end;
    isc_m_nop_ign: Result := 'NOP';
    isc_m_halt_ign: Result := 'HALT(TOS)';
    isc_o_seti_ign: Result := 'CALL (TOS-2)[TOS-1] := TOS, pop 3, push result';
    isc_o_geti_ign: Result := 'CALL (TOS-1)[TOS], pop 2, push result';
    isc_m_pop_nr:  Result := 'POP TOS..TOS-'+IntToStr(operand-1);
    isc_o_unaryop:
      begin
        Result := 'UnaryOpCall.Invalid';
        case TInsSOUnaryOp(operand) of
          sounop_abs: Result := 'CALL TOS.ABS(), pop 1, push result';
          sounop_neg: Result := 'CALL TOS.NEG(), pop 1, push result';
          sounop_not: Result := 'CALL TOS.NOT(), pop 1, push result';
        end;
      end;

    isc_o_binaryop:
      begin
        Result := 'BinaryOpCall.Invalid';
        case TInsSOBinaryOp(operand) of
          sobinop_add: Result := 'CALL (TOS-1).ADD(TOS), pop 2, push result';
          sobinop_sub: Result := 'CALL (TOS-1).SUB(TOS), pop 2, push result';
          sobinop_mul: Result := 'CALL (TOS-1).MUL(TOS), pop 2, push result';
          sobinop_div: Result := 'CALL (TOS-1).DIV(TOS), pop 2, push result';
          sobinop_mod: Result := 'CALL (TOS-1).MOD(TOS), pop 2, push result';
          sobinop_shl: Result := 'CALL (TOS-1).SHIFT_LEFT(TOS), pop 2, push result';
          sobinop_shr: Result := 'CALL (TOS-1).SHIFT_RIGHT(TOS), pop 2, push result';
          sobinop_rol: Result := 'CALL (TOS-1).ROTATE_LEFT(TOS), pop 2, push result';
          sobinop_ror: Result := 'CALL (TOS-1).ROTATE_RIGHT(TOS), pop 2, push result';
          sobinop_and: Result := 'CALL (TOS-1).AND(TOS), pop 2, push result';
          sobinop_or: Result := 'CALL (TOS-1).OR(TOS), pop 2, push result';
          sobinop_xor: Result := 'CALL (TOS-1).XOR(TOS), pop 2, push result';
        end;
      end;

    isc_o_compare:
      begin
        Result := 'Compare.Invalid';
        case TInsSOCompareOp(operand) of
          socompare_lt: Result := 'COMPARE (TOS-1) < (TOS), pop 2, push result';
          socompare_gt: Result := 'COMPARE (TOS-1) > (TOS), pop 2, push result';
          socompare_le: Result := 'COMPARE (TOS-1) <= (TOS), pop 2, push result';
          socompare_ge: Result := 'COMPARE (TOS-1) >= (TOS), pop 2, push result';
          socompare_equ: Result := 'COMPARE (TOS-1) = (TOS), pop 2, push result';
          socompare_neq: Result := 'COMPARE (TOS-1) <> (TOS), pop 2, push result';
        end;
      end;

    isc_m_load_type:
      begin
        Result := 'LoadType.Invalid';
        case TInsSOLoad(operand) of
          soload_nil: Result := 'PUSH nil';
          soload_true: Result := 'PUSH true';
          soload_false: Result := 'PUSH false';
          soload_list: Result := 'PUSH []';
          soload_dict: Result := 'PUSH {}';
        end;
      end;

    isc_m_include:
      begin
        Result := 'Include.Invalid';
        case TInsMIncludeMode(operand) of
          mincl_include: Result := 'CODE := LOAD(TOS) as Template, pop 1, EXEC CODE';
          mincl_use: Result := 'CODE := LOAD(TOS) as Code Unit, pop 1, EXEC CODE';
        end;
      end;

    isc_m_fprel_set_slot:
      Result := 'FRAME_SLOT['+IntToStr(operand)+'] := TOS';
    isc_m_fprel_load_slot:
      Result := 'PUSH FRAME_SLOT['+IntToStr(operand)+']';
    isc_m_ret_slot:
      Result := 'RETURN from call, push FRAME_SLOT['+IntToStr(operand)+'] as result';

    isc_m_jmp_addr:
      Result := 'GOTO IP+('+IntToStr(operand)+')';
    isc_m_jmp_true_addr:
      Result := 'IF (TOS=true), GOTO IP+('+IntToStr(operand)+')';
    isc_m_jmp_false_nil_addr:
      Result := 'IF (TOS in [false,nil]), GOTO IP+('+IntToStr(operand)+')';
    isc_m_jmppop_false_nil_addr:
      Result := 'IF (TOS in [false,nil]), pop 1, GOTO IP+('+IntToStr(operand)+')';
    isc_m_tos1_false_nil_rot_pop_jmp_addr:
      Result := 'IF (TOS in [false,nil]), switch(TOS,TOS-1), pop 1, GOTO IP+('+IntToStr(operand)+')';
    isc_m_tos1_true_rot_pop_jmp_addr:
      Result := 'IF (TOS=true), switch(TOS,TOS-1), pop 1, GOTO IP+('+IntToStr(operand)+')';

    isc_m_load_int_oper:
      Result := 'PUSH int '+IntToStr(operand);
    isc_m_decl_fun_addr:
      Result := 'DECL FUN [stab[TOS], ARGF TOS-4, ARGN TOS-3, SLOTN TOS-2, VARARG TOS-1] @ IP+('+IntToStr(operand)+'), pop 5';
    isc_o_listappend_nr:
      Result := 'CALL (TOS-'+IntToStr(operand)+').APPEND(TOS-'+IntToStr(operand-1)+'..TOS), pop '+
        IntToStr(operand)+', push result';
    isc_o_typecheck_stab:
      Result := 'TYPECHECK (TOS is TYPE(stab['+IntToStr(operand)+'])), pop 1, push result';
    isc_m_puts_stab:
      Result := 'ECHO.Write(stab['+IntToStr(operand)+'])';
    isc_m_setenv_stab:
      Result := 'ENV[stab['+IntToStr(operand)+']] := TOS';
    isc_m_getenv_stab:
      Result := 'PUSH ENV[stab['+IntToStr(operand)+']]';
    isc_o_setm_stab:
      Result := 'CALL (TOS-1).SETMEMBER(stab['+IntToStr(operand)+'],TOS), pop 2, push result';
    isc_o_getm_stab:
      Result := 'CALL TOS.GETMEMBER(stab['+IntToStr(operand)+']), pop 1, push result';
    isc_o_callcall_nrops:
      if operand > 0 then
        Result := 'CALL (TOS-'+IntToStr(operand)+').CALL(TOS-'+IntToStr(operand-1)+'..TOS), pop '+
        IntToStr(operand)+', push result'
      else
        Result := 'CALL (TOS-'+IntToStr(operand)+').CALL(), pop '+IntToStr(operand)+', push result';
    isc_o_callm_nrops:
      if operand > 0 then
        Result := 'POP NAME, METHODCALL (TOS-'+IntToStr(operand)+').NAME(TOS-'+IntToStr(operand-1)+'..TOS), pop '+
        IntToStr(operand)+', push result'
      else
        Result := 'POP NAME, METHODCALL (TOS-'+IntToStr(operand)+').NAME(), pop '+IntToStr(operand)+', push result';
    isc_m_load_int_stab:
      Result := 'PUSH STR_TO_INT(stab['+IntToStr(operand)+'])';
    isc_m_load_string_stab:
      Result := 'PUSH stab['+IntToStr(operand)+']';
    isc_m_include_stab:
      Result := 'CODE := LOAD(stab['+IntToStr(operand)+']) as Template, pop 1, EXEC CODE';
  else
    Result := 'UNKNOWN OPCODE';
  end;
end;

end.
