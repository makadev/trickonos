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
  TInsOpcodeCode = (
    isc_nop_ign = 0,
{$I ins_enum.inc}
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
  CTPLIncludeOpcodes = [isc_include_ign,isc_use_ign,isc_include_stab];
  // for code/frame stack return checks,
  // Opcodes that can change the code stack and the frame (1 result value)
  CTPLCallOpcodes = [
    isc_callcall_opern,
    isc_callm_opern];

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
  if (opcode >= Ord(isc_invalid)) or
     (opcode < 0) then
    begin
      Invalidate;
      Exit;
    end;

  case TInsOpcodeCode(opcode) of
{$I ins_type_ign.inc},
    isc_nop_ign:
    if operand <> 0 then Invalidate;

{$I ins_type_opern.inc}:
    if operand < 0 then Invalidate;

{$I ins_type_slot.inc}:
    if (operand > CL_SO_MaxFrame) or
       (operand < 0) then Invalidate;

{$I ins_type_addr.inc}:
    if operand = 0 then Invalidate; {no to microloops/decl on same pos}

{$I ins_type_stab.inc}:
    if operand < 0 then Invalidate;

{$I ins_type_operi.inc}: {32bit operand -- ignored};
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
    isc_nop_ign: Result := 'NOP';
{$I ins_asmout.inc}
  else
    Result := 'UNKNOWN OPCODE';
  end;
end;

end.
