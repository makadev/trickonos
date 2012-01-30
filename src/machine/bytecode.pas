{   Unit for bytecode reading/writing and basic declarations

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

unit bytecode;

{$MODE OBJFPC}{$H+}

{$PACKRECORDS 1}

interface

uses Sysutils, Classes, commontl, opcode, ucfs;

const
  CBCCompatCode = 1;
  CBCMagic = $ACADEEE;

type
(* Bytecode Header *)
  PBCHeader = ^TBCHeader;
  TBCHeader = packed record
    magic: VMWord;
    compat: VMWord;
    incmode: VMInt;
    stab_entries: VMInt;
    code_entries: VMInt;
    appendix_size: VMInt;
  end;

(* Code Block *)

  TByteCodeImage = packed array of TInstructionPackage;

(* Bytecode Reader *)
 
  TBCLErrorCode = (
    bcec_validated,      // everythings ok so far
    bcec_noncompat,      // compat code missmatch
    bcec_brockenheader,  // early header end, addresses out of range ...
    bcec_invalidincl,    // inclusion mode missmatch
    bcec_brokenstab,     // early stab end, bytes missmatch...
    bcec_brokencode,     // early code end, wrong jumps...
    bcec_invalidopcode,  // instr. validation failed
    bcec_stablimit,
    bcec_codelimit,
    bcec_bclimit
  );

  PByteCodeBlock = ^TByteCodeBlock;
  TByteCodeBlock = object
    inclmode: TInsMIncludeMode;
    stab: array of PUCFS32String;
    image: TByteCodeImage;
    bcsize: VMInt;
    procedure Init;
    function LoadDecode( AStream: TStream ): TBCLErrorCode;
    procedure WriteEncode( AStream: TStream );
    procedure Done;

    function StabAdd(s: PUCFS32String): VMInt;
    function OpcodeAdd: VMInt;
  end;

implementation

{ TByteCodeBlock }

procedure TByteCodeBlock.Init;
begin
  SetLength(stab,0);
  SetLength(image,0);
  bcsize := 0;
end;

function TByteCodeBlock.LoadDecode(AStream: TStream): TBCLErrorCode;
var bchead: TBCHeader;
    i: VMInt;
begin
  Result := bcec_validated;

  {check header magic}
  if (SizeOf(TBCHeader) <>
      AStream.Read(bchead,SizeOf(TBCHeader))) or
     (bchead.magic <> CBCMagic) then
    Exit(bcec_brockenheader);

  {check compat code}
  if bchead.compat <> CBCCompatCode then
    Exit(bcec_noncompat);

  {check incl mode range}
  if (bchead.incmode <= Ord(mincl_invalid)) or
     (bchead.incmode >= Ord(mincl_terminator)) then
    Exit(bcec_invalidincl);
  inclmode := TInsMIncludeMode(bchead.incmode);

  {check stab entry cnt}
  if not check_bc_maxstab(bchead.stab_entries) then
    Exit(bcec_stablimit);

  {check code entry cnt}
  if not check_bc_maxcode(bchead.code_entries) then
    Exit(bcec_codelimit);

  {load stab and check bc size}
  SetLength(stab, bchead.stab_entries);
  Inc(bcsize,SizeOf(TInstructionPackage)*bchead.code_entries+
             SizeOf(LongInt)*bchead.stab_entries);
  try
    for i := 0 to High(stab) do
      begin
        stab[i] := ucfs_utf8us(AStream.ReadAnsiString);
        Inc(bcsize,ucfs_length(stab[i]));
        if not check_bc_maxsize(bcsize) then
          begin
            Result := bcec_bclimit;
            break;
          end;
      end;
  except on e: Exception do
    Result := bcec_brokenstab;
  end;

  if Result <> bcec_validated then
    begin
      SetLength(stab,0);
      Exit(Result);
    end;

  {load code}
  SetLength(image, bchead.code_entries);
  if AStream.Read(image[0],bchead.code_entries*SizeOf(TInstructionPackage)) <>
     (bchead.code_entries*SizeOf(TInstructionPackage)) then
    Result := bcec_brokencode;

  if Result <> bcec_validated then
    begin
      SetLength(stab,0);
      SetLength(image,0);
      Exit(Result);
    end;

  {check simple opcode faults}
  for i := 0 to High(image) do
    if not image[i].IsValid then
      begin
        SetLength(stab,0);
        SetLength(image,0);
        Exit(bcec_invalidopcode);
      end;

  {$WARNING needs further verification, like stabindex/image addr range checks}
end;

procedure TByteCodeBlock.WriteEncode(AStream: TStream);
var bchead: TBCHeader;
    i: VMInt;
begin
  with bchead do
    begin
      magic := VMWord(CBCMagic);
      compat := CBCCompatCode;
      incmode := Ord(inclmode);
      stab_entries := Length(stab);
      code_entries := Length(image);
      appendix_size := 0;
    end;
  AStream.Write(bchead,SizeOf(TBCHeader));
  for i := 0 to High(stab) do
    AStream.WriteAnsiString(ucfs_to_utf8string(stab[i]));
  AStream.Write(image[0],Length(image)*SizeOf(TInstructionPackage));
end;

procedure TByteCodeBlock.Done;
var i: VMInt;
begin
  SetLength(image,0);
  for i := 0 to High(stab) do
    ucfs_release(stab[i]);
  SetLength(stab,0);
end;

function TByteCodeBlock.StabAdd(s: PUCFS32String): VMInt;
begin
  if check_bc_maxstab(Length(stab)+1) then
    begin
      SetLength(stab,Length(stab)+1);
      stab[High(stab)] := ucfs_incref(s);
      Result := High(stab);
      Inc(bcsize,ucfs_length(s)+SizeOf(LongInt));
    end
  else
    Result := -1;
end;

function TByteCodeBlock.OpcodeAdd: VMInt;
begin
  if check_bc_maxcode(Length(image)+1) then
    begin
      SetLength(image,Length(image)+1);
      Result := High(image);
      Inc(bcsize,SizeOf(TInstructionPackage));
    end
  else
    Result := -1;
end;

end.
