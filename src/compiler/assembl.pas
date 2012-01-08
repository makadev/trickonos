{   Unit for bytecode assembler

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

unit assembl;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, commontl, eomsg, coreobj, bytecode, opcode;

type
  TAssemblyNode = class
    private
      FNext: TAssemblyNode;
      FPrev: TAssemblyNode;
    protected
      procedure Insert( ANext, APrev: TAssemblyNode );
    public
      constructor Create;
  end;

  TLabelNode = class(TAssemblyNode)
    private
      FLabIdent: VMInt;
    public
      property LabIdent: VMInt read FLabIdent;
      constructor Create;
  end;

  TInstrNode = class(TAssemblyNode)
    public
      Ins: TInstructionPackage;
      constructor Create;
  end;

  TLRefInstrNode = class(TInstrNode)
  end;

  TAssembly = class
    private
      FFirst: TAssemblyNode;
      FLast: TAssemblyNode;
      procedure AppendNewIns;
      procedure AppendNewLRefIns;
    public
      constructor Create;
      destructor Destroy; override;

      procedure AppendAssembly( AAssembly: TAssembly );

      function FirstAsIns: TInstrNode;
      function FirstAsLab: TLabelNode;
      function FirstAsLRef: TLRefInstrNode;
      function LastAsIns: TInstrNode;
      function LastAsLab: TLabelNode;
      function LastAsLRef: TLRefInstrNode;

      procedure PrependLabel;
      procedure AppendLabel;
      procedure AppendLabel( ALabNode: TLabelNode );
      function GenLabel: TLabelNode;

      {generic, stab entry is operand}
      procedure InsStabLoad( l,c: Integer; oc: TInsOpcodeCode; const sentry: String );
      {generic, no operand (or known from opcode)}
      procedure InsNoOperand( l,c: Integer; oc: TInsOpcodeCode );
      {generic, operand is Integer}
      procedure InsOperand( l,c: Integer; oc: TInsOpcodeCode; op: VMInt );

      {generic, addr (label) is operand}
      procedure InsLRefOp( l,c: Integer; oc: TInsOpcodeCode; LabN: TLabelNode );

      {generic, call using n operands}
      procedure InsCall( l,c: Integer; oc: TInsOpcodeCode; opcount: VMInt );

      function LinkAndWrite( var codeblock: TByteCodeBlock ): Boolean;
  end;

procedure InitAssembler;
procedure DoneAssembler;

implementation

{label allocation}

var
  NextLabel: VMInt;

function GetLabel: VMInt;
begin
  Result := NextLabel;
  Inc(NextLabel,1);
end;

{ stab }

type
  PStabEntry = ^TStabEntry;
  TStabEntry = record
    idx: VMInt;
    s: String;
  end;

var
  StabEntryNr: VMInt;
  STabLookupTrie: THashTrie;

function STabEntryId( const s: String ): VMInt;
var pstabe: PStabEntry;
begin
  pstabe := STabLookupTrie.Lookup(s);
  if not Assigned(pstabe) then
    begin
      pstabe := New(PStabEntry);
      pstabe^.idx := StabEntryNr;
      Result := StabEntryNr;
      pstabe^.s := s;
      Inc(StabEntryNr,1);
      STabLookupTrie.Add(s,pstabe);
    end
  else
    Result := pstabe^.idx;
end;

{init&done}

function StabEntryRelease( const s: String; data, xdata: Pointer ): Boolean;
begin
  Dispose(PStabEntry(data));
  Result := true;
end;

procedure InitAssembler;
begin
  STabLookupTrie.Init(100);
  StabEntryNr := 0;
  NextLabel := 0;
end;

procedure DoneAssembler;
begin
  STabLookupTrie.ForEach(@StabEntryRelease,nil);
  STabLookupTrie.Clear;
  STabLookupTrie.Pack;
end;

{ TAssembly }

procedure TAssembly.AppendNewIns;
var n: TAssemblyNode;
begin
  n := TInstrNode.Create;
  n.Insert(nil,FLast);
  FLast := n;
  if not Assigned(FFirst) then
    FFirst := FLast;
end;

procedure TAssembly.AppendLabel;
var n: TAssemblyNode;
begin
  n := TLabelNode.Create;
  n.Insert(nil,FLast);
  FLast := n;
  if not Assigned(FFirst) then
    FFirst := FLast;
end;

procedure TAssembly.AppendLabel(ALabNode: TLabelNode);
begin
  ALabNode.Insert(nil,FLast);
  FLast := ALabNode;
  if not Assigned(FFirst) then
    FFirst := FLast;
end;

function TAssembly.GenLabel: TLabelNode;
begin
  Result := TLabelNode.Create;
end;

procedure TAssembly.AppendNewLRefIns;
var n: TAssemblyNode;
begin
  n := TLRefInstrNode.Create;
  n.Insert(nil,FLast);
  FLast := n;
  if not Assigned(FFirst) then
    FFirst := FLast;
end;

constructor TAssembly.Create;
begin
  FFirst := nil;
  FLast := nil;
end;

destructor TAssembly.Destroy;
var node, nodetmp: TAssemblyNode;
begin
  node := FFirst;
  while Assigned(node) do
    begin
      nodetmp := node.FNext;
      node.Free;
      node := nodetmp;
    end;
end;

procedure TAssembly.AppendAssembly(AAssembly: TAssembly);
begin
  if Assigned(FLast) then
    begin
      FLast.FNext := AAssembly.FFirst;
      if Assigned(AAssembly.FLast) then
        FLast := AAssembly.FLast;
    end
  else
    begin
      FFirst := AAssembly.FFirst;
      FLast := AAssembly.FLast;
    end;
  AAssembly.FFirst := nil;
  AAssembly.FLast := nil;
end;

function TAssembly.FirstAsIns: TInstrNode;
begin
  if Assigned(FFirst) and
     (FFirst.ClassType = TInstrNode) then
    Result := TInstrNode(FFirst)
  else
    Result := nil;
end;

function TAssembly.FirstAsLab: TLabelNode;
begin
  if Assigned(FFirst) and
     (FFirst.ClassType = TLabelNode) then
    Result := TLabelNode(FFirst)
  else
    Result := nil;
end;

function TAssembly.FirstAsLRef: TLRefInstrNode;
begin
  if Assigned(FFirst) and
     (FFirst.ClassType = TLRefInstrNode) then
    Result := TLRefInstrNode(FFirst)
  else
    Result := nil;
end;

function TAssembly.LastAsIns: TInstrNode;
begin
  if Assigned(FLast) and
     (FLast.ClassType = TInstrNode) then
    Result := TInstrNode(FLast)
  else
    Result := nil;
end;

function TAssembly.LastAsLab: TLabelNode;
begin
  if Assigned(FLast) and
     (FLast.ClassType = TLabelNode) then
    Result := TLabelNode(FLast)
  else
    Result := nil;
end;

function TAssembly.LastAsLRef: TLRefInstrNode;
begin
  if Assigned(FLast) and
     (FLast.ClassType = TLRefInstrNode) then
    Result := TLRefInstrNode(FLast);
end;

procedure TAssembly.PrependLabel;
var n: TAssemblyNode;
begin
  n := TLabelNode.Create;
  n.Insert(FFirst,nil);
  FFirst := n;
  if not Assigned(FLast) then
    FLast := FFirst;
end;

procedure TAssembly.InsStabLoad(l,c: Integer; oc: TInsOpcodeCode; const sentry: String);
var n: TInstrNode;
begin
  AppendNewIns;
  n := LastAsIns;
  n.Ins.SetOpcode(oc);
  n.Ins.SetOperand(STabEntryId(sentry));
  n.Ins.SetLineInfo(l,c);
  ASSERT(n.Ins.IsValid);
end;

procedure TAssembly.InsNoOperand(l,c: Integer; oc: TInsOpcodeCode);
var n: TInstrNode;
begin
  AppendNewIns;
  n := LastAsIns;
  n.Ins.SetOpcode(oc);
  n.Ins.ClearOperand;
  n.Ins.SetLineInfo(l,c);
  ASSERT(n.Ins.IsValid);
end;

procedure TAssembly.InsOperand(l, c: Integer; oc: TInsOpcodeCode;
  op: VMInt);
var n: TInstrNode;
begin
  AppendNewIns;
  n := LastAsIns;
  n.Ins.SetOpcode(oc);
  n.Ins.SetOperand(op);
  n.Ins.SetLineInfo(l,c);
  ASSERT(n.Ins.IsValid);
end;

procedure TAssembly.InsLRefOp(l,c: Integer; oc: TInsOpcodeCode; LabN: TLabelNode);
var n: TInstrNode;
begin
  AppendNewLRefIns;
  n := LastAsLRef;
  n.Ins.SetOpcode(oc);
  n.Ins.SetOperand(LabN.LabIdent);
  n.Ins.SetLineInfo(l,c);
  //ASSERT(n.Ins.IsValid);
end;

procedure TAssembly.InsCall(l,c: Integer; oc: TInsOpcodeCode; opcount: VMInt);
var n: TInstrNode;
begin
  AppendNewIns;
  n := LastAsIns;
  n.Ins.SetOpcode(oc);
  n.Ins.SetOperand(opcount);
  n.Ins.SetLineInfo(l,c);
  ASSERT(n.Ins.IsValid);
end;

function StabWriter( const s: String; data, xdata: Pointer ): Boolean;
begin
  PByteCodeBlock(xdata)^.stab[PStabEntry(data)^.idx] := s;
  Inc(PByteCodeBlock(xdata)^.bcsize,Length(s));
  Result := true;
end;

function TAssembly.LinkAndWrite(var codeblock: TByteCodeBlock): Boolean;
var nrinstr: VMInt;
    labtable: packed array of VMInt;
    cnode: TAssemblyNode;
begin
  Result := true;
  if (Length(codeblock.stab) > 0) or
     (Length(codeblock.image) > 0) then
    put_internalerror(2011120510);

  {correct code, if last node is label, we need a nop as target}
  if Assigned(LastAsLab) then
    InsNoOperand(0,0,isc_m_nop_ign);

  {check stab lengths}
  if not check_bc_maxstab(StabEntryNr) then
    begin
      put_warning('Stab Limit Exceeded.');
      Exit(false);
    end;

  {write the stab}
  codeblock.bcsize := SizeOf(LongInt)*StabEntryNr;
  SetLength(codeblock.stab,StabEntryNr);
  STabLookupTrie.ForEach(@StabWriter,@codeblock);

  {release a bit of mem}
  STabLookupTrie.ForEach(@StabEntryRelease,nil);
  STabLookupTrie.Clear;
  STabLookupTrie.Pack;

  {linking pass 1, collect label positions, count instructions}
  if NextLabel > 0 then
    begin
      SetLength(labtable,NextLabel);
      FillByte(labtable[0],NextLabel*SizeOf(VMInt),$FF);
    end
  else
    SetLength(labtable,0);

  nrinstr := 0;
  cnode := FFirst;
  while Assigned(cnode) do
    begin
      if cnode.ClassType <> TLabelNode then
        Inc(nrinstr,1)
      else
        begin
          if labtable[TLabelNode(cnode).LabIdent] >= 0 then
            put_internalerror(2011121020); // duplicate label
          labtable[TLabelNode(cnode).LabIdent] := nrinstr+1;
        end;
      cnode := cnode.FNext;
    end;

  {check code length}
  if not check_bc_maxcode(nrinstr) then
    begin
      put_warning('Code Limit Exceeded.');
      Exit(false);
    end;

  Inc(codeblock.bcsize,SizeOf(TInstructionPackage)*nrinstr);

  {check bcsize}
  if not check_bc_maxsize(codeblock.bcsize) then
    begin
      put_warning('Bytecode Limit Exceeded.');
      Exit(false);
    end;

  {linking/assembly pass 2, rewrite lrefs, copy code}
  if nrinstr > 0 then
    begin
      SetLength(codeblock.image,nrinstr);
      cnode := FFirst;
      nrinstr := 0;
      while Assigned(cnode) do
        begin
          if cnode.ClassType <> TLabelNode then
            begin
              {callc relative addr}
              if cnode.ClassType = TLRefInstrNode then
                begin
                  TLRefInstrNode(cnode).Ins.SetOperand(
                    labtable[TLRefInstrNode(cnode).Ins.GetOperand] - (nrinstr+1));
                  ASSERT(TLRefInstrNode(cnode).Ins.IsValid);
                end;
              {copy instruction pkg}
              Move(TInstrNode(cnode).Ins,
                   codeblock.image[nrinstr],
                   SizeOf(TInstructionPackage));
              Inc(nrinstr,1);
            end;
          cnode := cnode.FNext;
        end;
    end;

  SetLength(labtable,0);
end;

{ TLabelNode }

constructor TLabelNode.Create;
begin
  inherited Create;
  FLabIdent := GetLabel;
end;

{ TInstrNode }

constructor TInstrNode.Create;
begin
  inherited Create;
  Ins.Invalidate;
end;

{ TAssemblyNode }

procedure TAssemblyNode.Insert(ANext, APrev: TAssemblyNode);
begin
  FNext := ANext;
  FPrev := APrev;
  if Assigned(ANext) then
    ANext.FPrev := Self;
  if Assigned(APrev) then
    APrev.FNext := Self;
end;

constructor TAssemblyNode.Create;
begin
  FNext := nil;
  FPrev := nil;
end;

end.

