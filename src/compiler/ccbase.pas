{   Unit for basic scanner/parser nodes

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

unit ccbase;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, commontl, gens, eomsg, coreobj, csyms, assembl;

type
  {traced Node, only valid in compilation stage, no freeing needed, garbage
   is collected after compilation, thought its a good thing to keep places
   clean whenever it is known that nodes are not used again}
  TCCTracedNode = class
    private
      FSlot: MachineInt;
    public
      constructor Create; virtual;
      destructor Destroy; override;
  end;

  {Generic Temporal Node, Used exclusive in Compilation Stage}
  TCCNode = class(TCCTracedNode)
    protected
      FLine: StreamInt;
      FCol: StreamInt;
    public
      property Line: StreamInt read FLine write FLine;
      property Column: StreamInt read FCol write FCol;
      procedure SetLineInfoFrom( ccnode: TCCNode );
  end;

  {Scanner Node Base}
  TScanRecord = class(TCCNode)
    private
      FTokenType: TSMESymbol;
      FPattern: String;
    public
      property TokenType: TSMESymbol read FTokenType write FTokenType;
      property Pattern: String read FPattern write FPattern;
      constructor Create; override;
  end;

  {Parser/AST/Compiler Node Base}
  TParserNode = class(TCCNode)
    private
      FAssembly: TAssembly;
      FOccs: array of TCCNode;
      function getCount: Integer;
      function getOcc( i: MachineInt ): TCCNode;
    protected
      procedure CreateAssembly;
    public
      property Assembly: TAssembly read FAssembly;
      property Count: Integer read getCount;
      property Occ[ i: MachineInt ]: TCCNode read getOcc; default;

      class function Parse: TParserNode; virtual; abstract;

      procedure ReleaseAssembly;

      function AddOcc( ANode: TCCNode ): MachineInt;
      procedure ClearOccs( ARelease: Boolean = False );

      function Compile: Boolean; virtual;
      constructor Create; override;
      destructor Destroy; override;
  end;


type
  TCCFrameType = (
    ccft_function,
    ccft_loop
  );

  PCCFrameInfo = ^TCCFrameInfo;
  TCCFrameInfo = record
    exit_lab: TLabelNode;    // exit / break
    case ftype: TCCFrameType of
      ccft_function: (
        exitdecl_lab: TLabelNode;   // jump over actual code to decl
        callintro_lab: TLabelNode;  // call label for addr -> decl
        local_index: PHashTrie;     // local var -> bp+index mapping
      );
      ccft_loop: (
        reentry_lab: TLabelNode;    // continue
        stackuse: VMInt;
      );
  end;

procedure FrameStackPush(ftype: TCCFrameType);
procedure FrameStackPop;
function FrameStackEmpty: Boolean; inline;
function FrameStackNoFun: Boolean; inline;

{dont save the return value, since its an addr taken from dynamic array
 which may be incorrect after another push}
function FrameStackTop: PCCFrameInfo; inline;
{dont save the return value, since its an addr taken from dynamic array
 which may be incorrect after another push}
function FrameStackTopFun: PCCFrameInfo; inline;


procedure IncRec;
procedure DecRec;

procedure InitCC;
procedure ReleaseCC;

implementation

uses cscan;

type
  TNodeList = specialize TSimpleList<TCCTracedNode>;

var NodeList: TNodeList;
    FrameStackTopFunc: array of MachineInt;
    FrameStack: array of TCCFrameInfo;
    CCRecursions: MachineInt;

procedure FrameStackPush(ftype: TCCFrameType);
begin
  SetLength(FrameStack,Length(FrameStack)+1);
  FrameStack[High(FrameStack)].ftype := ftype;
  case FrameStackTop^.ftype of
    ccft_function:
      begin
        FrameStackTop^.local_index := New(PHashTrie);
        FrameStackTop^.local_index^.Init(5);
        SetLength(FrameStackTopFunc,Length(FrameStackTopFunc)+1);
        FrameStackTopFunc[High(FrameStackTopFunc)] := High(FrameStack);
      end;
    ccft_loop:
      FrameStackTop^.stackuse := 0;
    else
      put_internalerror(2011121002);
  end;
end;

procedure FrameStackPop;
begin
  case FrameStackTop^.ftype of
    ccft_function:
      begin
        FrameStackTop^.local_index^.Done;
        Dispose(FrameStackTop^.local_index);
        SetLength(FrameStackTopFunc,Length(FrameStackTopFunc)-1);
      end;
  end;
  SetLength(FrameStack,Length(FrameStack)-1);
end;

function FrameStackTop: PCCFrameInfo;
begin
  Result := @FrameStack[High(FrameStack)];
end;

function FrameStackEmpty: Boolean;
begin
  Result := Length(FrameStack) <= 0;
end;

function FrameStackNoFun: Boolean;
begin
  Result := Length(FrameStackTopFunc) <= 0;
end;

function FrameStackTopFun: PCCFrameInfo;
begin
  Result := @FrameStack[FrameStackTopFunc[High(FrameStackTopFunc)]];
end;

procedure IncRec;
begin
  Inc(CCRecursions,1);
  check_cc_maxrecurse(CCRecursions);
end;

procedure DecRec;
begin
  Dec(CCRecursions,1);
end;

procedure InitCC;
begin
  NodeList := TNodeList.Create;
  CCRecursions := 0;
  SetLength(FrameStack,0);
  SetLength(FrameStackTopFunc,0);
end;

procedure ReleaseCC;
begin
  if not Assigned(NodeList) then
    Exit;
  if Length(FrameStack) <> 0 then
    put_internalerror(2011121001);
  //SetLength(FrameStackTopFunc,0); <- implied by above line(s)
  while NodeList.Count > 0 do
    NodeList.Last.Free;
  NodeList.Free;
  NodeList := nil;
end;

{ TCCNode }

procedure TCCNode.SetLineInfoFrom(ccnode: TCCNode);
begin
  FLine := ccnode.FLine;
  FCol := ccnode.FCol;
end;

{ TCCTracedNode }

constructor TCCTracedNode.Create;
begin
  FSlot := NodeList.Push(self);
end;

destructor TCCTracedNode.Destroy;
begin
  if FSlot <> (NodeList.Count-1) then
    begin
      NodeList.Items[FSlot] := NodeList.Last;
      NodeList.Items[FSlot].FSlot := FSlot;
    end;
  NodeList.Pop;
  inherited Destroy;
end;

{ TParserNode }

function TParserNode.getCount: Integer;
begin
  Result := Length(FOccs);
end;

function TParserNode.getOcc( i: MachineInt ): TCCNode;
begin
  Result := FOccs[i];
end;

procedure TParserNode.CreateAssembly;
begin
  if not Assigned(FAssembly) then
    FAssembly := TAssembly.Create
  else
    put_internalerror(2011120401);
end;

procedure TParserNode.ReleaseAssembly;
begin
  if Assigned(FAssembly) then
    begin
      FAssembly.Free;
      FAssembly := nil;
    end;
end;

function TParserNode.AddOcc(ANode: TCCNode): MachineInt;
begin
  SetLength(FOccs,Length(FOccs)+1);
  FOccs[High(FOccs)] := ANode;
  Result := High(FOccs);
end;

procedure TParserNode.ClearOccs(ARelease: Boolean);
var i: MachineInt;
begin
  if ARelease then
    begin
      for i := 0 to Length(FOccs) do
        if Assigned(FOccs[i]) then
          FOccs[i].Free;
    end;
  SetLength(FOccs,0);
end;

function TParserNode.Compile: Boolean;
begin
  put_internalerror(2011120201);
  Result := false;
end;

constructor TParserNode.Create;
begin
  inherited Create;
  FAssembly := nil;
  SetLength(FOccs,0);
end;

destructor TParserNode.Destroy;
begin
  ReleaseAssembly;
  ClearOccs;
  inherited Destroy;
end;

{ TScanRecord }

constructor TScanRecord.Create;
begin
  inherited Create;
  FTokenType := SMES_ERROR;
  FPattern := '';
end;


end.

