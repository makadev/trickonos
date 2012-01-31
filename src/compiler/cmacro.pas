{   Unit for core built-in macros 

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

unit cmacro;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, eomsg, ucfs, appstart, ccbase, assembl, ccexprp, ccstatn, cscan,
  csyms, opcode;

type
  {2 arguments, both string, compiles to nothing (compiletime setter)}
  TCoreMacro_Marker = class(TPLDefaultMCall)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {2 arguments, both string, compiles to nothing (compiletime setter)}
  TCoreMacro_ECCMarker = class(TPLDefaultMCall)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {1 arguments, string, compiles to nothing (compiletime setter)}
  TCoreMacro_LNMarker = class(TPLDefaultMCall)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {generic, 2 arguments, dont use directly}
  TCoreMacro_Echo_Ext = class(TPLDefaultMCall)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {2 arguments}
  TCoreMacro_Echo_Fmt = class(TCoreMacro_Echo_Ext)
    public
      function Compile: Boolean; override;
  end;

  {2 arguments}
  TCoreMacro_Echo_Subst = class(TCoreMacro_Echo_Ext)
    public
      function Compile: Boolean; override;
  end;

  {1 or 0 argument}
  TCoreMacro_Echo_Ln = class(TPLDefaultMCall)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {1 argument}
  TCoreMacro_Use = class(TPLDefaultMCall)
    public
      function Compile: Boolean; override;
  end;

  {1 argument}
  TCoreMacro_Include = class(TPLDefaultMCall)
    public
      function Compile: Boolean; override;
  end;

  {1 argument}
  TCoreMacro_Open = class(TPLDefaultMCall)
    public
      function Compile: Boolean; override;
  end;

  {1 argument}
  TCoreMacro_Reopen = class(TPLDefaultMCall)
    public
      function Compile: Boolean; override;
  end;

  {1 argument}
  TCoreMacro_Path = class(TPLDefaultMCall)
    public
      function Compile: Boolean; override;
  end;

  {0 argument}
  TCoreMacro_Close = class(TPLDefaultMCall)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {0 or 1 arguments}
  TCoreMacro_Halt = class(TPLDefaultMCall)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {1 argument}
  TCoreMacro_Assert = class(TPLDefaultMCall)
    public
      function Compile: Boolean; override;
  end;

implementation

{ TCoreMacro_LNMarker }

class function TCoreMacro_LNMarker.Parse: TParserNode;
var lmarker: PUCFS32String;
begin
  IncRec;
  ExpectSym(SMES_ID);
  Result := Self.Create;
  Result.SetLineInfoFrom(CurrentToken);
  NextToken;
  ExpectSym(SMES_String);
  lmarker := CurrentToken.Pattern;
  NextToken;
  ExpectSym(SMES_SColon);
  cscan_setlcmarker(lmarker);
  NextToken;
  DecRec;
end;

function TCoreMacro_LNMarker.Compile: Boolean;
begin
  CreateAssembly;
  Result := true;
end;

{ TCoreMacro_ECCMarker }

class function TCoreMacro_ECCMarker.Parse: TParserNode;
var imarker, omarker: PUCFS32String;
begin
  IncRec;
  ExpectSym(SMES_ID);
  Result := Self.Create;
  Result.SetLineInfoFrom(CurrentToken);
  NextToken;
  ExpectSym(SMES_String);
  imarker := CurrentToken.Pattern;
  NextToken;
  ExpectSym(SMES_Comma);
  NextToken;
  ExpectSym(SMES_String);
  omarker := CurrentToken.Pattern;
  NextToken;
  ExpectSym(SMES_SColon);
  cscan_seteccmarker(imarker,omarker);
  NextToken;
  DecRec;
end;

function TCoreMacro_ECCMarker.Compile: Boolean;
begin
  CreateAssembly;
  Result := true;
end;

{ TCoreMacro_Assert }

function TCoreMacro_Assert.Compile: Boolean;
var exlab,hallab: TLabelNode;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Occ[1] is TParserNode);
  Result := TParserNode(Occ[1]).Compile;
  {push expr}
  Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
  {now the tricky coss jump... want assert to behave like if/elseif and
   loop conditions, but they use isc_m_jmppop_false_nil_addr.
   so we jmppop false nil @ halt, otherwise pop and jmp @exit}
  exlab := Assembly.GenLabel;
  hallab := Assembly.GenLabel;
  {jmp false nil @ halt}
  Assembly.InsLRefOp(Line,Column,isc_m_jmp_false_nil_addr,hallab);
  {otherwise, pop 1, jmp @ exit}
  Assembly.InsOperand(Line,Column,isc_m_pop_nr,1);
  Assembly.InsLRefOp(Line,Column,isc_m_jmp_addr,exlab);
  {now halt on false/nil, which is still on stack}
  Assembly.AppendLabel(hallab);
  Assembly.InsNoOperand(Line,Column,isc_m_halt_ign);
  {and finally, ne normal flow}
  Assembly.AppendLabel(exlab);
  DecRec;
end;

{ TCoreMacro_Marker }

class function TCoreMacro_Marker.Parse: TParserNode;
var imarker, omarker: PUCFS32String;
begin
  IncRec;
  ExpectSym(SMES_ID);
  Result := Self.Create;
  Result.SetLineInfoFrom(CurrentToken);
  NextToken;
  ExpectSym(SMES_String);
  imarker := CurrentToken.Pattern;
  NextToken;
  ExpectSym(SMES_Comma);
  NextToken;
  ExpectSym(SMES_String);
  omarker := CurrentToken.Pattern;
  NextToken;
  ExpectSym(SMES_SColon);
  cscan_setcodemarker(imarker,omarker);
  NextToken;
  DecRec;
end;

function TCoreMacro_Marker.Compile: Boolean;
begin
  CreateAssembly;
  Result := true;
end;

{ TCoreMacro_Echo_Subst }

function TCoreMacro_Echo_Subst.Compile: Boolean;
begin
  Result := inherited Compile;
  Assembly.InsOperand(Line,Column,isc_m_echo,Ord(mecho_echosubst));
end;

{ TCoreMacro_Echo_Fmt }

function TCoreMacro_Echo_Fmt.Compile: Boolean;
begin
  Result := inherited Compile;
  Assembly.InsOperand(Line,Column,isc_m_echo,Ord(mecho_echofmt));
end;

{ TCoreMacro_Echo_Ext }

class function TCoreMacro_Echo_Ext.Parse: TParserNode;
begin
  IncRec;
  ExpectSym(SMES_ID);
  Result := self.Create;
  Result.SetLineInfoFrom(CurrentToken);
  Result.AddOcc(CurrentToken);
  NextToken;
  Result.AddOcc(ParseExpr([SMES_Comma]));
  NextToken;
  Result.AddOcc(ParseExpr([SMES_SColon]));
  NextToken;
  DecRec;
end;

function TCoreMacro_Echo_Ext.Compile: Boolean;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Count=3);
  ASSERT(Occ[0].ClassType = TScanRecord);
  ASSERT(Occ[1] is TParserNode);
  ASSERT(Occ[2] is TParserNode);
  Result := TParserNode(Occ[1]).Compile;
  Result := TParserNode(Occ[2]).Compile and Result;
  Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
  Assembly.AppendAssembly(TParserNode(Occ[2]).Assembly);
  {-- need instruction here or stack will be uneven!}
  DecRec;
end;

{ TCoreMacro_Echo_Ln }

class function TCoreMacro_Echo_Ln.Parse: TParserNode;
begin
  IncRec;
  ExpectSym(SMES_ID);
  Result := self.Create;
  Result.SetLineInfoFrom(CurrentToken);
  Result.AddOcc(CurrentToken);
  NextToken;
  if (ucfs_length(TScanRecord(Result.Occ[0]).Pattern) > 4) and
     (CurrentToken.TokenType = SMES_SColon) then
    NextToken  // allow no expression for LineEnding only
  else
    begin
      Result.AddOcc(ParseExpr([SMES_SColon]));
      NextToken;
    end;
  DecRec;
end;

function TCoreMacro_Echo_Ln.Compile: Boolean;
begin
  CreateAssembly;
  IncRec;
  Assert(Count >= 1);
  ASSERT(Occ[0].ClassType = TScanRecord);
  ASSERT(((Count=2) and (Occ[1] is TParserNode)) or (ucfs_length(TScanRecord(Occ[0]).Pattern) > 5));
  Result := true;
  if ucfs_length(TScanRecord(Occ[0]).Pattern) > 5 then
    begin
      if Count = 2 then
        begin
          Result := TParserNode(Occ[1]).Compile;
          Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
          Assembly.InsOperand(Line,Column,isc_m_echo,Ord(mecho_echoln));
        end
      else
        Assembly.InsOperand(Line,Column,isc_m_echo,Ord(mecho_echonl));
    end
  else
    begin
      Result := TParserNode(Occ[1]).Compile;
      Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
      Assembly.InsOperand(Line,Column,isc_m_echo,Ord(mecho_echo));
    end;
  DecRec;
end;

{ TCoreMacro_Use }

function TCoreMacro_Use.Compile: Boolean;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Occ[1] is TParserNode);
  Result := TParserNode(Occ[1]).Compile;
  Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
  {now include}
  Assembly.InsOperand(Line,Column,isc_m_include,Ord(mincl_use));
  DecRec;
end;

{ TCoreMacro_Include }

function TCoreMacro_Include.Compile: Boolean;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Occ[1] is TParserNode);
  Result := TParserNode(Occ[1]).Compile;
  Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
  {now include}
  Assembly.InsOperand(Line,Column,isc_m_include,Ord(mincl_include));
  DecRec;
end;

{ TCoreMacro_Halt }

class function TCoreMacro_Halt.Parse: TParserNode;
begin
  IncRec;
  ExpectSym(SMES_ID);
  Result := self.Create;
  Result.SetLineInfoFrom(CurrentToken);
  Result.AddOcc(CurrentToken);
  NextToken;
  if CurrentToken.TokenType in EXPR_FIRST then
    Result.AddOcc(ParseExpr([SMES_SColon]))
  else
    ExpectSym(SMES_SColon);
  NextToken;
  DecRec;
end;

function TCoreMacro_Halt.Compile: Boolean;
begin
  CreateAssembly;
  IncRec;
  Result := true;
  if Count <= 2 then
    begin
      if Count = 1 then
        begin
          Assembly.InsOperand(Line,Column,isc_m_load_type,Ord(soload_true));
          Assembly.InsNoOperand(Line,Column,isc_m_halt_ign);
        end
      else
        begin
          ASSERT(Occ[1] is TParserNode);
          Result := TParserNode(Occ[1]).Compile;
          Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
          Assembly.InsNoOperand(Line,Column,isc_m_halt_ign);
        end;
    end
  else
    begin
      put_error_for(Line,Column,ucfs_to_utf8string(cscan_streamid),
        'Unexpected Number of Arguments for HALT. Expected 0..1, Got '+IntToStr(Count-1));
      Result := false;
    end;
  DecRec;
end;

{ TCoreMacro_Open }

function TCoreMacro_Open.Compile: Boolean;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Occ[1] is TParserNode);
  Result := TParserNode(Occ[1]).Compile;
  Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
  Assembly.InsOperand(Line,Column,isc_m_outputop,Ord(mout_open));
  DecRec;
end;

{ TCoreMacro_Reopen }

function TCoreMacro_Reopen.Compile: Boolean;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Occ[1] is TParserNode);
  Result := TParserNode(Occ[1]).Compile;
  Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
  Assembly.InsOperand(Line,Column,isc_m_outputop,Ord(mout_reopen));
  DecRec;
end;

{ TCoreMacro_Path }

function TCoreMacro_Path.Compile: Boolean;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Occ[1] is TParserNode);
  Result := TParserNode(Occ[1]).Compile;
  Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
  Assembly.InsOperand(Line,Column,isc_m_outputop,Ord(mout_path));
  DecRec;
end;

{ TCoreMacro_Close }

class function TCoreMacro_Close.Parse: TParserNode;
begin
  IncRec;
  ExpectSym(SMES_ID);
  Result := self.Create;
  Result.SetLineInfoFrom(CurrentToken);
  Result.AddOcc(CurrentToken);
  NextToken;
  ExpectSym(SMES_SColon);
  NextToken;
  DecRec;
end;

function TCoreMacro_Close.Compile: Boolean;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Count <= 1);
  Assembly.InsOperand(Line,Column,isc_m_outputop,Ord(mout_close));
  Result := true;
  DecRec;
end;

(******************************************************************************
 * Macro Register Hook and Init
 ******************************************************************************)

procedure InitAndRegister;
begin
  ccstatn.RegisterMacroCallNode('OPEN',TCoreMacro_Open);
  ccstatn.RegisterMacroCallNode('REOPEN',TCoreMacro_Reopen);
  ccstatn.RegisterMacroCallNode('OPENDIR',TCoreMacro_Path);
  ccstatn.RegisterMacroCallNode('CLOSE',TCoreMacro_Close);
  ccstatn.RegisterMacroCallNode('ASSERT',TCoreMacro_Assert);
  ccstatn.RegisterMacroCallNode('WRITESUBST',TCoreMacro_Echo_Subst);
  ccstatn.RegisterMacroCallNode('WRITEFMT',TCoreMacro_Echo_Fmt);
  ccstatn.RegisterMacroCallNode('WRITELN',TCoreMacro_Echo_Ln);
  ccstatn.RegisterMacroCallNode('WRITE',TCoreMacro_Echo_Ln);
  ccstatn.RegisterMacroCallNode('HALT',TCoreMacro_Halt);
  ccstatn.RegisterMacroCallNode('USE',TCoreMacro_Use);
  ccstatn.RegisterMacroCallNode('INCLUDE',TCoreMacro_Include);
  ccstatn.RegisterMacroCallNode('MARKER',TCoreMacro_Marker);
  ccstatn.RegisterMacroCallNode('LNMARKER',TCoreMacro_LNMarker);
  ccstatn.RegisterMacroCallNode('ECCMARKER',TCoreMacro_ECCMarker);
end;

initialization
  appstart.RegisterInit(@InitAndRegister);

end.

