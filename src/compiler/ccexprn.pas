{   Unit for expression node handling and compilation

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

unit ccexprn;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, commontl, ucfs, eomsg, ccbase, cscan, csyms, assembl, opcode, ffpa;

type
  {int or string}
  TExprConst = class(TParserNode)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {ident}
  TExprId = class(TParserNode)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {(id,expr)*}
  TExprDict = class(TParserNode)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {expr*}
  TExprList = class(TParserNode)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {op,expr}
  TExprUnary = class(TParserNode)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {op,lexpr,rexpr}
  TExprBinary = class(TParserNode)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {callerexpr(,argexpr)*}
  TExprCall = class(TParserNode)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {objectexpr,id}
  TExprSelect = class(TParserNode)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {objectexpr,id}
  TExprIs = class(TParserNode)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {objectexpr,indexexpr}
  TExprIndex = class(TParserNode)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {lexpr,rexpr}
  TExprAssign = class(TParserNode)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

procedure ExprAssembleTOSSetter( Line, Column: Integer; Assembly: TAssembly; name: PUCFS32String );
procedure ExprAssembleTOSGetter( Line, Column: Integer; Assembly: TAssembly; name: PUCFS32String );

implementation

uses ccexprp;

{ TExprIs }

class function TExprIs.Parse: TParserNode;
begin
  Result := nil;
  put_internalerror(2011120980);
end;

function TExprIs.Compile: Boolean;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Count=2);
  ASSERT(Occ[0] is TParserNode);
  ASSERT(Occ[1].ClassType = TScanRecord);
  Result := TParserNode(Occ[0]).Compile;
  Assembly.AppendAssembly(TParserNode(Occ[0]).Assembly);
  Assembly.InsStabLoad(Line,Column,isc_typecheck_stab,TScanRecord(Occ[1]).Pattern);
  DecRec;
end;

{ TExprConst }

class function TExprConst.Parse: TParserNode;
begin
  IncRec;
  ExpectSymSet([SMES_NIL, SMES_TRUE, SMES_FALSE, SMES_String, SMES_Int,
                SMES_IntBin, SMES_IntHex, SMES_IntOct]);
  Result := TExprConst.Create;
  Result.SetLineInfoFrom(CurrentToken);
  Result.AddOcc(CurrentToken);
  NextToken;
  DecRec;
end;

function TExprConst.Compile: Boolean;
var tmps: String;
    tmpi: PTFM_Integer;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Count=1);
  ASSERT(Occ[0].ClassType = TScanRecord);
  Result := true;
  case TScanRecord(Occ[0]).TokenType of
    SMES_NIL: Assembly.InsNoOperand(Line,Column,isc_soload_nil_ign);
    SMES_TRUE: Assembly.InsNoOperand(Line,Column,isc_soload_true_ign);
    SMES_FALSE: Assembly.InsNoOperand(Line,Column,isc_soload_false_ign);
    SMES_String: Assembly.InsStabLoad(Line,Column,isc_soload_string_stab,TScanRecord(Occ[0]).Pattern);

    SMES_Int,SMES_IntBin,SMES_IntHex,SMES_IntOct:
      begin
        tmps := ucfs_to_utf8string(TScanRecord(Occ[0]).Pattern);
        tmpi := nil;
        {convert number}
        case TScanRecord(Occ[0]).TokenType of
          SMES_Int: tmpi := tfm_from_string(tmps,32);
          SMES_IntBin: tmpi := tfm_from_bin(tmps,32);
          SMES_IntHex: tmpi := tfm_from_hex(tmps,32);
          SMES_IntOct: tmpi := tfm_from_oct(tmps,32);
          else
            put_internalerror(12020101);
        end;

        if Assigned(tmpi) then
          begin
            if tmpi^.Properties.NrBits = 32 then
              begin
                {cut stuff above and convert into longint}
                Assembly.InsOperand(Line,Column,isc_soload_int_operi,VMInt(tmpi^.logic_expanded_word(0)));
                tfm_release(tmpi);
              end
            else
              begin
                tmps := tfm_to_hex(tmpi,true);
                tfm_release(tmpi);
                TScanRecord(Occ[0]).PatternSetS(tmps);
                TScanRecord(Occ[0]).TokenType := SMES_IntHex;
                Assembly.InsStabLoad(Line,Column,isc_soload_int_stab,TScanRecord(Occ[0]).Pattern);
              end;
          end
        else
          begin
            put_error_for(Line,Column,ucfs_to_utf8string(cscan_streamid),
                          'Invalid Q Number Format.');
            Result := false;
          end;
      end;
    else
      put_internalerror(2011120301);
  end;
  DecRec;
end;

{ TExprList }

class function TExprList.Parse: TParserNode;
begin
  IncRec;
  ExpectSym(SMES_OSquare);
  Result := TExprList.Create;
  Result.SetLineInfoFrom(CurrentToken);
  NextToken;
  if CurrentToken.TokenType <> SMES_CSquare then
    begin
      ExpectSymSet(EXPR_FIRST);
      repeat
        Result.AddOcc(TExpr.Parse);
        if CurrentToken.TokenType = SMES_Comma then
          begin
            NextToken;
            ExpectSymSet(EXPR_FIRST);
          end;
      until CurrentToken.TokenType = SMES_CSquare;
    end;
  NextToken;
  DecRec;
end;

function TExprList.Compile: Boolean;
var i: MachineInt;
begin
  CreateAssembly;
  IncRec;
  Assembly.InsNoOperand(Line,Column,isc_soload_list_ign);
  Result := true;
  if Count > 0 then
    begin
      for i := 0 to Count-1 do
        begin
          ASSERT(Occ[i] is TParserNode);
          Result := TParserNode(Occ[i]).Compile and Result;
          Assembly.AppendAssembly(TParserNode(Occ[i]).Assembly);
        end;
    end;
  if Count >= 1 then
    Assembly.InsOperand(Line,Column,isc_listappend_opern,Count);
  DecRec;
end;

{ TExprDict }

class function TExprDict.Parse: TParserNode;
begin
  IncRec;
  ExpectSym(SMES_OCurly);
  Result := TExprDict.Create;
  Result.SetLineInfoFrom(CurrentToken);
  NextToken;
  if CurrentToken.TokenType <> SMES_CCurly then
    begin
      ExpectSymSet([SMES_ID,SMES_STRING]);
      repeat
        Result.AddOcc(CurrentToken);
        NextToken;
        ExpectSym(SMES_Colon);
        NextToken;
        Result.AddOcc(TExpr.Parse);
        if CurrentToken.TokenType = SMES_Comma then
          begin
            NextToken;
            ExpectSymSet([SMES_ID,SMES_STRING]);
          end;
      until CurrentToken.TokenType = SMES_CCurly;
    end;
  NextToken;
  DecRec;
end;

function TExprDict.Compile: Boolean;
var i: MachineInt;
begin
  CreateAssembly;
  IncRec;
  Assembly.InsNoOperand(Line,Column,isc_soload_dict_ign);
  Result := true;
  if Count > 0 then
    begin
      i := 0;
      while i < Count do
        begin
          ASSERT(Occ[i].ClassType = TScanRecord);
          ASSERT(Occ[i+1] is TParserNode);
          Result := TParserNode(Occ[i+1]).Compile and Result;
          if Result then
            begin
              {both setmember and setindex return the set value such that
                 x := dict['bla'] := z -> x = z,
                 so we need to safe the dict instance, while building with
                 setmember/setindex}
              Assembly.InsOperand(Line,Column,isc_dup_opern,1);
              // stackdup
              if TScanRecord(Occ[i]).TokenType = SMES_ID then
                begin
                  Assembly.AppendAssembly(TParserNode(Occ[i+1]).Assembly);
                  Assembly.InsStabLoad(Occ[i].Line,Occ[i].Column,isc_setm_stab,
                                       TScanRecord(Occ[i]).Pattern);
                end
              else
                begin
                  {string into x["<string>"] := bla, case sensitive,
                    needs different order, first <x>, then stabentry, then <expr>}
                  Assembly.InsStabLoad(Occ[i].Line,Occ[i].Column,isc_soload_string_stab,
                    TScanRecord(Occ[i]).Pattern);
                  Assembly.AppendAssembly(TParserNode(Occ[i+1]).Assembly);
                  Assembly.InsNoOperand(Occ[i].Line,Occ[i].Column,isc_seti_ign);
                end;
               // pop
               Assembly.InsOperand(Line,Column,isc_pop_opern,1);
            end;
          Inc(i,2);
        end;
    end;
  DecRec;
end;

procedure ExprAssembleTOSGetter( Line, Column: Integer; Assembly: TAssembly; name: PUCFS32String );
begin
  if FrameStackNoFun or
     (not FrameStackTopFun^.local_index^.Exists(name)) then
    {default global load}
    Assembly.InsStabLoad(Line,Column,isc_getenv_stab,name)
  else
    begin
      {local load slot in function frame}
      Assembly.InsOperand(Line,Column,isc_fprel_load_slot,
        PtrInt(FrameStackTopFun^.local_index^.Lookup(name)-nil));
    end;
end;

{ TExprId }

class function TExprId.Parse: TParserNode;
begin
  IncRec;
  ExpectSym(SMES_ID);
  Result := TExprId.Create;
  Result.SetLineInfoFrom(CurrentToken);
  Result.AddOcc(CurrentToken);
  NextToken;
  DecRec;
end;

function TExprId.Compile: Boolean;
begin
  CreateAssembly;
  ASSERT(Count=1);
  ASSERT(Occ[0].ClassType = TScanRecord);
  IncRec;
  ExprAssembleTOSGetter(Line,Column,Assembly,TScanRecord(Occ[0]).Pattern);
  Result := true;
  DecRec;
end;

{ TExprIndex }

class function TExprIndex.Parse: TParserNode;
begin
  Result := nil;
  put_internalerror(2011112944);
end;

function TExprIndex.Compile: Boolean;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Count=2);
  ASSERT(Occ[0] is TParserNode);
  ASSERT(Occ[1] is TParserNode);
  {assemble left side expression -> TOS}
  Result := TParserNode(Occ[0]).Compile;
  Assembly.AppendAssembly(TParserNode(Occ[0]).Assembly);
  {assemble idx access}
  Result := TParserNode(Occ[1]).Compile and Result;
  Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
  Assembly.InsNoOperand(Line,Column,isc_geti_ign);
  DecRec;
end;

{ TExprSelect }

class function TExprSelect.Parse: TParserNode;
begin
  Result := nil;
  put_internalerror(2011112943);
end;

function TExprSelect.Compile: Boolean;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Count=2);
  ASSERT(Occ[0] is TParserNode);
  ASSERT(Occ[1].ClassType = TScanRecord);
  {assemble left side expression -> TOS}
  Result := TParserNode(Occ[0]).Compile;
  Assembly.AppendAssembly(TParserNode(Occ[0]).Assembly);
  {assemble member get}
  Assembly.InsStabLoad(Line,Column,isc_getm_stab,TScanRecord(Occ[1]).Pattern);
  DecRec;
end;

{ TExprCall }

class function TExprCall.Parse: TParserNode;
begin
  Result := nil;
  put_internalerror(2011112942);
end;

function TExprCall.Compile: Boolean;
var i: MachineInt;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Count>0);
  ASSERT(Occ[0] is TParserNode);
  Result := true;
  if Occ[0].ClassType = TExprSelect then
    begin
      {expr.ident(args) -> Method Call (callm_nrops)}
      ASSERT(TParserNode(Occ[0]).Count = 2);
      ASSERT(TParserNode(Occ[0]).Occ[0] is TParserNode);
      ASSERT(TParserNode(Occ[0]).Occ[1].ClassType = TScanRecord);
      {assemble expr -> TOS}
      Result := TParserNode(TParserNode(Occ[0]).Occ[0]).Compile and Result;
      Assembly.AppendAssembly(TParserNode(TParserNode(Occ[0]).Occ[0]).Assembly);
      {put arguments on stack}
      for i := 1 to Count-1 do
        begin
          Result := TParserNode(Occ[i]).Compile and Result;
          Assembly.AppendAssembly(TParserNode(Occ[i]).Assembly);
        end;
      {load ident for call -> TOS}
      Assembly.InsStabLoad(Occ[0].Line,Occ[0].Column,isc_soload_string_stab,
                           TScanRecord(TParserNode(Occ[0]).Occ[1]).Pattern);
      {callm}
      Assembly.InsCall(Line,Column, isc_callm_opern, Count-1 );
    end
  else
    begin
      {expr(args) and (expr <> e.ident) -> "Call on Callable" (Method Call callcall_nrops)}
      {put expr...arg_n on stack}
      for i := 0 to Count-1 do
        begin
          ASSERT(Occ[i] is TParserNode);
          Result := TParserNode(Occ[i]).Compile and Result;
          Assembly.AppendAssembly(TParserNode(Occ[i]).Assembly);
        end;
      {call call}
      Assembly.InsCall(Line,Column, isc_callcall_opern, Count-1 );
    end;
  if not check_cc_maxargs(Count-1) then
    begin
      Result := false;
      put_error_for(Line,Column,ucfs_to_utf8string(cscan_streamid),'Call exceeds max. arg Limit.');
    end;
  DecRec;
end;

{ TExprBinary }

class function TExprBinary.Parse: TParserNode;
begin
  Result := nil;
  put_internalerror(2011112940);
end;

function TExprBinary.Compile: Boolean;
var tmplab: TLabelNode;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Count=3);
  ASSERT(Occ[0].ClassType = TScanRecord);
  ASSERT(Occ[1] is TParserNode);
  ASSERT(Occ[2] is TParserNode);
  Result := TParserNode(Occ[1]).Compile;
  Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
  Result := TParserNode(Occ[2]).Compile and Result;
  if not (TScanRecord(Occ[0]).TokenType in [SMES_AND,SMES_OR]) then
    begin
      Assembly.AppendAssembly(TParserNode(Occ[2]).Assembly);
      case TScanRecord(Occ[0]).TokenType of
        SMES_Plus: Assembly.InsNoOperand(Line,Column,isc_sobinop_add_ign);
        SMES_Minus: Assembly.InsNoOperand(Line,Column,isc_sobinop_sub_ign);
        SMES_Star: Assembly.InsNoOperand(Line,Column,isc_sobinop_mul_ign);
        SMES_XOR: Assembly.InsNoOperand(Line,Column,isc_sobinop_xor_ign);
        SMES_DIV: Assembly.InsNoOperand(Line,Column,isc_sobinop_div_ign);
        SMES_MOD: Assembly.InsNoOperand(Line,Column,isc_sobinop_mod_ign);
        SMES_OAngel: Assembly.InsNoOperand(Line,Column,isc_socmp_lt_ign);
        SMES_CAngel: Assembly.InsNoOperand(Line,Column,isc_socmp_gt_ign);
        SMES_LE: Assembly.InsNoOperand(Line,Column,isc_socmp_le_ign);
        SMES_GE: Assembly.InsNoOperand(Line,Column,isc_socmp_ge_ign);
        SMES_EQ: Assembly.InsNoOperand(Line,Column,isc_socmp_equ_ign);
        SMES_NEQ: Assembly.InsNoOperand(Line,Column,isc_socmp_neq_ign);
        SMES_SHL,SMES_SHLK: Assembly.InsNoOperand(Line,Column,isc_sobinop_shl_ign);
        SMES_SHR,SMES_SHRK: Assembly.InsNoOperand(Line,Column,isc_sobinop_shr_ign);
        SMES_ROL,SMES_ROLK: Assembly.InsNoOperand(Line,Column,isc_sobinop_rol_ign);
        SMES_ROR,SMES_RORK: Assembly.InsNoOperand(Line,Column,isc_sobinop_ror_ign);
        else
          put_internalerror(2011120431);
      end;
    end
  else
    begin
      case TScanRecord(Occ[0]).TokenType of
        SMES_AND:
          begin
            {
              AND
              LEFT     TRUE:BOOL   FALSE:BOOL  NIL:NILTYPE  d:ANY
              RIGHT      a:ANY       a:ANY       a:ANY      b:ANY
              RESULT     a:ANY     FALSE:BOOL  NIL:NILTYPE  d.and(b)

              ->  1. push leftexpr
                  2. isc_m_jmp_false_nil_addr @6
                  3. push rightexpr
                  4. isc_m_tos1_true_rot_pop_jmp_addr @6
                  5. eval leftexpr.and(rightexpr)
                  6.

            }
            tmplab := Assembly.GenLabel;
            {push left}
            Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
            {jmpfalsenil}
            Assembly.InsLRefOp(Line,Column,isc_jmp_false_nil_addr,tmplab);
            {push right}
            Assembly.AppendAssembly(TParserNode(Occ[2]).Assembly);
            {jmp on tos-1 = true, rotate, pop}
            Assembly.InsLRefOp(Line,Column,isc_tos1_true_rot_pop_jmp_addr,tmplab);
            {eval no shortcut and}
            Assembly.InsNoOperand(Line,Column,isc_sobinop_and_ign);
            {append label}
            Assembly.AppendLabel(tmplab);
          end;
        SMES_OR:
          begin
            {
              OR
              LEFT     TRUE:BOOL   FALSE:BOOL  NIL:NILTYPE  d:ANY
              RIGHT     ignored     a:ANY         a:ANY     b:ANY
              RESULT   TRUE:BOOL    a:ANY         a:ANY     d.or(b)

              see @ and, dual stuff (inversed jump conditions)
            }
            tmplab := Assembly.GenLabel;
            {push left}
            Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
            {jmptrue}
            Assembly.InsLRefOp(Line,Column,isc_jmp_true_addr,tmplab);
            {push right}
            Assembly.AppendAssembly(TParserNode(Occ[2]).Assembly);
            {jmp on tos-1 = false, rotate, pop}
            Assembly.InsLRefOp(Line,Column,isc_tos1_false_nil_rot_pop_jmp_addr,tmplab);
            {eval no shortcut and}
            Assembly.InsNoOperand(Line,Column,isc_sobinop_or_ign);
            {append label}
            Assembly.AppendLabel(tmplab);
          end
        else
          put_internalerror(2011120432);
      end;
    end;
  DecRec;
end;

{ TExprUnary }

class function TExprUnary.Parse: TParserNode;
begin
  Result := nil;
  put_internalerror(2011112941);
end;

function TExprUnary.Compile: Boolean;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Count=2);
  ASSERT(Occ[0].ClassType = TScanRecord);
  ASSERT(Occ[1] is TParserNode);
  Result := TParserNode(Occ[1]).Compile;
  Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
  case TScanRecord(Occ[0]).TokenType of
    SMES_Plus: Assembly.InsNoOperand(Line,Column,isc_sounop_abs_ign);
    SMES_Minus: Assembly.InsNoOperand(Line,Column,isc_sounop_neg_ign);
    SMES_NOT: Assembly.InsNoOperand(Line,Column,isc_sounop_not_ign);
    else
      put_internalerror(2011120430);
  end;
  DecRec;
end;

{ TExprAssign }

class function TExprAssign.Parse: TParserNode;
begin
  Result := nil;
  put_internalerror(2011112946);
end;

procedure ExprAssembleTOSSetter( Line, Column: Integer; Assembly: TAssembly; name: PUCFS32String );
begin
  {setter}
  if FrameStackNoFun or
     (not FrameStackTopFun^.local_index^.Exists(name)) then
    {default global set}
    Assembly.InsStabLoad(Line,Column,isc_setenv_stab,name)
  else
    begin
      {local set slot in function frame}
      Assembly.InsOperand(Line,Column,isc_fprel_set_slot,
        PtrInt(FrameStackTopFun^.local_index^.Lookup(name)-nil));
    end;
end;

function TExprAssign.Compile: Boolean;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Count=2);
  ASSERT(Occ[0] is TParserNode);
  ASSERT(Occ[1] is TParserNode);
  Result := true;
  if Occ[0].ClassType = TExprId then
    begin
      {id := expr -> env.id := expr or funct.slot[IdSlot(id)] := expr}
      ASSERT(TParserNode(Occ[0]).Count=1);
      ASSERT(TParserNode(Occ[0]).Occ[0].ClassType = TScanRecord);
      {expr}
      Result := TParserNode(Occ[1]).Compile and Result;
      Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
      {setter}
      ExprAssembleTOSSetter(Line,Column,Assembly,TScanRecord(TParserNode(Occ[0]).Occ[0]).Pattern);
    end
  else if Occ[0].ClassType = TExprSelect then
    begin
      {sexpr.id := expr -> sexpr.setm(id,expr)}
      ASSERT(TParserNode(Occ[0]).Count=2);
      ASSERT(TParserNode(Occ[0]).Occ[0] is TParserNode);
      ASSERT(TParserNode(Occ[0]).Occ[1].ClassType = TScanRecord);
      {sexpr}
      Result := TParserNode(TParserNode(Occ[0]).Occ[0]).Compile and Result;
      Assembly.AppendAssembly(TParserNode(TParserNode(Occ[0]).Occ[0]).Assembly);
      {expr}
      Result := TParserNode(Occ[1]).Compile and Result;
      Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
      {setm}
      Assembly.InsStabLoad(Line,Column,isc_setm_stab,
        TScanRecord(TParserNode(Occ[0]).Occ[1]).Pattern);
    end
  else if Occ[0].ClassType = TExprIndex then
    begin
      {oexpr[iexpr] := expr -> oexpr.seti(iexpr,expr)}
      ASSERT(TParserNode(Occ[0]).Count=2);
      ASSERT(TParserNode(Occ[0]).Occ[0] is TParserNode);
      ASSERT(TParserNode(Occ[0]).Occ[1] is TParserNode);
      {oexpr}
      Result := TParserNode(TParserNode(Occ[0]).Occ[0]).Compile and Result;
      Assembly.AppendAssembly(TParserNode(TParserNode(Occ[0]).Occ[0]).Assembly);
      {iexpr}
      Result := TParserNode(TParserNode(Occ[0]).Occ[1]).Compile and Result;
      Assembly.AppendAssembly(TParserNode(TParserNode(Occ[0]).Occ[1]).Assembly);
      {expr}
      Result := TParserNode(Occ[1]).Compile and Result;
      Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
      {seti}
      Assembly.InsNoOperand(Line,Column,isc_seti_ign);
    end
  else
    begin
      put_error_for(Line, Column, ucfs_to_utf8string(cscan_streamid), 'Invalid Assignment.');
      Result := false;
    end;
  DecRec;
end;


end.

