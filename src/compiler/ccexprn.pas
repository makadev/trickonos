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
  SysUtils, commontl, eomsg, ccbase, cscan, csyms, assembl, opcode;

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

procedure ExprAssembleTOSSetter( Line, Column: Integer; Assembly: TAssembly; const name: String );

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
  Assembly.InsStabLoad(Line,Column,isc_o_typecheck_stab,Upcase(TScanRecord(Occ[1]).Pattern));
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
begin
  CreateAssembly;
  IncRec;
  ASSERT(Count=1);
  ASSERT(Occ[0].ClassType = TScanRecord);
  Result := true;
  case TScanRecord(Occ[0]).TokenType of
    SMES_NIL: Assembly.InsOperand(Line,Column,isc_m_load_type,Ord(soload_nil));
    SMES_TRUE: Assembly.InsOperand(Line,Column,isc_m_load_type,Ord(soload_true));
    SMES_FALSE: Assembly.InsOperand(Line,Column,isc_m_load_type,Ord(soload_false));
    SMES_String: Assembly.InsStabLoad(Line,Column,isc_m_load_string_stab,TScanRecord(Occ[0]).Pattern);

    SMES_Int:
      begin
        if StrToIntDef(TScanRecord(Occ[0]).Pattern,-1) >= 0 then
          Assembly.InsOperand(Line,Column,isc_m_load_int_oper,StrToInt(TScanRecord(Occ[0]).Pattern))
        else
          begin
            put_error_for(Line,Column,CurrentStreamID,'Int >= 31 bit currently not supported.');
            Result := false;
          end;
        // Assembly.InsStabLoad(Line,Column,isc_m_load_int_stab,TScanRecord(Occ[0]).Pattern);
      end;
    SMES_IntBin:
      begin
        if StrToIntDef('%'+TScanRecord(Occ[0]).Pattern,-1) >= 0 then
          Assembly.InsOperand(Line,Column,isc_m_load_int_oper,StrToInt('%'+TScanRecord(Occ[0]).Pattern))
        else
          begin
            put_error_for(Line,Column,CurrentStreamID,'Int > 31 bit currently not supported.');
            Result := false;
          end;
        // Assembly.InsStabLoad(Line,Column,isc_m_load_int_stab,'%'+TScanRecord(Occ[0]).Pattern);
      end;
    SMES_IntHex:
      begin
        if StrToIntDef('$'+TScanRecord(Occ[0]).Pattern,-1) >= 0 then
          Assembly.InsOperand(Line,Column,isc_m_load_int_oper,StrToInt('$'+TScanRecord(Occ[0]).Pattern))
        else
          begin
            put_error_for(Line,Column,CurrentStreamID,'Int > 31 bit currently not supported.');
            Result := false;
          end;
        // Assembly.InsStabLoad(Line,Column,isc_m_load_int_stab,'$'+TScanRecord(Occ[0]).Pattern);
      end;
    SMES_IntOct:
      begin
        if StrToIntDef('&'+TScanRecord(Occ[0]).Pattern,-1) >= 0 then
          Assembly.InsOperand(Line,Column,isc_m_load_int_oper,StrToInt('&'+TScanRecord(Occ[0]).Pattern))
        else
          begin
            put_error_for(Line,Column,CurrentStreamID,'Int > 31 bit currently not supported.');
            Result := false;
          end;
        // Assembly.InsStabLoad(Line,Column,isc_m_load_int_stab,'&'+TScanRecord(Occ[0]).Pattern);
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
  Assembly.InsOperand(Line,Column,isc_m_load_type,Ord(soload_list));
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
    Assembly.InsOperand(Line,Column,isc_o_listappend_nr,Count);
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
  Assembly.InsOperand(Line,Column,isc_m_load_type,Ord(soload_dict));
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
              Assembly.InsOperand(Line,Column,isc_m_stackdup_nr,1);
              // stackdup
              if TScanRecord(Occ[i]).TokenType = SMES_ID then
                begin
                  Assembly.AppendAssembly(TParserNode(Occ[i+1]).Assembly);
                  Assembly.InsStabLoad(Occ[i].Line,Occ[i].Column,isc_o_setm_stab,
                    upcase(TScanRecord(Occ[i]).Pattern));
                end
              else
                begin
                  {string into x["<string>"] := bla, case sensitive,
                    needs different order, first <x>, then stabentry, then <expr>}
                  Assembly.InsStabLoad(Occ[i].Line,Occ[i].Column,isc_m_load_string_stab,
                    TScanRecord(Occ[i]).Pattern);
                  Assembly.AppendAssembly(TParserNode(Occ[i+1]).Assembly);
                  Assembly.InsNoOperand(Occ[i].Line,Occ[i].Column,isc_o_seti_ign);
                end;
               // pop
               Assembly.InsOperand(Line,Column,isc_m_pop_nr,1);
            end;
          Inc(i,2);
        end;
    end;
  DecRec;
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
  if FrameStackNoFun or
     (not FrameStackTopFun^.local_index^.Exists(Upcase(TScanRecord(Occ[0]).Pattern))) then
    {default global load}
    Assembly.InsStabLoad(Line,Column,isc_m_getenv_stab,Upcase(TScanRecord(Occ[0]).Pattern))
  else
    begin
      {local load slot in function frame}
      Assembly.InsOperand(Line,Column,isc_m_fprel_load_slot,
        PtrInt(FrameStackTopFun^.local_index^.Lookup(Upcase(TScanRecord(Occ[0]).Pattern))-nil));
    end;
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
  Assembly.InsNoOperand(Line,Column,isc_o_geti_ign);
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
  Assembly.InsStabLoad(Line,Column,isc_o_getm_stab,Upcase(TScanRecord(Occ[1]).Pattern));
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
      Assembly.InsStabLoad(Occ[0].Line,Occ[0].Column,isc_m_load_string_stab,
        Upcase(TScanRecord(TParserNode(Occ[0]).Occ[1]).Pattern));
      {callm}
      Assembly.InsCall(Line,Column, isc_o_callm_nrops, Count-1 );
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
      Assembly.InsCall(Line,Column, isc_o_callcall_nrops, Count-1 );
    end;
  if not check_cc_maxargs(Count-1) then
    begin
      Result := false;
      put_error_for(Line,Column,CurrentStreamID,'Call exceeds max. arg Limit.');
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
        SMES_Plus: Assembly.InsOperand(Line,Column,isc_o_binaryop,Ord(sobinop_add));
        SMES_Minus: Assembly.InsOperand(Line,Column,isc_o_binaryop,Ord(sobinop_sub));
        SMES_Star: Assembly.InsOperand(Line,Column,isc_o_binaryop,Ord(sobinop_mul));
        {SMES_Slash: Assembly.InsIOperand(Line,Column,isc_o_binaryop,Ord(sobinop_div));}
        SMES_DIV: Assembly.InsOperand(Line,Column,isc_o_binaryop,Ord(sobinop_div));
        SMES_OAngel: Assembly.InsOperand(Line,Column,isc_o_compare,Ord(socompare_lt));
        SMES_CAngel: Assembly.InsOperand(Line,Column,isc_o_compare,Ord(socompare_gt));
        SMES_LE: Assembly.InsOperand(Line,Column,isc_o_compare,Ord(socompare_le));
        SMES_GE: Assembly.InsOperand(Line,Column,isc_o_compare,Ord(socompare_ge));
        SMES_EQ: Assembly.InsOperand(Line,Column,isc_o_compare,Ord(socompare_equ));
        SMES_NEQ: Assembly.InsOperand(Line,Column,isc_o_compare,Ord(socompare_neq));
        SMES_SHL,SMES_SHLK: Assembly.InsOperand(Line,Column,isc_o_binaryop,Ord(sobinop_shl));
        SMES_SHR,SMES_SHRK: Assembly.InsOperand(Line,Column,isc_o_binaryop,Ord(sobinop_shr));
        SMES_ROL,SMES_ROLK: Assembly.InsOperand(Line,Column,isc_o_binaryop,Ord(sobinop_rol));
        SMES_ROR,SMES_RORK: Assembly.InsOperand(Line,Column,isc_o_binaryop,Ord(sobinop_ror));
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
            Assembly.InsLRefOp(Line,Column,isc_m_jmp_false_nil_addr,tmplab);
            {push right}
            Assembly.AppendAssembly(TParserNode(Occ[2]).Assembly);
            {jmp on tos-1 = true, rotate, pop}
            Assembly.InsLRefOp(Line,Column,isc_m_tos1_true_rot_pop_jmp_addr,tmplab);
            {eval no shortcut and}
            Assembly.InsOperand(Line,Column,isc_o_binaryop,Ord(sobinop_and));
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
            Assembly.InsLRefOp(Line,Column,isc_m_jmp_true_addr,tmplab);
            {push right}
            Assembly.AppendAssembly(TParserNode(Occ[2]).Assembly);
            {jmp on tos-1 = false, rotate, pop}
            Assembly.InsLRefOp(Line,Column,isc_m_tos1_false_nil_rot_pop_jmp_addr,tmplab);
            {eval no shortcut and}
            Assembly.InsOperand(Line,Column,isc_o_binaryop,Ord(sobinop_or));
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
    SMES_Plus: Assembly.InsOperand(Line,Column,isc_o_unaryop,Ord(sounop_abs));
    SMES_Minus: Assembly.InsOperand(Line,Column,isc_o_unaryop,Ord(sounop_neg));
    SMES_NOT: Assembly.InsOperand(Line,Column,isc_o_unaryop,Ord(sounop_not));
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

procedure ExprAssembleTOSSetter( Line, Column: Integer; Assembly: TAssembly; const name: String );
begin
  {setter}
  if FrameStackNoFun or
     (not FrameStackTopFun^.local_index^.Exists(name)) then
    {default global set}
    Assembly.InsStabLoad(Line,Column,isc_m_setenv_stab,name)
  else
    begin
      {local set slot in function frame}
      Assembly.InsOperand(Line,Column,isc_m_fprel_set_slot,
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
      ExprAssembleTOSSetter(Line,Column,Assembly,Upcase(TScanRecord(TParserNode(Occ[0]).Occ[0]).Pattern));
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
      Assembly.InsStabLoad(Line,Column,isc_o_setm_stab,
        Upcase(TScanRecord(TParserNode(Occ[0]).Occ[1]).Pattern));
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
      Assembly.InsNoOperand(Line,Column,isc_o_seti_ign);
    end
  else
    begin
      put_error_for(Line, Column, CurrentStreamID, 'Invalid Assignment.');
      Result := false;
    end;
  DecRec;
end;


end.

