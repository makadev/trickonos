{   Unit for statement node parsing and compilation

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

unit ccstatn;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, commontl, eomsg, ucfs, ccbase, ccexprp, cscan, csyms, coreobj,
  opcode, assembl;

const
  SymSetStatFirst = [SMES_PUT, SMES_IF, SMES_FUNCTION,
                     SMES_REPEAT, SMES_WHILE,
                     SMES_FOREACH, SMES_CLASS]+EXPR_FIRST;

  C_Name_Constructor = 'CREATE';
  C_Name_Self = 'SELF';
  C_Name_Result = 'RESULT';
  C_Name_Vararg = 'VARARGS';

type
  {wrapper that returns a real TPLStat, or is an Expr.}
  TPLStat = class(TParserNode)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {ID+ -- varlist}
  TFuncStubVar = class(TParserNode)
    class function Parse: TParserNode; override;
    function Compile: Boolean; override;
  end;

  {name(,ID [:= EXPR | nil!])*,VARLIST,STATS}
  TPLFunction = class(TPLStat)
    class function Parse: TParserNode; override;
    function Compile: Boolean; override;
  end;

  {(EXPR,STATS)+,STATS -- last stats is else, may be empty}
  TPLIfElse = class(TPLStat)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {EXPR,STATS}
  TPLWhile = class(TPLStat)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {STATS,EXPR}
  TPLRepeat = class(TPLStat)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {ID,EXPR,STATS (for list iterator)
   ID,ID,EXPR,STATS (for dict iterator)}
  TPLForeach = class(TPLStat)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {ID,FunctionDecl*}
  TPLClass = class(TPLStat)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {list of SMES_PUT}
  TPLPuts = class(TPLStat)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {Default: name(,EXPR)*}
  TPLDefaultMCall = class(TPLStat)
    public
      class function Parse: TParserNode; override;
  end;

  {Default: name(,EXPR)*,STATS}
  TPLDefaultMBlockCall = class(TPLStat)
    public
      class function Parse: TParserNode; override;
  end;

  {list of TPLStat}
  TPLStats = class(TParserNode)
    public
      class function Parse: TParserNode; override;
      function Compile: Boolean; override;
  end;

  {root - wrapper for TPLStats which reads first token}
  TTemplateFile = class(TPLStats)
    public
      class function Parse: TParserNode; override;
  end;

procedure ParseArgs( node: TParserNode; sterminator: TSMESymbol );
procedure ParseParams( node: TParserNode; sterminator: TSMESymbol );

type
  TCTPLStatCls = class of TPLStat;
  TCTPLMCallCls = class of TPLDefaultMCall;
  TCTPLMBlockCallCls = class of TPLDefaultMBlockCall;
  TTPLMacroType = (
    tpl_none,
    tpl_mblock,
    tpl_mcall
  );

procedure RegisterMacroCallNode( const id: String; nodecls: TCTPLMCallCls );
procedure RegisterMacroBlockNode( const id: String; nodecls: TCTPLMBlockCallCls );

function LookupMacroNode( const id: String; out node: TCTPLStatCls ): TTPLMacroType;

implementation

uses ccexprn;

procedure ParseArgs(node: TParserNode; sterminator: TSMESymbol);
{ PARSES:
    ARGS ::= ['(' [EXPR (',' EXPR)*] ')'] <sterminator>
  Set CurrentToken to First after <sterminator>}
begin
  ExpectSymSet([SMES_ORound, sterminator]);
  if CurrentToken.TokenType = SMES_ORound then
    begin
      NextToken;
      if CurrentToken.TokenType <> SMES_CRound then
        begin
          repeat
            node.AddOcc(ParseExpr([SMES_CRound,SMES_Comma]));
            if CurrentToken.TokenType = SMES_Comma then
              begin
                NextToken;
                ExpectSymSet(EXPR_FIRST);
              end;
          until CurrentToken.TokenType = SMES_CRound;
        end;
      NextToken;
    end;
  ExpectSym(sterminator);
  NextToken;
end;

procedure ParseParams(node: TParserNode; sterminator: TSMESymbol);
{ PARSES:
    ARGS ::= ['(' [id [:= EXPR] (',' id [:= EXPR])*] ')'] <sterminator>
  Set CurrentToken to First after <sterminator>}
begin
  ExpectSymSet([SMES_ORound, sterminator]);
  if CurrentToken.TokenType = SMES_ORound then
    begin
      NextToken;
      if CurrentToken.TokenType <> SMES_CRound then
        begin
          ExpectSym(SMES_ID);
          repeat
            node.AddOcc(CurrentToken);
            NextToken;
            if CurrentToken.TokenType = SMES_Assign then
              begin
                {found id :=, expect expression}
                NextToken;
                node.AddOcc(ParseExpr([SMES_CRound,SMES_Comma]));
              end
            else
              node.AddOcc(nil);
            ExpectSymSet([SMES_CRound,SMES_Comma]);
            if CurrentToken.TokenType = SMES_Comma then
              begin
                NextToken;
                ExpectSym(SMES_ID);
              end;
          until CurrentToken.TokenType = SMES_CRound;
        end;
      NextToken;
    end;
  ExpectSym(sterminator);
  NextToken;
end;

{ TPLClass }

class function TPLClass.Parse: TParserNode;
begin
  IncRec;
  ExpectSym(SMES_CLASS);
  Result := self.Create;
  Result.SetLineInfoFrom(CurrentToken);
  NextToken;
  {clsname}
  ExpectSym(SMES_ID);
  Result.SetLineInfoFrom(CurrentToken);
  Result.AddOcc(CurrentToken);
  NextToken;
  {methods}
  repeat
    ExpectSymSet([SMES_FUNCTION,SMES_END]);
    if CurrentToken.TokenType = SMES_FUNCTION then
      begin
        Result.AddOcc( TPLFunction.Parse );
        Continue;
      end;
  until CurrentToken.TokenType = SMES_END;
  NextToken;
  ExpectSym(SMES_SColon);
  NextToken;
  DecRec;
end;

function TPLClass.Compile: Boolean;
var i: Integer;
begin
  CreateAssembly;
  IncRec;
  ASSERT( Count >= 1 );
  ASSERT( Occ[0].ClassType = TScanRecord );
  Result := true;
  {create a new class}
  Assembly.InsStabLoad(Line,Column,isc_m_class_stab,TScanRecord(Occ[0]).Pattern);
  {push class frame, so functions now they are methods and a class is on stack}
  FrameStackPush(ccft_class);
  for i := 1 to Count-1 do
    begin
      {compile methods, create method decls}
      Assert(Occ[i] is TParserNode);
      Result :=  TParserNode(Occ[i]).Compile and Result;
      Assembly.AppendAssembly(TParserNode(Occ[i]).Assembly);
    end;
  {write back Name, pop TOS}
  ExprAssembleTOSSetter(Line,Column,Assembly,TScanRecord(Occ[0]).Pattern);
  Assembly.InsOperand(Line,Column,isc_m_pop_nr,1);
  {pop cls frame}
  FrameStackPop;
  DecRec;
end;

{ TPLForeach }

class function TPLForeach.Parse: TParserNode;
begin
  { foreach [ID | (ID:ID) ] in <expr> do [begin] <stats> end; }
  IncRec;
  ExpectSym(SMES_FOREACH);
  Result := self.Create;
  Result.SetLineInfoFrom(CurrentToken);
  NextToken;
  ExpectSym(SMES_ID);
  Result.AddOcc(CurrentToken);
  NextToken;
  if CurrentToken.TokenType = SMES_Colon then
    begin
      NextToken;
      ExpectSym(SMES_ID);
      Result.AddOcc(CurrentToken);
      NextToken;
    end;
  ExpectSym(SMES_IN);
  NextToken;
  Result.AddOcc(ParseExpr([SMES_DO]));
  NextToken;
  if CurrentToken.TokenType = SMES_BEGIN then
    NextToken;
  Result.AddOcc(TPLStats.Parse);
  ExpectSym(SMES_END);
  NextToken;
  ExpectSym(SMES_SColon);
  NextToken;
  DecRec;
end;

function TPLForeach.Compile: Boolean;
var us: PUCFS32String;
begin
  CreateAssembly;
  IncRec;
  ASSERT((Count = 3) or (Count = 4));
  ASSERT(Occ[0].ClassType = TScanRecord);
  ASSERT((Count = 3) or ((Count = 4) and (Occ[1].ClassType = TScanRecord)));
  ASSERT((Count = 3) or ((Count = 4) and (Occ[2] is TParserNode)));
  ASSERT((Count = 3) or ((Count = 4) and (Occ[3] is TParserNode)));
  ASSERT((Count = 4) or ((Count = 3) and (Occ[1] is TParserNode)));
  ASSERT((Count = 4) or ((Count = 3) and (Occ[2] is TParserNode)));

  {create loopframe for break/continue}
  FrameStackPush(ccft_loop);
  FrameStackTop^.reentry_lab := Assembly.GenLabel;
  FrameStackTop^.exit_lab := Assembly.GenLabel;

  {push eval expr}
  if Count = 3 then
    begin
      Result := TParserNode(Occ[1]).Compile;
      {tell lower frames, this loop needs one extra stackslot (need for exit)}
      FrameStackTop^.stackuse := 1;
      Result := TParserNode(Occ[2]).Compile and Result;
      Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);
      {emit iterator creation}
      {$WARNING doh.. obvious hack}
      us := ucfs_a7us('ITERATOR');
      Assembly.InsStabLoad(Line,Column,isc_m_load_string_stab,us);
      ucfs_release(us);
      Assembly.InsCall(Line,Column,isc_o_callm_nrops,0);
      {introlabel}
      Assembly.AppendLabel(FrameStackTop^.reentry_lab);
      {dup iterator, otherwise it is gone after next call}
      Assembly.InsOperand(Line,Column,isc_m_stackdup_nr,1);
      {iter fails or pushes Iterator Object, call Next which pushes bool}
      us := ucfs_a7us('NEXT');
      Assembly.InsStabLoad(Line,Column,isc_m_load_string_stab,us);
      ucfs_release(us);
      Assembly.InsCall(Line,Column,isc_o_callm_nrops,0);
      {check if iterator returned false -> exit on false, proceed on true}
      Assembly.InsLRefOp(Line,Column,isc_m_jmppop_false_nil_addr,FrameStackTop^.exit_lab);
      {pop bool if true otherwise}
      Assembly.InsOperand(Line,Column,isc_m_pop_nr,1);
      {dup Iterator again, for member access call}
      Assembly.InsOperand(Line,Column,isc_m_stackdup_nr,1);
      {get the current listelement and set ID}
      us := ucfs_a7us('CURRENT');
      Assembly.InsStabLoad(Line,Column,isc_o_getm_stab,us);
      ucfs_release(us);
      ExprAssembleTOSSetter(Column,Line,Assembly,TScanRecord(Occ[0]).Pattern);
      Assembly.InsOperand(Line,Column,isc_m_pop_nr,1);
      {stats code}
      Assembly.AppendAssembly(TParserNode(Occ[2]).Assembly);
    end
  else
    begin
      Result := TParserNode(Occ[2]).Compile;
      {tell lower frames, this loop needs one extra stackslot (need for exit)}
      FrameStackTop^.stackuse := 1;
      Result := TParserNode(Occ[3]).Compile and Result;
      Assembly.AppendAssembly(TParserNode(Occ[2]).Assembly);
      {emit iterator start/next}
      us := ucfs_a7us('ITERATOR');
      Assembly.InsStabLoad(Line,Column,isc_m_load_string_stab,us);
      ucfs_release(us);
      Assembly.InsCall(Line,Column,isc_o_callm_nrops,0);
      {introlabel}
      Assembly.AppendLabel(FrameStackTop^.reentry_lab);
      {dup iterator for next call}
      Assembly.InsOperand(Line,Column,isc_m_stackdup_nr,1);
      {iter fails or pushes Iterator Object -> set ID:ID from key, value}
      us := ucfs_a7us('NEXT');
      Assembly.InsStabLoad(Line,Column,isc_m_load_string_stab,us);
      ucfs_release(us);
      Assembly.InsCall(Line,Column,isc_o_callm_nrops,0);
      {check if iterator returned true/false -> exit on false, proceed on true}
      Assembly.InsLRefOp(Line,Column,isc_m_jmppop_false_nil_addr,FrameStackTop^.exit_lab);
      {pop bool otherwise}
      Assembly.InsOperand(Line,Column,isc_m_pop_nr,1);
      {dup iterator 2 member access calls}
      Assembly.InsOperand(Line,Column,isc_m_stackdup_nr,2);
      us := ucfs_a7us('CURRENTKEY');
      Assembly.InsStabLoad(Line,Column,isc_o_getm_stab,us);
      ucfs_release(us);
      ExprAssembleTOSSetter(Column,Line,Assembly,TScanRecord(Occ[0]).Pattern);
      Assembly.InsOperand(Line,Column,isc_m_pop_nr,1);
      us := ucfs_a7us('CURRENTVALUE');
      Assembly.InsStabLoad(Line,Column,isc_o_getm_stab,us);
      ucfs_release(us);
      ExprAssembleTOSSetter(Column,Line,Assembly,TScanRecord(Occ[1]).Pattern);
      Assembly.InsOperand(Line,Column,isc_m_pop_nr,1);
      {stats code}
      Assembly.AppendAssembly(TParserNode(Occ[3]).Assembly);
    end;

  {back jmp @ intro}
  Assembly.InsLRefOp(Line,Column,isc_m_jmp_addr,FrameStackTop^.reentry_lab);
  {exitlabel}
  Assembly.AppendLabel(FrameStackTop^.exit_lab);
  {cleanup -> pop iterator object}
  Assembly.InsOperand(Line,Column,isc_m_pop_nr,1);
  FrameStackPop;
  DecRec;
end;

{ TPLRepeat }

class function TPLRepeat.Parse: TParserNode;
begin
  { repeat <stats> until <expr>; }
  ExpectSym(SMES_REPEAT);
  IncRec;
  Result := self.Create;
  Result.SetLineInfoFrom(CurrentToken);
  NextToken;
  Result.AddOcc(TPLStats.Parse);
  ExpectSym(SMES_UNTIL);
  NextToken;
  Result.AddOcc(ParseExpr([SMES_SColon]));
  NextToken;
  DecRec;
end;

function TPLRepeat.Compile: Boolean;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Count=2);
  ASSERT(Occ[0] is TParserNode);
  ASSERT(Occ[1] is TParserNode);

  {create a loop frame}
  FrameStackPush(ccft_loop);
  FrameStackTop^.reentry_lab := Assembly.GenLabel;
  FrameStackTop^.exit_lab := Assembly.GenLabel;

  {compile and add stats, prepend loop label}
  Result := TParserNode(Occ[0]).Compile;
  if not Assigned(TParserNode(Occ[0]).Assembly.FirstAsLab) then
    TParserNode(Occ[0]).Assembly.PrependLabel;
  Assembly.AppendAssembly(TParserNode(Occ[0]).Assembly);

  {append reentry}
  Assembly.AppendLabel(FrameStackTop^.reentry_lab);

  {compile and add expression}
  Result := TParserNode(Occ[1]).Compile and Result;
  Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);

  {jmppop on false/nil @ loop intro, otherwise pop expression val}
  Assembly.InsLRefOp(Line,Column,isc_m_jmppop_false_nil_addr,Assembly.FirstAsLab);
  Assembly.InsOperand(Line,Column,isc_m_pop_nr,1);

  {append exitlabel}
  Assembly.AppendLabel(FrameStackTop^.exit_lab);

  {pop frame}
  FrameStackPop;
  DecRec;
end;

{ TPLWhile }

class function TPLWhile.Parse: TParserNode;
begin
  IncRec;
  ExpectSym(SMES_WHILE);
  Result := self.Create;
  Result.SetLineInfoFrom(CurrentToken);
  NextToken;
  Result.AddOcc(ParseExpr([SMES_DO]));
  NextToken;
  if CurrentToken.TokenType = SMES_BEGIN then
    NextToken;
  Result.AddOcc(TPLStats.Parse);
  ExpectSym(SMES_END);
  NextToken;
  ExpectSym(SMES_SColon);
  NextToken;
  DecRec;
end;

function TPLWhile.Compile: Boolean;
begin
  { while <expr> do [begin] <stats> end; }
  CreateAssembly;
  IncRec;
  ASSERT(Count=2);
  ASSERT(Occ[0] is TParserNode);
  ASSERT(Occ[1] is TParserNode);

  {compile and add expression (intro label is added afterwards)}
  Result := TParserNode(Occ[0]).Compile;
  Assembly.AppendAssembly(TParserNode(Occ[0]).Assembly);

  {prepend while intro label}
  if not Assigned(Assembly.FirstAsLab) then
    Assembly.PrependLabel;

  {create a loop frame}
  FrameStackPush(ccft_loop);
  FrameStackTop^.reentry_lab := Assembly.FirstAsLab;
  FrameStackTop^.exit_lab := Assembly.GenLabel;

  {append exit jmppop on false/nil, otherwise pop expression result explicit}
  Assembly.InsLRefOp(Line,Column,isc_m_jmppop_false_nil_addr,FrameStackTop^.exit_lab);
  Assembly.InsOperand(Line,Column,isc_m_pop_nr,1);

  {compile and add stats}
  Result := TParserNode(Occ[1]).Compile and Result;
  Assembly.AppendAssembly(TParserNode(Occ[1]).Assembly);

  {jmp @ head}
  Assembly.InsLRefOp(Line,Column,isc_m_jmp_addr,FrameStackTop^.reentry_lab);

  {append exitlabel}
  Assembly.AppendLabel(FrameStackTop^.exit_lab);

  {pop frame}
  FrameStackPop;
  DecRec;
end;

{ TFuncStubVar }

class function TFuncStubVar.Parse: TParserNode;
begin
  IncRec;
  ExpectSym(SMES_VAR);
  Result := self.Create;
  Result.SetLineInfoFrom(CurrentToken);
  NextToken;
  repeat
    ExpectSym(SMES_ID);
    Result.AddOcc(CurrentToken);
    NextToken;
    ExpectSymSet([SMES_Comma,SMES_SColon]);
    if CurrentToken.TokenType = SMES_Comma then
      begin
        NextToken;
        continue;
      end;
  until CurrentToken.TokenType = SMES_SColon;
  NextToken;
  DecRec;
end;

function TFuncStubVar.Compile: Boolean;
begin
  put_internalerror(2011121010);
  Result := false;
end;

{ TPLFunction }

class function TPLFunction.Parse: TParserNode;
begin
  IncRec;
  ExpectSym(SMES_FUNCTION);
  NextToken;
  Result := Self.Create;
  ExpectSym(SMES_ID);
  Result.SetLineInfoFrom(CurrentToken);
  Result.AddOcc(CurrentToken);
  NextToken;
  ParseParams(Result,SMES_SColon);
  if CurrentToken.TokenType = SMES_VAR then
    Result.AddOcc(TFuncStubVar.Parse)
  else
    Result.AddOcc(TFuncStubVar.Create); // <- dummy, needed in compilation
  {pascalish, compat}
  if CurrentToken.TokenType = SMES_BEGIN then
    NextToken;
  Result.AddOcc(TPLStats.Parse);
  ExpectSym(SMES_END);
  NextToken;
  ExpectSym(SMES_SColon);
  NextToken;
  DecRec;
end;

function TPLFunction.Compile: Boolean;
var idxtab: PHashTrie;
    i,argsn,argsf,slotn: MachineInt;
    varglab, bodylab: TLabelNode;
    dexpr,varg,varargexpr,ismethod: Boolean;
begin
  CreateAssembly;
  IncRec;
  Result := true;

  {check if this is a method decl}
  ismethod := (not FrameStackEmpty) and
              (FrameStackTop^.ftype = ccft_class);

  {push a frame for nodes below}
  FrameStackPush(ccft_function);
  {add exit label (jmp @ ret), for call entry, fast exit}
  FrameStackTop^.exit_lab := Assembly.GenLabel;
  {add exitdecl label (jmp after ret), for the initial registration which
   jumps above the code and only executes the decl whenever this codepiece is
   executed}
  FrameStackTop^.exitdecl_lab := Assembly.GenLabel;
  {add callintrolabel, the entrypoint for a functioncall}
  FrameStackTop^.callintro_lab := Assembly.GenLabel;
  idxtab := FrameStackTop^.local_index;

  {assemble the initial intro:
    loc_0: jmp @exitdecl_lab
    .callintro_lab: }
  Assembly.InsLRefOp(Line,Column,isc_m_jmp_addr,FrameStackTop^.exitdecl_lab);
  Assembly.AppendLabel(FrameStackTop^.callintro_lab);

  (*
     Most of the checking is done Caller (better, vm) side.
     A Call must be registered with
      - ARGF (fixed argument = without Default expr.)
      - ARGN (arguments = inclusive Default expr., Vararg)
      - SLOTN (number of slots = stackframe size inclusive args and local vars)
      - CVAR (boolean, wether vararg passing is on)

      On Call Translation, this will be checked against the Caller side and
      if possible (Operands >= ARGN) prepared beforehand.
      The only Problem left is
        1. calculate ARGF,ARGN,SLOTN,CVAR (and check syntax)
        2. Default Expression Evaluation whenever ARGF <= Operands < ARGN
        3. Default Expression Evaluation when CVAR and Operands < ARGN

      The Solution for 2./3. is a jumptable.
      The VM does Jump @ BaseLabel+(Operands-ARGF) whenver
        ARGF <= Operands < ARGN and the function Header contains a
        block of jumps after baselabel, where each directs to the
        next default expr. evaluation.
      ->
        .callintro_lab:
          ...
          jmp_addr @leftof_args_0
          jmp_addr @leftof_args_1
          jmp_addr @leftof_args_2 (-> can be removed)

        .leftof_args_2:
          - evaluate last default expr on TOS.
          - move TOS into slot ARGN-1 (or argn-2 if vararg without def. expr...)
        .leftof_args_1:
          - evaluate last default expr on TOS.
          - move TOS into slot ARGN (or argn-1 if vararg without def. expr...)

        .leftof_args_0:
          bodycode
      -> jmptable not needed with fixed args only (or fixed args + vararg without
         def. expr..)
  *)

  (*
   * ARGUMENT CHECKS AND COMPILATION
   *
   *)

  {assemble the "argument loader" code. for params with Default Expr, we need
   to assemble an additional test}
  dexpr := false; // -> set true when first default expression is hit
  varg := false;  // -> hit vararg
  varargexpr := false; // -> vararg got default expr
  argsf := 0;     // -> nr fixed arguments (without default expr, as they are not fixed)
  argsn := 0;     // -> nr args (argument slots)
  i := 1;
  while Occ[i].ClassType <> TFuncStubVar do
    begin
      {handle/check expression}
      if Assigned(Occ[i+1]) then
        begin
          if not dexpr then
            dexpr := true;
          {first assemble the expr code, but dont append it as long as we dont
           know exact arg count. this is only to ensure that no parameter (stackloc)
           will be assembled, which we dont know at this point}
          Result := TParserNode(Occ[i+1]).Compile and Result;
          {add a label before default expr}
          TParserNode(Occ[i+1]).Assembly.PrependLabel;
        end
      else
        begin
          if dexpr and
             (ucfs_to_string(TScanRecord(Occ[i]).Pattern) <> C_Name_Vararg) then
            begin
              Result := false;
              put_error_for(Occ[i].Line,Occ[i].Column,ucfs_to_string(cscan_streamid),
                'Only Parameters with Default Expression or Vararg is allowed after Default Expressions.');
            end;
        end;
      {handle/check ident}
      if idxtab^.Lookup(TScanRecord(Occ[i]).Pattern) = nil then
        begin
          Inc(argsn,1);
          if ucfs_to_string(TScanRecord(Occ[i]).Pattern) <> C_Name_Vararg then
            begin
              if not dexpr then
                Inc(argsf,1);
            end
          else
            begin
              varg := true;
              if Assigned(Occ[i+1]) then
                varargexpr := true;
            end;
          idxtab^.Add(TScanRecord(Occ[i]).Pattern,PtrInt(argsn+1)+nil);
        end
      else
        begin
          put_error_for(Occ[i].Line,Occ[i].Column,ucfs_to_string(cscan_streamid),
            'Duplicate Identifier '+ucfs_to_string(TScanRecord(Occ[i]).Pattern));
          Result := false;
        end;
      Inc(i,2);
    end;

  if not check_cc_maxargs(argsn) then
    begin
      put_error_for(Line,Column,ucfs_to_string(cscan_streamid),'Function exceeds max. arg Limit.');
      Result := false;
    end;

  slotn := argsn+1; // up til here, argsn slots for arguments, 1 additional for function obj

  (*
   * ARGUMENT CODE GENERATION (Jump Table/Function Entrycode,
   *                           Default Arg Creation)
   *
   *)

  if Result then
    begin
      bodylab := nil;
      varglab := nil;
      {so far, the header was checked, assemble the jumptable
        everthing from argn...argf+2 needs a table entry.
        argn-1 if vararg has no Def. Expr.}

      {only need a body jump when argsf <> argsn.. otherwise we have
       fixed args and .callintrolab is the correct point}
      if argsf <> argsn then
        begin
          {the body jump if no def. expr is evaluated}
          bodylab := Assembly.GenLabel;
          Assembly.InsLRefOp(Line,Column,isc_m_jmp_addr,bodylab);
        end;

      if varargexpr or (not varg) then
        begin
          {add jumptable entries}
          for i := argsn downto argsf+2 do
            begin
              Assembly.InsLRefOp(Line,Column,isc_m_jmp_addr,
                TParserNode(Occ[i*2]).Assembly.FirstAsLab);
            end;
          {add code}
          for i := argsf+1 to argsn do
            begin
              Assembly.AppendAssembly(TParserNode(Occ[i*2]).Assembly);
              Assembly.InsOperand(Line,Column,isc_m_fprel_set_slot,i+1);
              Assembly.InsOperand(Line,Column,isc_m_pop_nr,1);
            end;
        end
      else
        begin
          {initialize vararg to nil if its not with default expr
            -> need an extra entry}
          varglab := Assembly.GenLabel;
          Assembly.InsLRefOp(Line,Column,isc_m_jmp_addr,varglab);
          {add jumptable entries}
          for i := argsn-1 downto argsf+2 do
            begin
              Assembly.InsLRefOp(Line,Column,isc_m_jmp_addr,
                TParserNode(Occ[i*2]).Assembly.FirstAsLab);
            end;
          {add code, one node short since vararg is added seperate}
          for i := argsf+1 to argsn-1 do
            begin
              Assembly.AppendAssembly(TParserNode(Occ[i*2]).Assembly);
              Assembly.InsOperand(Line,Column,isc_m_fprel_set_slot,i+1);
              Assembly.InsOperand(Line,Column,isc_m_pop_nr,1);
            end;
        end;


      {vararg init}
      if Assigned(varglab) then
        begin
          Assembly.AppendLabel(varglab);
          Assembly.InsOperand(Line,Column,isc_m_load_type,Ord(soload_nil));
          Assembly.InsOperand(Line,Column,isc_m_fprel_set_slot,argsn+1);
          Assembly.InsOperand(Line,Column,isc_m_pop_nr,1);
        end;

      {body label}
      if Assigned(bodylab) then
        Assembly.AppendLabel(bodylab);
    end;

  (*
   * FUNCTION STATS CODE
   *
   *)

   if ismethod then
     begin
       {this is a method, the function object is not accessible (stack layout
        is different, object is at slot 1, function is "out of frame").
        since method can i.g. be accessed by self, we register self for
        object access, if its not overriden}
       if idxtab^.Lookup(C_Name_Self) = nil then
         begin
           i := 1;
           idxtab^.Add(C_Name_Self,PtrInt(i)+nil);
         end;
     end
   else
     begin
       {Register Function Name as Variable which contains itself
        (the function object @ slot 1) if it wasnt overridden by arguments}
       if idxtab^.Lookup(TScanRecord(Occ[0]).Pattern) = nil then
         begin
           i := 1;
           idxtab^.Add(TScanRecord(Occ[0]).Pattern,PtrInt(i)+nil);
         end;
     end;

   if ismethod and
      (ucfs_to_string(TScanRecord(Occ[0]).Pattern) = C_Name_Constructor) then
     begin
       {special method create, map Result to slot 1, since this is the
        instance that must be returned}
       if idxtab^.Lookup(C_Name_Result) = nil then
         begin
           i := 1;
           idxtab^.Add(C_Name_Result,PtrInt(i)+nil);
         end;
     end
   else
     begin
       {register Result for return arguments (and slot number so ret nows which
        slot is passed back)}
       if idxtab^.Lookup(C_Name_Result) = nil then
         begin
           Inc(slotn,1);
           idxtab^.Add(C_Name_Result,PtrInt(slotn)+nil);
         end;
     end;

   {now, register all vars.. VAR node is @ Occ[argsn*2], so no need for search,
    well Count-2 may be simpler xD}
   for i := 0 to TParserNode(Occ[Count-2]).Count-1 do
     begin
       if idxtab^.Lookup(TScanRecord(TParserNode(Occ[Count-2]).Occ[i]).Pattern) = nil then
         begin
           Inc(slotn,1);
           idxtab^.Add(TScanRecord(TParserNode(Occ[Count-2]).Occ[i]).Pattern,PtrInt(slotn)+nil);
         end
       else
         begin
           put_error_for(TParserNode(Occ[Count-2]).Occ[i].Line,
                         TParserNode(Occ[Count-2]).Occ[i].Column,ucfs_to_string(cscan_streamid),
             'Duplicate Identifier '+ucfs_to_string(TScanRecord(TParserNode(Occ[Count-2]).Occ[i]).Pattern));
           Result := false;
         end;
     end;

   {hohoho.. now code @ Occ[Count-1]}
   Result := TParserNode(Occ[Count-1]).Compile and Result;
   Assembly.AppendAssembly(TParserNode(Occ[Count-1]).Assembly);

   {exitlabel}
   Assembly.AppendLabel(FrameStackTop^.exit_lab);
   {ret RESULT}
   Assembly.InsOperand(Line,Column,isc_m_ret_slot,PtrInt(idxtab^.Lookup(C_Name_Result)-nil));

   {exit decl}
   Assembly.AppendLabel(FrameStackTop^.exitdecl_lab);
   {Function DECL}
   Assembly.InsOperand(Line,Column,isc_m_load_int_oper,argsf);         // fixed args
   Assembly.InsOperand(Line,Column,isc_m_load_int_oper,argsn);         // all args
   Assembly.InsOperand(Line,Column,isc_m_load_int_oper,slotn);         // slots in frame (local vars inclusive args)
   if varg then                                                         // vararg or not
     Assembly.InsOperand(Line,Column,isc_m_load_type,Ord(soload_true))
   else
     Assembly.InsOperand(Line,Column,isc_m_load_type,Ord(soload_false));
   Assembly.InsLRefOp(Line,Column,isc_m_decl_fun_addr,FrameStackTop^.callintro_lab); // and declare

   FrameStackPop;

   if ismethod then
     begin
       // declare method in current class
       // this will enter the function into the class
       Assembly.InsStabLoad(Line,Column,isc_m_decl_method_stab,TScanRecord(Occ[0]).Pattern);
     end
   else
     begin
       // compile TOS setting after FrameStackPop since it will set it in the frame below
       // which may be again a function
       ExprAssembleTOSSetter(Line,Column,Assembly,TScanRecord(Occ[0]).Pattern); // function @ TOS -> set by name
       Assembly.InsOperand(Line,Column,isc_m_pop_nr,1); // pop after setting
     end;

   if not check_cc_maxframe(slotn) then
     begin
       put_error_for(Line,Column,ucfs_to_string(cscan_streamid),'Function exceeds max. frame Limit.');
       Result := false;
     end;
   DecRec;
end;

{ TPLIfElse }

class function TPLIfElse.Parse: TParserNode;
var pcompat: Boolean;
begin
  IncRec;
  {if EXPR THEN TPL_STATS (elseif EXPR THEN TPL_STATS)* [else TPL_STATS] end ;}
  ExpectSym(SMES_IF);
  Result := self.Create;
  Result.SetLineInfoFrom(CurrentToken);
  NextToken;
  Result.AddOcc(ParseExpr([SMES_THEN]));
  NextToken;
  {pascalish, compat}
  if CurrentToken.TokenType = SMES_BEGIN then
    begin
      pcompat := true;
      NextToken;
    end
  else 
    pcompat := false;
  Result.AddOcc(TPLStats.Parse);
  if not pcompat then
    ExpectSymSet([SMES_END,SMES_ELSEIF,SMES_ELSE])
  else
    begin
      ExpectSym(SMES_END);
      NextToken;
    end;
  while CurrentToken.TokenType = SMES_ELSEIF  do
    begin
      NextToken;
      Result.AddOcc(ParseExpr([SMES_THEN]));
      NextToken;
      {pascalish, compat}
      if pcompat then
        begin
          ExpectSym(SMES_BEGIN);
          NextToken;
        end;
      Result.AddOcc(TPLStats.Parse);
      if pcompat then
        begin
          ExpectSym(SMES_END);
          NextToken;
        end;
    end;
  if not pcompat then
    ExpectSymSet([SMES_END,SMES_ELSE])
  else
    ExpectSymSet([SMES_SColon,SMES_ELSE]);
  if CurrentToken.TokenType = SMES_ELSE then
    begin
      NextToken;
      {pascalish, compat}
      if pcompat then
        begin
          ExpectSym(SMES_BEGIN);
          NextToken;
        end;
      Result.AddOcc(TPLStats.Parse);
      if pcompat then
        begin
          ExpectSym(SMES_END);
          NextToken;
        end;
    end
  else
    Result.AddOcc(TPLStats.Create);
  if not pcompat then
    begin
      ExpectSym(SMES_END);
      NextToken;
    end;
  ExpectSym(SMES_SColon);
  NextToken;
  DecRec;
end;

function TPLIfElse.Compile: Boolean;
var i: MachineInt;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Count>=3);
  Result := true;
  for i := 0 to Count-1 do
    begin
      ASSERT(Occ[i] is TParserNode);
      Result := TParserNode(Occ[i]).Compile and Result;
    end;

  {append exitlabel to last STATS (else stats)}
  if not Assigned(TParserNode(Occ[Count-1]).Assembly.LastAsLab) then
    TParserNode(Occ[Count-1]).Assembly.AppendLabel;

  i := 0;
  while i < (Count-1) do
    begin
      {append exit jump to stats}
      TParserNode(Occ[i+1]).Assembly.InsLRefOp(Occ[Count-1].Line,Occ[Count-1].Column,
        isc_m_jmp_addr,TParserNode(Occ[Count-1]).Assembly.LastAsLab);
      {append next label to stats}
      TParserNode(Occ[i+1]).Assembly.AppendLabel;
      {expr->TOS}
      Assembly.AppendAssembly(TParserNode(Occ[i]).Assembly);
      {jmpfalsenil(TOS),Pop -> next label}
      Assembly.InsLRefOp(Occ[Count-1].Line,Occ[Count-1].Column,
        isc_m_jmppop_false_nil_addr,TParserNode(Occ[i+1]).Assembly.LastAsLab);
      {otherwise, explict Pop expr}
      Assembly.InsOperand(Occ[i+1].Line,Occ[i+1].Column,
        isc_m_pop_nr,1);
      Assembly.AppendAssembly(TParserNode(Occ[i+1]).Assembly);
      {> expr, jmpfalsenil next, stats, jmp exit, :next}
      Inc(i,2);
    end;

  {append else stats}
  Assembly.AppendAssembly(TParserNode(Occ[Count-1]).Assembly);
  DecRec;
end;

{ TPLDefaultMBlockCall }

class function TPLDefaultMBlockCall.Parse: TParserNode;
begin
  IncRec;
  ExpectSym(SMES_ID);
  Result := self.Create;
  Result.SetLineInfoFrom(CurrentToken);
  Result.AddOcc(CurrentToken);
  Result.AddOcc(ParseExpr([SMES_Colon]));
  NextToken;
  Result.AddOcc(TPLStats.Parse);
  ExpectSym(SMES_END);
  NextToken;
  ExpectSym(SMES_SColon);
  NextToken;
  DecRec;
end;

{ TPLDefaultMCall }

class function TPLDefaultMCall.Parse: TParserNode;
begin
  IncRec;
  ExpectSym(SMES_ID);
  Result := self.Create;
  Result.SetLineInfoFrom(CurrentToken);
  Result.AddOcc(CurrentToken);
  NextToken;
  Result.AddOcc(ParseExpr([SMES_SColon]));
  NextToken;
  DecRec;
end;

{ TPLPuts }

class function TPLPuts.Parse: TParserNode;
begin
  IncRec;
  ExpectSym(SMES_PUT);
  Result := self.Create;
  Result.SetLineInfoFrom(CurrentToken);
  while CurrentToken.TokenType = SMES_PUT do
    begin
      Result.AddOcc(CurrentToken);
      NextToken;
    end;
  DecRec;
end;

function TPLPuts.Compile: Boolean;
var i: Integer;
begin
  CreateAssembly;
  IncRec;
  for i := 0 to Count-1 do
    begin
      ASSERT(Occ[i].ClassType = TScanRecord);
      Assembly.InsStabLoad(Line,Column,isc_m_puts_stab,TScanRecord(Occ[i]).Pattern);
    end;
  Result := true;
  DecRec;
end;

{ TPLStat }

class function TPLStat.Parse: TParserNode;
var nodecls: TCTPLStatCls;
begin
  IncRec;
  ExpectSymSet(SymSetStatFirst);
  Result := nil;
  case CurrentToken.TokenType of
    SMES_PUT: Result := TPLPuts.Parse;
    SMES_IF: Result := TPLIfElse.Parse;
    SMES_FUNCTION: Result := TPLFunction.Parse;
    SMES_REPEAT: Result := TPLRepeat.Parse;
    SMES_WHILE: Result := TPLWhile.Parse;
    SMES_FOREACH: Result := TPLForeach.Parse;
    SMES_CLASS: Result := TPLClass.Parse;
    SMES_ID:
      begin
        {need to check for MCall or MBlock, otherwise EXPR}
        case LookupMacroNode(ucfs_to_string(CurrentToken.Pattern),nodecls) of
          tpl_none:
            begin
              Result := TPLStat.Create;
              Result.AddOcc(ParseExpr([SMES_SColon]));
              Result.SetLineInfoFrom(Result.Occ[0]);
              NextToken;
            end;
          tpl_mblock, tpl_mcall: Result := nodecls.Parse;
        end;
      end;
    else
      begin
        {may be a useless expression, but it is allowed, so -> doit}
        Result := TPLStat.Create;
        Result.AddOcc(ParseExpr([SMES_SColon]));
        Result.SetLineInfoFrom(Result.Occ[0]);
        NextToken;
      end;
  end;
  DecRec;
end;

function TPLStat.Compile: Boolean;
begin
  CreateAssembly;
  IncRec;
  ASSERT(Occ[0] is TParserNode);
  Result := TParserNode(Occ[0]).Compile;
  Assembly.AppendAssembly(TParserNode(Occ[0]).Assembly);
  Assembly.InsOperand(Occ[0].Line,Occ[0].Column,isc_m_pop_nr,1);
  DecRec;
end;

{ TPLStats }

class function TPLStats.Parse: TParserNode;
begin
  IncRec;
  Result := TPLStats.Create;
  while CurrentToken.TokenType in SymSetStatFirst do
    Result.AddOcc(TPLStat.Parse);
  DecRec;
end;

function TPLStats.Compile: Boolean;
var i: Integer;
begin
  CreateAssembly;
  IncRec;
  Result := true;
  for i := 0 to Count-1 do
    begin
      ASSERT(Occ[i] is TParserNode);
      Result := TParserNode(Occ[i]).Compile and Result;
      Assembly.AppendAssembly(TParserNode(Occ[i]).Assembly);
    end;
  DecRec;
end;

{ TTemplateFile }

class function TTemplateFile.Parse: TParserNode;
begin
  {load first token}
  NextToken;
  IncRec;
  Result := inherited Parse;
  DecRec;
  if CurrentToken.TokenType <> SMES_EOS then
    begin
      {
       probably:
         something wasn't fetched correctly (wrong expects, symbols sets,
         missing keywords), so instead of an error, the parser terminates early.
       also possible:
         root statement code outro sequence is wrong -> stats parser stops and
         leaves invalid token..
       -> prefered action, critical and not internal}
      put_critical_for(CurrentToken.Line, CurrentToken.Column, ucfs_to_string(cscan_streamid),
                       'Unexpected end of Statement');
    end;
end;

(*******************************************************************************
 Macro Register
 ******************************************************************************)

type
  PMacroPackage = ^TMacroPackage;
  TMacroPackage = record
    t: TTPLMacroType;
    c: TCTPLStatCls;
  end;

var
  MTrie: THashTrie;

function macrocall_clean( unused: PUCFS32String; data, unused2: Pointer ): Boolean;
begin
  Dispose(PMacroPackage(data));
  Result := true;
end;

procedure RegisterMacroCallNode( const id: String; nodecls: TCTPLMCallCls );
var ppack: PMacroPackage;
begin
  if not Assigned(nodecls) then
    put_internalerror(2011112910);
  ppack := New(PMacroPackage);
  ppack^.c := nodecls;
  ppack^.t := tpl_mcall;
  ppack := PMacroPackage(MTrie.Add(Upcase(id),ppack));
  if Assigned(ppack) then
    begin
      Dispose(ppack);
      put_internalerror(2011112911); // duplicate
    end;
end;

procedure RegisterMacroBlockNode( const id: String; nodecls: TCTPLMBlockCallCls );
var ppack: PMacroPackage;
begin
  if (not Assigned(nodecls)) or (Length(id) <= 0) then
    put_internalerror(2011112912);
  ppack := New(PMacroPackage);
  ppack^.c := nodecls;
  ppack^.t := tpl_mblock;
  ppack := PMacroPackage(MTrie.Add(Upcase(id),ppack));
  if Assigned(ppack) then
    begin
      Dispose(ppack);
      put_internalerror(2011112913); // duplicate
    end;
end;

function LookupMacroNode( const id: String; out node: TCTPLStatCls ): TTPLMacroType;
var ppack: PMacroPackage;
begin
  ppack := PMacroPackage(MTrie.Lookup(Upcase(id)));
  Result := tpl_none;
  if Assigned(ppack) then
    begin
      Result := ppack^.t;
      node := ppack^.c;
    end;
end;

initialization
  MTrie.Init(16);

finalization
  MTrie.ForEach(@macrocall_clean,nil);
  MTrie.Clear;

end.

