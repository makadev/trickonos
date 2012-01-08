{   Unit for priority expression parsing

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

unit ccexprp;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, eomsg, ccbase, cscan, csyms, ccexprn;

{ Generic Expr:
   Expr ::= PRIOM4 // ( := PRIOM4 ) .
   PRIOM4 ::= PRIOM3 // ( <relop> PRIOM3 ).
   PRIOM3 ::= PRIOM2 // ( <addop> PRIOM2 ).
   PRIOM2 ::= PRIOM1 // ( <mulop> PRIOM1 ).
   PRIOM1 ::= PRIOS | <unary> PRIOM1.
   PRIOS ::= VALUE ['(','[','.'] ...
   VALUE ::= <decidable>
}

const
  VALUE_FIRST = [ SMES_NIL, SMES_TRUE, SMES_FALSE,
                  SMES_ID,
                  SMES_Int, SMES_IntBin, SMES_IntHex, SMES_IntOct, SMES_String,
                  SMES_OSquare, SMES_ORound, SMES_OCurly ];

  SEL_OPS = [ SMES_ORound, SMES_OSquare, SMES_Dot ];

  UNARY_FIRST = [ SMES_Plus, SMES_Minus, SMES_NOT ];

  RELOP_CLS = [ SMES_OAngel, SMES_CAngel, SMES_LE, SMES_GE, SMES_EQ, SMES_NEQ, SMES_IS ];
  ADDOP_CLS = [ SMES_Plus, SMES_Minus, SMES_OR ];
  MULOP_CLS = [ SMES_Star, {SMES_Slash,} SMES_DIV,
    SMES_SHL, SMES_SHR, SMES_ROL, SMES_ROR,
    SMES_SHLK, SMES_SHRK, SMES_ROLK, SMES_RORK,
    SMES_AND ];


  EXPR_FIRST = VALUE_FIRST + UNARY_FIRST;

type
  {Pseudo/NT Nodes}
  TExpr = class(TParserNode)
    public
      class function Parse: TParserNode; override;
  end;

  TExprPrioM4 = class(TParserNode)
    public
      class function Parse: TParserNode; override;
  end;

  TExprPrioM3 = class(TParserNode)
    public
      class function Parse: TParserNode; override;
  end;

  TExprPrioM2 = class(TParserNode)
    public
      class function Parse: TParserNode; override;
  end;

  TExprPrioM1 = class(TParserNode)
    public
      class function Parse: TParserNode; override;
  end;

  TExprPrioS = class(TParserNode)
    public
      class function Parse: TParserNode; override;
  end;

  TExprValue = class(TParserNode)
    public
      class function Parse: TParserNode; override;
  end;

function ParseExpr( follow: TSMESSymSet ): TParserNode;

implementation

function ParseExpr(follow: TSMESSymSet): TParserNode;
begin
  ExpectSymSet(EXPR_FIRST);
  Result := TExpr.Parse;
  ExpectSymSet(follow);
end;

{ TExprValue }

class function TExprValue.Parse: TParserNode;
begin
  IncRec;
  ExpectSymSet(VALUE_FIRST);
  case CurrentToken.TokenType of
    SMES_ID: Result := TExprId.Parse;
    SMES_NIL,
    SMES_TRUE,
    SMES_FALSE,
    SMES_Int,
    SMES_IntBin,
    SMES_IntHex,
    SMES_IntOct,
    SMES_String: Result := TExprConst.Parse;
    SMES_OSquare: Result := TExprList.Parse;
    SMES_OCurly: Result := TExprDict.Parse;
    SMES_ORound:
      begin
        NextToken;
        Result := TExpr.Parse;
        ExpectSym(SMES_CRound);
        NextToken;
      end
    else
      put_internalerror(2011112950);
  end;
  DecRec;
end;

{ TExprPrioS }

class function TExprPrioS.Parse: TParserNode;
var tmp: TParserNode;
begin
  IncRec;
  Result := TExprValue.Parse;
  if CurrentToken.TokenType in SEL_OPS then
    begin
      repeat
        case CurrentToken.TokenType of
          SMES_ORound:
            begin
              {expr // (list of expr)}
              tmp := TExprCall.Create;
              tmp.AddOcc(Result);
              tmp.SetLineInfoFrom(CurrentToken);
              NextToken;
              if CurrentToken.TokenType <> SMES_CRound then
                begin
                  repeat
                    ExpectSymSet(EXPR_FIRST);
                    tmp.AddOcc(TExpr.Parse);
                    ExpectSymSet([SMES_CRound,SMES_Comma]);
                    if CurrentToken.TokenType = SMES_Comma then
                      begin
                        NextToken;
                        continue;
                      end;
                  until CurrentToken.TokenType = SMES_CRound;
                end;
              NextToken;
            end;
          SMES_OSquare:
            begin
              {expr // [list of expr] -> rewritten into binary}
              repeat
                tmp := TExprIndex.Create;
                tmp.SetLineInfoFrom(CurrentToken);
                NextToken;
                ExpectSymSet(EXPR_FIRST);
                tmp.AddOcc(Result);
                tmp.AddOcc(TExpr.Parse);
                Result := tmp;
                ExpectSymSet([SMES_CSquare,SMES_Comma]);
              until CurrentToken.TokenType = SMES_CSquare;
              NextToken;
            end;
          SMES_Dot:
            begin
              {expr // .name}
              tmp := TExprSelect.Create;
              tmp.AddOcc(Result);
              tmp.SetLineInfoFrom(CurrentToken);
              NextToken;
              ExpectSym(SMES_ID);
              tmp.AddOcc(CurrentToken);
              NextToken;
            end;
          else
            put_internalerror(2011112930);
        end;
        Result := tmp;
      until not (CurrentToken.TokenType in SEL_OPS);
    end;
  DecRec;
end;

{ TExprPrioM4 }

class function TExprPrioM4.Parse: TParserNode;
var tmp: TParserNode;
begin
  IncRec;
  Result := TExprPrioM3.Parse;
  while CurrentToken.TokenType in RELOP_CLS do
    begin
      if CurrentToken.TokenType <> SMES_IS then
        begin
          tmp := TExprBinary.Create;
          tmp.AddOcc(CurrentToken);
          tmp.SetLineInfoFrom(CurrentToken);
          tmp.AddOcc(Result);
          Result := tmp;
          NextToken;
          Result.AddOcc(TExprPrioM3.Parse);
        end
      else
        begin
          {expr // is name}
          tmp := TExprIs.Create;
          tmp.AddOcc(Result);
          tmp.SetLineInfoFrom(CurrentToken);
          Result := tmp;
          NextToken;
          ExpectSym(SMES_ID);
          Result.AddOcc(CurrentToken);
          NextToken;
        end;
    end;
  DecRec;
end;

{ TExprPrioM3 }

class function TExprPrioM3.Parse: TParserNode;
var tmp: TParserNode;
begin
  IncRec;
  Result := TExprPrioM2.Parse;
  while CurrentToken.TokenType in ADDOP_CLS do
    begin
      tmp := TExprBinary.Create;
      tmp.AddOcc(CurrentToken);
      tmp.SetLineInfoFrom(CurrentToken);
      tmp.AddOcc(Result);
      Result := tmp;
      NextToken;
      Result.AddOcc(TExprPrioM2.Parse);
    end;
  DecRec;
end;

{ TExprPrioM2 }

class function TExprPrioM2.Parse: TParserNode;
var tmp: TParserNode;
begin
  IncRec;
  Result := TExprPrioM1.Parse;
  while CurrentToken.TokenType in MULOP_CLS do
    begin
      tmp := TExprBinary.Create;
      tmp.AddOcc(CurrentToken);
      tmp.SetLineInfoFrom(CurrentToken);
      tmp.AddOcc(Result);
      Result := tmp;
      NextToken;
      Result.AddOcc(TExprPrioM1.Parse);
    end;
  DecRec;
end;

{ TExprPrioM1 }

class function TExprPrioM1.Parse: TParserNode;
begin
  IncRec;
  if CurrentToken.TokenType in UNARY_FIRST then
    begin
      Result := TExprUnary.Create;
      Result.AddOcc(CurrentToken);
      Result.SetLineInfoFrom(CurrentToken);
      NextToken;
      Result.AddOcc(TExprPrioM1.Parse);
    end
  else
    Result := TExprPrioS.Parse;
  DecRec;
end;

{ TExpr }

class function TExpr.Parse: TParserNode;
var tmp: TParserNode;
begin
  IncRec;
  ExpectSymSet(EXPR_FIRST);
  Result := TExprPrioM4.Parse;
  if CurrentToken.TokenType = SMES_Assign then
    begin
      tmp := TExprAssign.Create;
      tmp.SetLineInfoFrom(CurrentToken);
      CurrentToken.Free;
      tmp.AddOcc(Result);
      Result := tmp;
      NextToken;
      Result.AddOcc(TExpr.Parse);
    end;
  DecRec;
end;

end.

