{   Include for sonull's Base Type for basic Types (integer/string/boolean/...)

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

(******************************************************************************
 Base Type (Mortal, No Name/InstanceSize, does default access/error returning)
 ******************************************************************************)

type
  TSOTypeBase = class(TSOOperType)
    protected
      class function IsMortal: Boolean; override;
    public
      class function BinOpAdd(soself, rightop: PSOInstance): PSOInstance; override;
      class function BinOpAnd(soself, rightop: PSOInstance): PSOInstance; override;
      class function BinOpDiv(soself, rightop: PSOInstance): PSOInstance; override;
      class function BinOpMul(soself, rightop: PSOInstance): PSOInstance; override;
      class function BinOpOr(soself, rightop: PSOInstance): PSOInstance; override;
      class function BinOpRol(soself, rightop: PSOInstance): PSOInstance; override;
      class function BinOpRor(soself, rightop: PSOInstance): PSOInstance; override;
      class function BinOpShl(soself, rightop: PSOInstance): PSOInstance; override;
      class function BinOpShr(soself, rightop: PSOInstance): PSOInstance; override;
      class function BinOpSub(soself, rightop: PSOInstance): PSOInstance; override;
      class function DirectCall(soself: PSOInstance; args: PSOMethodVarArgs; argnum: VMInt): PSOInstance; override;
      class function GetIndex(soself, idx: PSOInstance): PSOInstance; override;
      class function GetMember(soself: PSOInstance; const name: String): PSOInstance; override;
      class function MethodCall(soself: PSOInstance; const name: String; args: PSOMethodVarArgs; argnum: VMInt): PSOInstance; override;
      class function SetIndex(soself, idx, value: PSOInstance): PSOInstance; override;
      class function SetMember(soself, value: PSOInstance; const name: String): PSOInstance; override;
      class function TypeQuery(soself: PSOInstance): String; override;
      class function UnOpAbs(soself: PSOInstance): PSOInstance; override;
      class function UnOpNeg(soself: PSOInstance): PSOInstance; override;
      class function UnOpNot(soself: PSOInstance): PSOInstance; override;
  end;

{ TSOTypeBase }

class function TSOTypeBase.BinOpAdd(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := init_operation_error(soself,DEFAULT_METHOD_BinOpAdd+'(<'+
    rightop^.GetTypeCls.TypeQuery(rightop)+'>)');
end;

class function TSOTypeBase.BinOpAnd(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := init_operation_error(soself,DEFAULT_METHOD_BinOpAnd+'(<'+
    rightop^.GetTypeCls.TypeQuery(rightop)+'>)');
end;

class function TSOTypeBase.BinOpDiv(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := init_operation_error(soself,DEFAULT_METHOD_BinOpDiv+'(<'+
    rightop^.GetTypeCls.TypeQuery(rightop)+'>)');
end;

class function TSOTypeBase.BinOpMul(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := init_operation_error(soself,DEFAULT_METHOD_BinOpMul+'(<'+
    rightop^.GetTypeCls.TypeQuery(rightop)+'>)');
end;

class function TSOTypeBase.BinOpOr(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := init_operation_error(soself,DEFAULT_METHOD_BinOpOr+'(<'+
    rightop^.GetTypeCls.TypeQuery(rightop)+'>)');
end;

class function TSOTypeBase.BinOpRol(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := init_operation_error(soself,DEFAULT_METHOD_BinOpRol+'(<'+
    rightop^.GetTypeCls.TypeQuery(rightop)+'>)');
end;

class function TSOTypeBase.BinOpRor(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := init_operation_error(soself,DEFAULT_METHOD_BinOpRor+'^(<'+
    rightop^.GetTypeCls.TypeQuery(rightop)+'>)');
end;

class function TSOTypeBase.BinOpShl(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := init_operation_error(soself,DEFAULT_METHOD_BinOpShl+'(<'+
    rightop^.GetTypeCls.TypeQuery(rightop)+'>)');
end;

class function TSOTypeBase.BinOpShr(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := init_operation_error(soself,DEFAULT_METHOD_BinOpShr+'(<'+
    rightop^.GetTypeCls.TypeQuery(rightop)+'>)');
end;

class function TSOTypeBase.BinOpSub(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := init_operation_error(soself,DEFAULT_METHOD_BinOpSub+'(<'+
    rightop^.GetTypeCls.TypeQuery(rightop)+'>)');
end;

class function TSOTypeBase.DirectCall(soself: PSOInstance; args: PSOMethodVarArgs;
  argnum: VMInt): PSOInstance;
begin
  Result := init_operation_error(soself,DEFAULT_METHOD_DirectCall+'(<'+IntToStr(argnum)+' Arguments>)');
end;

class function TSOTypeBase.IsMortal: Boolean;
begin
  Result := true;
end;

class function TSOTypeBase.GetIndex(soself, idx: PSOInstance): PSOInstance;
begin
  Result := init_operation_error(soself,DEFAULT_METHOD_GetIndex+'(<'+
    idx^.GetTypeCls.TypeQuery(idx)+'>)');
end;

class function TSOTypeBase.GetMember(soself: PSOInstance; const name: String
  ): PSOInstance;
var a: TSOAttributHandler;
begin
  SelfCheck(soself,self);
  a := GetInternalAttribute(name,BaseTypeName);
  if Assigned(a) then
    Result := a(name,soself,nil)
  else
    Result := init_operation_error(soself,DEFAULT_METHOD_GetMember+'('+name+')');
end;

class function TSOTypeBase.MethodCall(soself: PSOInstance; const name: String;
  args: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
var h: TSOMethodHandler;
begin
  SelfCheck(soself,self);
  h := GetInternalMethod(name,BaseTypeName);
  if Assigned(h) then
    begin
      check_so_maxargs(argnum);
      Result := h(name,soself,args,argnum);
    end
  else
    Result := init_operation_error(soself,name+'(..<'+IntToStr(argnum)+' Arguments>..)');
end;

class function TSOTypeBase.SetIndex(soself, idx, value: PSOInstance): PSOInstance;
begin
  Result := init_operation_error(soself,DEFAULT_METHOD_SetIndex+'(<'+
                                        idx^.GetTypeCls.TypeQuery(idx)+
                                        '>,<'+value^.GetTypeCls.TypeQuery(value)+'>)');
end;

class function TSOTypeBase.SetMember(soself, value: PSOInstance; const name: String
  ): PSOInstance;
var a: TSOAttributHandler;
begin
  SelfCheck(soself,self);
  a := GetInternalAttribute(name,BaseTypeName);
  if Assigned(a) then
    Result := a(name,soself,value)
  else
    Result := init_operation_error(soself,DEFAULT_METHOD_SetMember+'('+name+
                                          ',<'+value^.GetTypeCls.TypeQuery(value)+'>)');
end;

class function TSOTypeBase.TypeQuery(soself: PSOInstance): String;
begin
  Result := BaseTypeName;
end;

class function TSOTypeBase.UnOpAbs(soself: PSOInstance): PSOInstance;
begin
  Result := init_operation_error(soself,DEFAULT_METHOD_UnOpAbs);
end;

class function TSOTypeBase.UnOpNeg(soself: PSOInstance): PSOInstance;
begin
  Result := init_operation_error(soself,DEFAULT_METHOD_UnOpNeg);
end;

class function TSOTypeBase.UnOpNot(soself: PSOInstance): PSOInstance;
begin
  Result := init_operation_error(soself,DEFAULT_METHOD_UnOpNot);
end;