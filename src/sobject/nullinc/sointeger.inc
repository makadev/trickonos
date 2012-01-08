{   Include for sonull's Integer Type

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

(*******************************************************************************
  INTEGER
 ******************************************************************************)

type
  {integer instance}
  PSO_Integer = ^TSO_Integer;
  TSO_Integer = object(TSOInstance)
    val: VMInt;
  end;

  {Class for <Integer> Type}
  TSOTypeInteger = class(TSOTypeBase)
    protected
      class function InstanceSize: MachineInt; override;
      class procedure PostConstructor(instance: PSOInstance); override;
    public
      class function BaseTypeName: String; override;

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

      class function UnOpAbs(soself: PSOInstance): PSOInstance; override;
      class function UnOpNeg(soself: PSOInstance): PSOInstance; override;
      class function UnOpNot(soself: PSOInstance): PSOInstance; override;

      class function Compare(soself, rightop: PSOInstance): TSOCompareResult; override;
  end;

{ TSOTypeInteger }

class function TSOTypeInteger.BaseTypeName: String;
begin
  Result := C_SOTYPE_INTEGER_NAME;
end;

class function TSOTypeInteger.InstanceSize: MachineInt;
begin
  Result := SizeOf(TSO_Integer);
end;

class procedure TSOTypeInteger.PostConstructor(instance: PSOInstance);
begin
  PSO_Integer(instance)^.val := $DEAD;
end;

class function TSOTypeInteger.BinOpAdd(soself, rightop: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    begin
{$PUSH}
{$R-}
{$Q-}
      Result := so_integer_init(PSO_Integer(soself)^.val + PSO_Integer(rightop)^.val);
{$POP}
    end
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpAdd);
end;

class function TSOTypeInteger.BinOpAnd(soself, rightop: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    begin
      Result := so_integer_init(PSO_Integer(soself)^.val and PSO_Integer(rightop)^.val);
    end
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpAdd);
end;

class function TSOTypeInteger.BinOpDiv(soself, rightop: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    begin
      if so_integer_get(rightop) <> 0 then
        begin
          Result := so_integer_init(PSO_Integer(soself)^.val div PSO_Integer(rightop)^.val)
        end
      else
        Result := init_range_error(soself,rightop,1,DEFAULT_METHOD_BinOpDiv);
    end
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpDiv);
end;

class function TSOTypeInteger.BinOpMul(soself, rightop: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    begin
{$PUSH}
{$R-}
{$Q-}
      Result := so_integer_init(PSO_Integer(soself)^.val * PSO_Integer(rightop)^.val);
{$POP}
    end
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpMul);
end;

class function TSOTypeInteger.BinOpOr(soself, rightop: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    begin
      Result := so_integer_init(PSO_Integer(soself)^.val or PSO_Integer(rightop)^.val);
    end
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpOr);
end;

class function TSOTypeInteger.BinOpRol(soself, rightop: PSOInstance): PSOInstance;
var i: VMInt;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    begin
      if PSO_Integer(rightop)^.val >= 0 then
        begin
          i := PSO_Integer(rightop)^.val mod (SizeOf(VMInt)*8);
{$PUSH}
{$R-}
{$Q-}
          Result := so_integer_init(RolDWord(PSO_Integer(soself)^.val,i));
{$POP}
        end
      else
        Result := init_invargvalue_error(soself,rightop,1,DEFAULT_METHOD_BinOpRol);
    end
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpRol);
end;

class function TSOTypeInteger.BinOpRor(soself, rightop: PSOInstance): PSOInstance;
var i: VMInt;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    begin
      if PSO_Integer(rightop)^.val >= 0 then
        begin
          i := PSO_Integer(rightop)^.val mod (SizeOf(VMInt)*8);
{$PUSH}
{$R-}
{$Q-}
          Result := so_integer_init(RorDWord(PSO_Integer(soself)^.val,i));
{$POP}
        end
      else
        Result := init_invargvalue_error(soself,rightop,1,DEFAULT_METHOD_BinOpRor);
    end
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpRor);
end;

class function TSOTypeInteger.BinOpShl(soself, rightop: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    begin
      if PSO_Integer(rightop)^.val >= 0 then
        begin
          if PSO_Integer(rightop)^.val < (SizeOf(VMInt)*8) then
            Result := so_integer_init(PSO_Integer(soself)^.val shl PSO_Integer(rightop)^.val)
          else
            Result := so_integer_init(0);
        end
      else
        Result := init_invargvalue_error(soself,rightop,1,DEFAULT_METHOD_BinOpShl);
    end
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpShl);
end;

class function TSOTypeInteger.BinOpShr(soself, rightop: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    begin
      if PSO_Integer(rightop)^.val >= 0 then
        begin
          if PSO_Integer(rightop)^.val < (SizeOf(VMInt)*8) then
            begin
              Result := so_integer_init(PSO_Integer(soself)^.val shr PSO_Integer(rightop)^.val);
              if so_integer_get(soself) < 0 then
                 begin
{$PUSH}
{$R-}
{$Q-}
                   {fix non sar}
                   VMWord(PSO_Integer(Result)^.val) := VMWord(PSO_Integer(Result)^.val) or (VMWord(1) shl VMWord(31));
{$POP}
                 end;
            end
          else
            begin
              Result := so_integer_init(0);
              if so_integer_get(soself) < 0 then
                so_integer_init(-1);
            end;
        end
      else
        Result := init_invargvalue_error(soself,rightop,1,DEFAULT_METHOD_BinOpShr);
    end
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpShr);
end;

class function TSOTypeInteger.BinOpSub(soself, rightop: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    begin
{$PUSH}
{$R-}
{$Q-}
      Result := so_integer_init(PSO_Integer(soself)^.val - PSO_Integer(rightop)^.val);
{$POP}
    end
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpSub);
end;

class function TSOTypeInteger.UnOpAbs(soself: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  Result := so_integer_init(abs(PSO_Integer(soself)^.val));
end;

class function TSOTypeInteger.UnOpNeg(soself: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
{$PUSH}
{$R-}
{$Q-}
  Result := so_integer_init(-(PSO_Integer(soself)^.val));
{$POP}
end;

class function TSOTypeInteger.UnOpNot(soself: PSOInstance): PSOInstance;
begin
  Result := so_integer_init(not so_integer_get(soself));
end;

class function TSOTypeInteger.Compare(soself, rightop: PSOInstance
  ): TSOCompareResult;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    begin
      if PSO_Integer(soself)^.val = PSO_Integer(rightop)^.val then
        Result := socmp_isEqual
      else if PSO_Integer(soself)^.val < PSO_Integer(rightop)^.val then
        Result := socmp_isLess
      else
        Result := socmp_isGreater;
    end
  else
    Result := inherited Compare(soself, rightop);
end;

(*******************************************************************************
  INTEGER Methods/Attr/Helpers
 ******************************************************************************)

function so_integer_init( const i: VMInt ): PSOInstance;
begin
  Result := InitInstance(TSOTypeInteger);
  PSO_Integer(Result)^.val := i;
end;

function so_integer_get(soint: PSOInstance): VMInt;
begin
  SelfCheck(soint,TSOTypeInteger);
  Result := PSO_Integer(soint)^.val;
end;

{INT::ToString()}
function _Int_ToString_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if argnum = 0 then
    begin
      Result := so_string_init(IntToStr(PSO_Integer(soself)^.val));
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;
