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
    fnum: PTFM_Integer;
  end;

  {Class for <Integer> Type}
  TSOTypeInteger = class(TSOTypeBase)
    protected
      class function InstanceSize: MachineInt; override;
      class procedure PostConstructor(instance: PSOInstance); override;
      class procedure PreDestructor(instance: PSOInstance); override;
    public
      class function BaseTypeName: String; override;

      class function BinOpAdd(soself, rightop: PSOInstance): PSOInstance; override;
      class function BinOpAnd(soself, rightop: PSOInstance): PSOInstance; override;
      class function BinOpDiv(soself, rightop: PSOInstance): PSOInstance; override;
      class function BinOpMod(soself, rightop: PSOInstance): PSOInstance; override;
      class function BinOpMul(soself, rightop: PSOInstance): PSOInstance; override;
      class function BinOpOr(soself, rightop: PSOInstance): PSOInstance; override;
      class function BinOpXor(soself, rightop: PSOInstance): PSOInstance; override;
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
  PSO_Integer(instance)^.fnum := nil;
end;

class procedure TSOTypeInteger.PreDestructor(instance: PSOInstance);
begin
  tfm_release(PSO_Integer(instance)^.fnum);
  PSO_Integer(instance)^.fnum := nil;
end;

class function TSOTypeInteger.BinOpAdd(soself, rightop: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    Result := so_integer_init_tfm(tfm_add(PSO_Integer(soself)^.fnum, PSO_Integer(rightop)^.fnum))
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpAdd);
end;

class function TSOTypeInteger.BinOpAnd(soself, rightop: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    Result := so_integer_init_tfm(tfm_and(PSO_Integer(soself)^.fnum, PSO_Integer(rightop)^.fnum))
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpAdd);
end;

class function TSOTypeInteger.BinOpDiv(soself, rightop: PSOInstance): PSOInstance;
var i: PTFM_Integer;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    begin
      i := tfm_div(PSO_Integer(soself)^.fnum, PSO_Integer(rightop)^.fnum);
      if Assigned(i) then
        Result := so_integer_init_tfm(i)
      else
        Result := init_range_error(soself,rightop,1,DEFAULT_METHOD_BinOpDiv);
    end
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpDiv);
end;

class function TSOTypeInteger.BinOpMod(soself, rightop: PSOInstance
  ): PSOInstance;
var i: PTFM_Integer;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    begin
      i := tfm_mod(PSO_Integer(soself)^.fnum, PSO_Integer(rightop)^.fnum);
      if Assigned(i) then
        Result := so_integer_init_tfm(i)
      else
        Result := init_range_error(soself,rightop,1,DEFAULT_METHOD_BinOpMod);
    end
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpMod);
end;

class function TSOTypeInteger.BinOpMul(soself, rightop: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    Result := so_integer_init_tfm(tfm_mul(PSO_Integer(soself)^.fnum, PSO_Integer(rightop)^.fnum))
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpMul);
end;

class function TSOTypeInteger.BinOpOr(soself, rightop: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    Result := so_integer_init_tfm(tfm_or(PSO_Integer(soself)^.fnum, PSO_Integer(rightop)^.fnum))
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpOr);
end;

class function TSOTypeInteger.BinOpXor(soself, rightop: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    Result := so_integer_init_tfm(tfm_xor(PSO_Integer(soself)^.fnum, PSO_Integer(rightop)^.fnum))
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpXor);
end;

class function TSOTypeInteger.BinOpRol(soself, rightop: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    begin
      if so_integer_fits(rightop) and
         (not PSO_Integer(rightop)^.fnum^.is_signed) then
        Result := so_integer_init_tfm(tfm_rol(PSO_Integer(soself)^.fnum,so_integer_get(rightop,true)))
      else
        Result := init_invargvalue_error(soself,rightop,1,DEFAULT_METHOD_BinOpRol);
    end
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpRol);
end;

class function TSOTypeInteger.BinOpRor(soself, rightop: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    begin
      if so_integer_fits(rightop) and
         (not PSO_Integer(rightop)^.fnum^.is_signed) then
        Result := so_integer_init_tfm(tfm_ror(PSO_Integer(soself)^.fnum,so_integer_get(rightop,true)))
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
      if so_integer_fits(rightop) and
         (not PSO_Integer(rightop)^.fnum^.is_signed) then
        Result := so_integer_init_tfm(tfm_shl(PSO_Integer(soself)^.fnum,so_integer_get(rightop,true)))
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
      if so_integer_fits(rightop) and
         (not PSO_Integer(rightop)^.fnum^.is_signed) then
        Result := so_integer_init_tfm(tfm_sar(PSO_Integer(soself)^.fnum,so_integer_get(rightop,true)))
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
    Result := so_integer_init_tfm(tfm_sub(PSO_Integer(soself)^.fnum, PSO_Integer(rightop)^.fnum))
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpSub);
end;

class function TSOTypeInteger.UnOpAbs(soself: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  Result := so_integer_init_tfm(tfm_abs(PSO_Integer(soself)^.fnum))
end;

class function TSOTypeInteger.UnOpNeg(soself: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  Result := so_integer_init_tfm(tfm_neg(PSO_Integer(soself)^.fnum))
end;

class function TSOTypeInteger.UnOpNot(soself: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_integer_class);
  Result := so_integer_init_tfm(tfm_not(PSO_Integer(soself)^.fnum));
end;

class function TSOTypeInteger.Compare(soself, rightop: PSOInstance
  ): TSOCompareResult;
var ncmp: VMInt;
begin
  SelfCheck(soself,so_integer_class);
  if rightop^.IsType(so_integer_class) then
    begin
      ncmp := PSO_Integer(soself)^.fnum^.compare(PSO_Integer(rightop)^.fnum);
      if ncmp = 0 then
        Result := socmp_isEqual
      else if ncmp < 0 then
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
  PSO_Integer(Result)^.fnum := tfm_load_int(i,SizeOf(VMInt)*8);
end;

function so_integer_init_tfm( num: PTFM_Integer ): PSOInstance;
begin
  ASSERT(num <> nil);
  Result := InitInstance(TSOTypeInteger);
  PSO_Integer(Result)^.fnum := num;
end;

function so_integer_fits( soint: PSOInstance ): Boolean;
begin
  SelfCheck(soint,TSOTypeInteger);
  Result := PSO_Integer(soint)^.fnum^.is_wordsize;
  if Result then
    begin
      PSO_Integer(soint)^.fnum^.expand;
      Result := (MachineInt(PSO_Integer(soint)^.fnum^.logic_expanded_word(0)) >= Low(VMInt)) and
                (MachineInt(PSO_Integer(soint)^.fnum^.logic_expanded_word(0)) <= High(VMInt));
    end;
end;

function so_integer_string( soint: PSOInstance ): String;
begin
  SelfCheck(soint,TSOTypeInteger);
  Result := tfm_to_string(PSO_Integer(soint)^.fnum);
end;

function so_integer_get( soint: PSOInstance; usecut: Boolean ): VMInt;
begin
  SelfCheck(soint,TSOTypeInteger);
  if usecut or
     so_integer_fits(soint) then
    begin
      PSO_Integer(soint)^.fnum^.expand;
      Result := VMInt(MachineInt(PSO_Integer(soint)^.fnum^.logic_expanded_word(0)));
    end
  else
    put_internalerror($12011900); // number too big
end;

{INT::ToString()}
function _Int_ToString_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if argnum = 0 then
    begin
      Result := so_string_init(tfm_to_string(PSO_Integer(soself)^.fnum));
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;
