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

var
  TMethodTrie_Integer: THashTrie;

procedure so_integer_addmethod( const mname: String; methh: TSOMethodHandler );
begin
  if TMethodTrie_Integer.Add(UpCase(mname),methh) <> nil then
    put_internalerror(12012203);
end;

type
  {integer instance}
  PSO_Integer = ^TSO_Integer;
  TSO_Integer = object(TSOInstance)
    fnum: PTFM_Integer;
  end;

function socls_Integer_TypeQuery( soself: PSOInstance ): String;
begin
  SelfCheck(soself,so_integer_class);
  Result := C_SOTYPE_INTEGER_NAME;
end;

procedure socls_Integer_PostConstructor(instance: PSOInstance);
begin
  PSO_Integer(instance)^.fnum := nil;
end;

procedure socls_Integer_PreDestructor(instance: PSOInstance);
begin
  tfm_release(PSO_Integer(instance)^.fnum);
  PSO_Integer(instance)^.fnum := nil;
end;

function socls_Integer_MethodCall( callinfo: PMethodCallInfo ): PSOInstance;
{generic, lookup method and call}
var m: TSOMethodHandler;
begin
  with callinfo^ do
    begin
      m := TSOMethodHandler( TMethodTrie_Integer.LookupByHash(hashk,name) );
      if Assigned(m) then
        Result := m( callinfo )
      else
        Result := nil;
    end;
end;

function socls_Integer_Compare(soself, rightop: PSOInstance): TSOCompareResult;
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
    Result := DefaultCompare(soself, rightop);
end;

(*******************************************************************************
  INTEGER Methods/Attr/Helpers
 ******************************************************************************)

{Integer::Add(integer) -> integer}
function _Integer_Add_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      SelfCheck(soself,so_integer_class);
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_integer_class) then
            Result := so_integer_init_tfm(tfm_add(PSO_Integer(soself)^.fnum, PSO_Integer(soargs^[0])^.fnum))
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{Integer::And(integer) -> integer}
function _Integer_And_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      SelfCheck(soself,so_integer_class);
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_integer_class) then
            Result := so_integer_init_tfm(tfm_and(PSO_Integer(soself)^.fnum, PSO_Integer(soargs^[0])^.fnum))
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{Integer::Div(integer) -> integer}
function _Integer_Div_(callinfo: PMethodCallInfo): PSOInstance;
var i: PTFM_Integer;
begin
  with callinfo^ do
    begin
      SelfCheck(soself,so_integer_class);
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_integer_class) then
            begin
              i := tfm_div(PSO_Integer(soself)^.fnum, PSO_Integer(soargs^[0])^.fnum);
              if Assigned(i) then
                Result := so_integer_init_tfm(i)
              else
                Result := init_range_error(soself,soargs^[0],1,name);
            end
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{Integer::Mod(integer) -> integer}
function _Integer_Mod_(callinfo: PMethodCallInfo): PSOInstance;
var i: PTFM_Integer;
begin
  with callinfo^ do
    begin
      SelfCheck(soself,so_integer_class);
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_integer_class) then
            begin
              i := tfm_mod(PSO_Integer(soself)^.fnum, PSO_Integer(soargs^[0])^.fnum);
              if Assigned(i) then
                Result := so_integer_init_tfm(i)
              else
                Result := init_range_error(soself,soargs^[0],1,name);
            end
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{Integer::Mul(integer) -> integer}
function _Integer_Mul_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      SelfCheck(soself,so_integer_class);
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_integer_class) then
            Result := so_integer_init_tfm(tfm_mul(PSO_Integer(soself)^.fnum, PSO_Integer(soargs^[0])^.fnum))
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{Integer::Or(integer) -> integer}
function _Integer_Or_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      SelfCheck(soself,so_integer_class);
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_integer_class) then
            Result := so_integer_init_tfm(tfm_or(PSO_Integer(soself)^.fnum, PSO_Integer(soargs^[0])^.fnum))
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{Integer::Xor(integer) -> integer}
function _Integer_Xor_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      SelfCheck(soself,so_integer_class);
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_integer_class) then
            Result := so_integer_init_tfm(tfm_xor(PSO_Integer(soself)^.fnum, PSO_Integer(soargs^[0])^.fnum))
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{Integer::Rol(integer) -> integer}
function _Integer_Rol_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      SelfCheck(soself,so_integer_class);
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_integer_class) then
            begin
              if so_integer_fits(soargs^[0]) and
                 (not PSO_Integer(soargs^[0])^.fnum^.is_signed) then
                Result := so_integer_init_tfm(tfm_rol(PSO_Integer(soself)^.fnum,so_integer_get(soargs^[0],true)))
              else
                Result := init_invargvalue_error(soself,soargs^[0],1,name);
            end
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{Integer::Ror(integer) -> integer}
function _Integer_Ror_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      SelfCheck(soself,so_integer_class);
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_integer_class) then
            begin
              if so_integer_fits(soargs^[0]) and
                 (not PSO_Integer(soargs^[0])^.fnum^.is_signed) then
                Result := so_integer_init_tfm(tfm_ror(PSO_Integer(soself)^.fnum,so_integer_get(soargs^[0],true)))
              else
                Result := init_invargvalue_error(soself,soargs^[0],1,name);
            end
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{Integer::Shl(integer) -> integer}
function _Integer_Shl_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      SelfCheck(soself,so_integer_class);
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_integer_class) then
            begin
              if so_integer_fits(soargs^[0]) and
                 (not PSO_Integer(soargs^[0])^.fnum^.is_signed) then
                Result := so_integer_init_tfm(tfm_shl(PSO_Integer(soself)^.fnum,so_integer_get(soargs^[0],true)))
              else
                Result := init_invargvalue_error(soself,soargs^[0],1,name);
            end
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{Integer::Shr(integer) -> integer}
function _Integer_Shr_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      SelfCheck(soself,so_integer_class);
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_integer_class) then
            begin
              if so_integer_fits(soargs^[0]) and
                 (not PSO_Integer(soargs^[0])^.fnum^.is_signed) then
                Result := so_integer_init_tfm(tfm_shr(PSO_Integer(soself)^.fnum,so_integer_get(soargs^[0],true)))
              else
                Result := init_invargvalue_error(soself,soargs^[0],1,name);
            end
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{Integer::Sar(integer) -> integer}
function _Integer_Sar_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      SelfCheck(soself,so_integer_class);
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_integer_class) then
            begin
              if so_integer_fits(soargs^[0]) and
                 (not PSO_Integer(soargs^[0])^.fnum^.is_signed) then
                Result := so_integer_init_tfm(tfm_sar(PSO_Integer(soself)^.fnum,so_integer_get(soargs^[0],true)))
              else
                Result := init_invargvalue_error(soself,soargs^[0],1,name);
            end
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{Integer::Sub(integer) -> integer}
function _Integer_Sub_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      SelfCheck(soself,so_integer_class);
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_integer_class) then
            Result := so_integer_init_tfm(tfm_sub(PSO_Integer(soself)^.fnum, PSO_Integer(soargs^[0])^.fnum))
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{Integer::Abs() -> integer}
function _Integer_Abs_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      SelfCheck(soself,so_integer_class);
      if argnum = 0 then
        Result := so_integer_init_tfm(tfm_abs(PSO_Integer(soself)^.fnum))
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{Integer::Not() -> integer}
function _Integer_Not_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      SelfCheck(soself,so_integer_class);
      if argnum = 0 then
        Result := so_integer_init_tfm(tfm_not(PSO_Integer(soself)^.fnum))
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{Integer::Neg() -> integer}
function _Integer_Neg_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      SelfCheck(soself,so_integer_class);
      if argnum = 0 then
        Result := so_integer_init_tfm(tfm_neg(PSO_Integer(soself)^.fnum))
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

function so_integer_init( const i: VMInt ): PSOInstance;
{init integer of size VMInt and assign value}
begin
  Result := InitInstance(so_integer_class);
  PSO_Integer(Result)^.fnum := tfm_load_int(i,SizeOf(VMInt)*8);
end;

function so_integer_init_tfm( num: PTFM_Integer ): PSOInstance;
{init integer and assign PTFM_Integer}
begin
  ASSERT(num <> nil);
  Result := InitInstance(so_integer_class);
  PSO_Integer(Result)^.fnum := num;
end;

function so_integer_fits( soint: PSOInstance ): Boolean;
{check if integer fits into VMInt}
begin
  SelfCheck(soint,so_integer_class);
  Result := PSO_Integer(soint)^.fnum^.is_wordsize;
  if Result then
    begin
      PSO_Integer(soint)^.fnum^.expand;
      Result := (MachineInt(PSO_Integer(soint)^.fnum^.logic_expanded_word(0)) >= Low(VMInt)) and
                (MachineInt(PSO_Integer(soint)^.fnum^.logic_expanded_word(0)) <= High(VMInt));
    end;
end;

function so_integer_string( soint: PSOInstance ): String;
{convert integer to string}
begin
  SelfCheck(soint,so_integer_class);
  Result := tfm_to_string(PSO_Integer(soint)^.fnum);
end;

function so_integer_get( soint: PSOInstance; usecut: Boolean ): VMInt;
{read Integer as VMInt, either test using fit test or simply casts the
 first word to VMInt (usecut)
 produces internalerror if integer doesn't fit and usecut=false}
begin
  SelfCheck(soint,so_integer_class);
  if usecut or
     so_integer_fits(soint) then
    begin
      PSO_Integer(soint)^.fnum^.expand;
      Result := VMInt(MachineInt(PSO_Integer(soint)^.fnum^.logic_expanded_word(0)));
    end
  else
    put_internalerror($12011900); // number too big
end;

{Integer::ToStr()}
function _Integer_ToStr_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      if argnum = 0 then
        begin
          Result := so_string_init_a7(tfm_to_string(PSO_Integer(soself)^.fnum));
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;
