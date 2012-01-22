{   Include for sonull's Boolean Type

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
 BOOLEAN (Immortal, 2 Instances)
 ******************************************************************************)

var
  TMethodTrie_Boolean: THashTrie;

procedure so_boolean_addmethod( const mname: String; methh: TSOMethodHandlerPure );
begin
  if TMethodTrie_Boolean.Add(UpCase(mname),methh) <> nil then
    put_internalerror(12012201);
end;

type
  TSO_Boolean = TSOInstance;


function socls_Boolean_TypeQuery( soself: PSOInstance ): String;
begin
  Result := C_SOTYPE_BOOLEAN_NAME;
end;

function socls_Boolean_MethodCall( hashk: MachineWord; const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
{generic, lookup method and call}
var m: TSOMethodHandlerPure;
begin
  m := TSOMethodHandlerPure( TMethodTrie_Boolean.LookupByHash(hashk,mname) );
  if Assigned(m) then
    Result := m( mname, soself, soargs, argnum )
  else
    Result := nil;
end;

function _Boolean_And_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
{if true, return other, else return false}
begin
  SelfCheck(soself,so_boolean_class);
  if argnum = 1 then
    begin
      if soself = so_true then
        Result := soargs^[0]
      else
        Result := soself;
      // rightop may not be bool
      Result^.IncRef;
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;

function _Boolean_Or_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
{if false return other, else return self}
begin
  SelfCheck(soself,so_boolean_class);
  if argnum = 1 then
    begin
      if soself = so_true then
        Result := soself
      else
        Result := soargs^[0];
      // rightop may not be bool
      Result^.IncRef;
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;

function _Boolean_Xor_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
{xor is not part of the object logic, simply operates on boolean
  -> x.xor(x) = false, x.xor(not x) = true
  -> x.xor(object) -> fail}
begin
  SelfCheck(soself,so_boolean_class);
  if argnum = 1 then
    begin
      if soargs^[0]^.IsType(so_boolean_class) then
        begin
          if soself = soargs^[0] then
            Result := so_false
          else
            Result := so_true;
        end
      else
        Result := init_invargtype_error(soself,soargs^[0],1,mname);
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;

function _Boolean_Not_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
{Return true/false}
begin
  SelfCheck(soself,so_boolean_class);
  if argnum = 0 then
    begin
      if soself = so_true then
        Result := so_false
      else
        Result := so_true;
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;

