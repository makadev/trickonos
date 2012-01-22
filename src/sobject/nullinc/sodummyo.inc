{   Include for sonull's Dummy Type

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
 Immortal Dummy Object, for Debugging (and fun :<)
  "it knows" every method and returns itself while printing debug infos
 ******************************************************************************)

type
  TSO_DummyObject = TSOInstance;

function socls_DummyObject_MethodCall(const name: String; soself: PSOInstance; args: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
var i: VMInt;
begin
  SelfCheck(soself,so_dummy_class);
  put_info(C_SOTYPE_DUMMY_NAME+' method '+name+' called with '+IntToStr(argnum)+' Args.');
  if debug_mode then
    for i := 0 to argnum-1 do
      put_debug('  ARG '+IntToStr(i+1)+' is '+args^[0]^.GetTypeCls^.TypeQuery(args^[0]));
  Result := soself;
end;

function socls_DummyObject_TypeQuery(soself: PSOInstance): String;
begin
  SelfCheck(soself,so_dummy_class);
  Result := C_SOTYPE_DUMMY_NAME;
  put_debug(C_SOTYPE_DUMMY_NAME+' got (Type)checked :o');
end;

function socls_DummyObject_Compare(soself, rightop: PSOInstance): TSOCompareResult;
begin
  SelfCheck(soself,so_dummy_class);
  put_debug(C_SOTYPE_DUMMY_NAME+' getting Compared against '+rightop^.GetTypeCls^.TypeQuery(rightop));
  Result := DefaultCompare(soself, rightop);
end;

function socls_DummyObject_MethodCallOverride(const name: String; soself: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_dummy_class);
  put_debug(C_SOTYPE_DUMMY_NAME+' getting Checked for MethodOverride '+name);
  Result := nil;
end;
