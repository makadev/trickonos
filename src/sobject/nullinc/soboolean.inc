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

type
  {Class for <Boolean> Type}
  TSOTypeBoolean = class(TSOTypeBase)
    protected
      class function InstanceSize: MachineInt; override;
      class function IsMortal: Boolean; override;
    public
      class function BaseTypeName: String; override;
      class function BinOpAnd(soself, rightop: PSOInstance): PSOInstance; override;
      class function BinOpOr(soself, rightop: PSOInstance): PSOInstance; override;
      class function UnOpNot(soself: PSOInstance): PSOInstance; override;
  end;


{ TSOTypeBoolean }

class function TSOTypeBoolean.InstanceSize: MachineInt;
begin
  Result := SizeOf(TSOInstance);
end;

class function TSOTypeBoolean.IsMortal: Boolean;
begin
  Result := false;
end;

class function TSOTypeBoolean.BaseTypeName: String;
begin
  Result := C_SOTYPE_BOOLEAN_NAME;
end;

class function TSOTypeBoolean.BinOpAnd(soself, rightop: PSOInstance): PSOInstance;
{if true, return other, else return false}
begin
  SelfCheck(soself,so_boolean_class);
  if soself = so_true then
    Result := rightop
  else
    Result := soself;
  // rightop may not be bool
  Result^.IncRef;
end;

class function TSOTypeBoolean.BinOpOr(soself, rightop: PSOInstance): PSOInstance;
{if false return other, else return self}
begin
  SelfCheck(soself,so_boolean_class);
  if soself = so_true then
    Result := soself
  else
    Result := rightop;
  // rightop may not be bool
  Result^.IncRef;
end;

class function TSOTypeBoolean.UnOpNot(soself: PSOInstance): PSOInstance;
{Return true/false}
begin
  SelfCheck(soself,so_boolean_class);
  if soself = so_true then
    Result := so_false
  else
    Result := so_true;
end;

