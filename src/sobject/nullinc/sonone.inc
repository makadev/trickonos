{   Include for sonull's None Type

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
 NONE (Immortal, 1 instance)
 ******************************************************************************)

type
  {Class for <None> Type}
  TSOTypeNone = class(TSOTypeBase)
    protected
      class function InstanceSize: MachineInt; override;
      class function IsMortal: Boolean; override;
    public
      class function BaseTypeName: String; override;
      class function UnOpNot(soself: PSOInstance): PSOInstance; override;
  end;

{ TSOTypeNone }

class function TSOTypeNone.InstanceSize: MachineInt;
{doesnt have extra space, only a reference}
begin
  Result := SizeOf(TSOInstance);
end;

class function TSOTypeNone.IsMortal: Boolean;
begin
  Result := false;
end;

class function TSOTypeNone.BaseTypeName: String;
begin
  Result := C_SOTYPE_NONE_NAME;
end;

class function TSOTypeNone.UnOpNot(soself: PSOInstance): PSOInstance;
{not nil = nil}
begin
  SelfCheck(soself,so_none_class);
  Result := soself;
end;
