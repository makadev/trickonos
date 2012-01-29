{   Unit for String Constants and Resource Strings

    Copyright (C) Matthias Karbe

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

unit cons;

{$mode objfpc}{$H+}

interface

uses SysUtils, ucfs;

{$I ctab_int.inc}

function ci_str( i: TConstant_Index ): String; inline;
function ci_us( i: TConstant_Index; cpy: Boolean ): PUCFS32String; inline;

function mi_str( i: TMessage_Index ): String; inline;
function mi_us( i: TMessage_Index; cpy: Boolean ): PUCFS32String; inline;

implementation

var
  _ci_us: array[TConstant_Index] of PUCFS32String;
  _mi: array[TMessage_Index] of String;
  _mi_us: array[TMessage_Index] of PUCFS32String;

{$I ctab_imp.inc}

function mi_str( i: TMessage_Index ): String;
begin
  Result := _mi[i];
end;

function mi_us( i: TMessage_Index; cpy: Boolean ): PUCFS32String;
begin
  if not Assigned(_mi_us[i]) then
    _mi_us[i] := ucfs_utf8us(_mi[i]);
  if not cpy then
    Result := _mi_us[i]
  else
    Result := ucfs_copy(_mi_us[i]);
end;

function ci_str( i: TConstant_Index ): String;
begin
  Result := C_String_Constant_Table[i];
end;

function ci_us( i: TConstant_Index; cpy: Boolean ): PUCFS32String;
begin
  if not Assigned(_ci_us[i]) then
    _ci_us[i] := ucfs_utf8us(C_String_Constant_Table[i]);
  if not cpy then
    Result := _ci_us[i]
  else
    Result := ucfs_copy(_ci_us[i]);
end;

procedure wipe_ci;
var i: TConstant_Index;
begin
  for i := Low(TConstant_Index) to High(TConstant_Index) do
    if Assigned(_ci_us[i]) then
      begin
        ucfs_release(_ci_us[i]);
        _ci_us[i] := nil;
      end;
end;

procedure wipe_mi;
var i: TMessage_Index;
begin
  for i := Low(TMessage_Index) to High(TMessage_Index) do
    if Assigned(_mi_us[i]) then
      begin
        ucfs_release(_mi_us[i]);
        _mi_us[i] := nil;
      end;
end;

initialization
  FillByte(_ci_us,SizeOf(_ci_us),0);
  FillByte(_mi_us,SizeOf(_mi_us),0);

finalization
  wipe_mi;
  wipe_ci;

end.

