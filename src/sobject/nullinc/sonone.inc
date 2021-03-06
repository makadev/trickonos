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
  TSO_None = TSOInstance;

function socls_None_TypeQuery( soself: PSOInstance ): String;
begin
  {$IFDEF SELFCHECK}SelfCheck(soself,so_none_class);{$ENDIF}
  Result := C_SOTYPE_NONE_NAME;
end;

function socls_None_MethodCall( callinfo: PMethodCallInfo ): PSOInstance;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_none_class);{$ENDIF}
      if (hashk = DEFAULT_METHOD_UnOpNot_Hash) and
         (ucfs_compare(name,ci_us(ci_dm_not,false))=0) then
        begin
          {NONE::Not() -> nil}
          Result := soself;
        end
      else
        Result := nil;
    end;
end;

