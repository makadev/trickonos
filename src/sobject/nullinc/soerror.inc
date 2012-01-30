{   Include for sonull's Error Type

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
 ERROR
 ******************************************************************************)

type
  {error instance}
  PSO_Error = ^TSO_Error;
  TSO_Error = object(TSOInstance)
    msg: String;
  end;

procedure socls_Error_PreDestructor(instance: PSOInstance);
begin
  SetLength(PSO_Error(instance)^.msg,0);
end;

function socls_Error_TypeQuery(soself: PSOInstance): String;
begin
  {$IFDEF SELFCHECK}SelfCheck(soself,so_error_class);{$ENDIF}
  Result := C_SOTYPE_ERROR_NAME;
end;

(******************************************************************************
 ERROR Methods/Attr/Helper
 ******************************************************************************)

function so_error_init( const s: string ): PSOInstance;
begin
  Result := InitInstance(so_error_class);
  PSO_Error(Result)^.msg := s;
end;

function so_error_get(soerror: PSOInstance): String;
begin
  {$IFDEF SELFCHECK}SelfCheck(soerror,so_error_class);{$ENDIF}
  Result := PSO_Error(soerror)^.msg;
end;

