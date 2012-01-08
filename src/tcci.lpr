{   Loader/Starter

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

program tcci;

{$mode objfpc}{$H+}
{$CODEPAGE UTF8}


uses sysutils, appstart, eomsg,
  {compiler macros}
  cmacro;

{$R *.res}

begin
  ExitCode := 2;
{$IFDEF CLEANSHUTDOWN}
  try
{$ENDIF}
  appstart.Setup;
  appstart.Run;
{$IFDEF CLEANSHUTDOWN}
  except
    on e: ECriticalException do
      begin
        // ignore for clean shutdown - exitcode is set
      end;
    on Exception do
      raise; // reraise others
  end;
{$ENDIF}
  appstart.Shutdown;
end.

