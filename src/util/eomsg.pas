{   Unit for reporting, messages, critical and internal faults

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

unit eomsg;

{$mode objfpc}{$H+}

interface

uses SysUtils, commontl;

(******************************************************************************
 * Exceptions
 ******************************************************************************)

type
  {DOC>> Root for partitial unexpected Exceptions}
  EInternalException = class(Exception)
  end;

{$IFDEF CLEANSHUTDOWN}
  ECriticalException = class(Exception)
  end;
{$ENDIF}

(******************************************************************************
 * Info and Error Output
 ******************************************************************************)

type
  TOutMode = (
    omSilent = 0,
    omCritical,
    omError,
    omWarning,
    omInfo,
    omDebug
  );

const
  C_MessagePrefix: array[TOutMode] of String = (
      '>>', // <- used as default message prefix, building ..">> Error: "...
      'Critical:',
      'Error:',
      'Warning:',
      'Info:',
      '#Debug#:'
    );

{DOC>> checks level>=omDebug, allowing checks before doing work for extra debug
       infos}
function debug_mode: Boolean;
{DOC>> checks level>=omInfo, allowing checks before doing work for extra infos}
function info_mode: Boolean;

{DOC>> set maximum number of errors allowed, before a critical is triggered.
       setting <= 0 means "endless", setting the error max will reset the
       error counter}
procedure set_maxerrors( maxerrs: Integer );
{DOC>> get error counter}
function get_errors: Integer;
{DOC>> set current verbosity Level}
procedure set_outmode( omode: TOutMode );
{DOC>> set current outfile (for resetting to stderr, use reset_outfile,
       stdout is not supported, since stdout is default for generated Output!)}
procedure set_outfile( const fn: String );
{DOC>> flush current outfile and reset it to StdErr}
procedure reset_outfile;
{DOC>> flush current outfile}
procedure flush_outfile;

{DOC>> Trigger an Internal Error (unexpected/unspecified/unwanted behaviour). @br
       In debugmode (debug_mode=@true), (flushes and) resets the outfile to
       StdErr and raises an EInternalException such that the Line/File can be
       found.
       In info or less verbose mode (debug_mode=@false), triggers a critical
       with given code val (which terminates the Applikation).}
procedure put_internalerror( val: Integer );


(*
   Output Routines without specific Line/Column/File info.
   Line/Column/File will be set by the VM, so these report errors using the
   current VM Instructions LineInfo and Template (Short) Name.
 *)

{DOC>> set lineinfo, used for messages without line/column/file}
procedure set_currentlineinfo( line, column: StreamInt ); inline;
{DOC>> set filename, used for messages without line/column/file}
procedure set_currentfile( const s: String ); inline;

{DOC>> Output a Message (verbosity < omSilent)}
procedure put_message( const s: String ); inline;
{DOC>> Output a Message (verbosity >= omCritical),
       Application will be Terminated using Halt (0 for Failtest, 1 default).}
procedure put_critical( const s: String ); inline;
{DOC>> Output a Message (verbosity >= omError), after a specified maximum
       number of errors, this triggers a critical}
procedure put_error( const s: String ); inline;
{DOC>> Output a Message (verbosity >= omWarning)}
procedure put_warning( const s: String ); inline;
{DOC>> Output a Message (verbosity >= omInfo)}
procedure put_info( const s: String ); inline;
{DOC>> Output a Message (verbosity >= omDebug)}
procedure put_debug( const s: String ); inline;


(*
   Output Routines with specific Line/Column/File info.
   This should be used f.e. for Compiler/Scanner reporting positions in
   the Scanned/Parsed stream, without changing the LineInfo which points
   to the Instruction that started the Compiler/Scanner.
 *)


{DOC>> Output a Message (see put_*), use explicit line/colum/filename}
procedure put_message_for( line, column: StreamInt; const sid, s: String );
{DOC>> Output a Message (see put_*), use explicit line/colum/filename}
procedure put_error_for( line, column: StreamInt; const sid, s: String ); inline;
{DOC>> Output a Message (see put_*), use explicit line/colum/filename}
procedure put_critical_for( line, column: StreamInt; const sid, s: String ); inline;
{DOC>> Output a Message (see put_*), use explicit line/colum/filename}
procedure put_warning_for( line, column: StreamInt; const sid, s: String ); inline;
{DOC>> Output a Message (see put_*), use explicit line/colum/filename}
procedure put_info_for( line, column: StreamInt; const sid, s: String ); inline;
{DOC>> Output a Message (see put_*), use explicit line/colum/filename}
procedure put_debug_for( line, column: StreamInt; const sid, s: String ); inline;


{DOC>> Output a Level Message, used by all put_* functions}
procedure put_levelmessage( lvl: TOutMode; Line, Column: StreamInt;
                            const sid, s: String );

{DOC>> Checks if failtest mode is set/unset (used to set the correct Exitcode
       on Termination).}
function failtest_mode: Boolean; inline;
{DOC>> Set/Unset failtest mode. In failtest mode criticals should halt with
       exitcode = 0, normal and unspecified (internal errors) terminations
       with exitcode <> 0. Without failtest mode, criticals and unspecified
       terminations should set an exitcode <> 0, Normal behaviour an exitcode
       of 0.
       In both cases, Normal behaviour can be changed due to Templates setting
       an exitcode = 0 or <> 0. The result is inverted, allowing to check
       Templates with failtest mode.}
procedure set_failtest_mode( switch: Boolean );

(**
 * Init&Fin
 **)

{Initialize Error/Reporting unit}
procedure eomsg_init;
{Finalize Error/Reporting unit (flush output)}
procedure eomsg_fini;

implementation

(******************************************************************************
 * Failtest
 ******************************************************************************)

var
  failtestmode: Boolean;

function failtest_mode: Boolean;
begin
  Result := failtestmode;
end;

procedure set_failtest_mode( switch: Boolean );
begin
  failtestmode := switch;
end;

(******************************************************************************
 * Current Line/Column/File Info
 ******************************************************************************)

var
  currentFile: String;
  currentLine: StreamInt;
  currentCol: StreamInt;

procedure set_currentlineinfo( line, column: StreamInt );
begin
  currentLine := line;
  currentCol := column;
end;

procedure set_currentfile( const s: String );
begin
  currentFile := s;
end;

(******************************************************************************
 * Info and Error Output
 ******************************************************************************)

const
  C_OutBuffer = 512;

var
  outmode: TOutMode;
  outbuffer: array[ 1 .. C_OutBuffer ] of char;
  outfile_isset: Boolean;
  outfile: Text;
  maxerrors: Integer;
  reperrors: Integer;

function debug_mode: Boolean;
begin
  Result := outmode >= omDebug;
end;

function info_mode: Boolean;
begin
  Result := outmode >= omInfo;
end;

procedure set_maxerrors(maxerrs: Integer);
begin
  maxerrors := maxerrs;
  reperrors := 0;
end;

function get_errors: Integer;
begin
  Result := reperrors;
end;

procedure set_outmode(omode: TOutMode);
begin
  outmode := omode;
end;

procedure set_outfile(const fn: String);
begin
  reset_outfile;
  if FileExists(fn) then
    put_critical('Output Target '+fn+' already exists.');
{$I+}
  AssignFile(outfile,fn);
  Rewrite(outfile);
  SetTextBuf(outfile,outbuffer,C_OutBuffer);
{$I-}
  if IOResult <> 0 then
    put_internalerror(201121191);
  outfile_isset := true;
end;

procedure reset_outfile;
begin
  flush_outfile;
  if not outfile_isset then
    exit;
  outfile_isset := false;
{$I+}
  CloseFile(outfile);
{$I-}
  if IOResult <> 0 then
    put_internalerror(201110211);
end;

procedure flush_outfile;
begin
  if outfile_isset then
    Flush(outfile)
  else
    Flush(StdErr);
end;

procedure put_internalerror(val: Integer);
begin
  if debug_mode then
    begin
      reset_outfile;
      raise EInternalException.Create('Internal Error ('+IntToStr(val)+')');
    end
  else
    put_critical( 'Internal Error ('+IntToStr(val)+')' );
end;

(******************************************************************************
 * Info and Error Output Reporter
 ******************************************************************************)

procedure put_message(const s: String);
begin
  put_message_for(currentLine,currentCol,currentFile,s);
end;

procedure put_critical(const s: String);
begin
  put_critical_for(currentLine,currentCol,currentFile,s);
end;

procedure put_error(const s: String);
begin
  put_error_for(currentLine,currentCol,currentFile,s);
end;

procedure put_warning(const s: String);
begin
  put_warning_for(currentLine,currentCol,currentFile,s);
end;

procedure put_info(const s: String);
begin
  put_info_for(currentLine,currentCol,currentFile,s);
end;

procedure put_debug(const s: String);
begin
  put_debug_for(currentLine,currentCol,currentFile,s);
end;

(******************************************************************************
 * Info and Error Output Reporter with LineInfo
 ******************************************************************************)

procedure put_message_for(line, column: StreamInt; const sid, s: String);
var linestr: String;
begin
  if outmode <= omSilent then
    Exit;
  if line >= 0 then
    begin
      if Column >= 0 then
        WriteStr(linestr,'.l',line,'.c',column)
      else
        WriteStr(linestr,'.l',line);
    end
  else
    linestr := '';
  if outfile_isset then
    WriteLn(outfile,sid,linestr,
      C_MessagePrefix[omSilent],' ',s)
  else
    WriteLn(StdErr,sid,linestr,
      C_MessagePrefix[omSilent],' ',s);
end;

procedure put_error_for(line, column: StreamInt; const sid, s: String);
begin
  put_levelmessage(omError,line,column,sid,s);
end;

procedure put_critical_for(line, column: StreamInt; const sid, s: String);
begin
  put_levelmessage(omCritical,line,column,sid,s);
end;

procedure put_warning_for(line, column: StreamInt; const sid, s: String);
begin
  put_levelmessage(omWarning,line,column,sid,s);
end;

procedure put_info_for(line, column: StreamInt; const sid, s: String);
begin
  put_levelmessage(omInfo,line,column,sid,s);
end;

procedure put_debug_for(line, column: StreamInt; const sid, s: String);
begin
  put_levelmessage(omDebug,line,column,sid,s);
end;

procedure put_levelmessage(lvl: TOutMode; Line, Column: StreamInt; const sid,
  s: String);
var linestr: String;
begin
  if lvl > omSilent then
    begin
      {check wether to report the line}
      if lvl <= outmode then
        begin
          if Line >= 0 then
            begin
              if Column >= 0 then
                WriteStr(linestr,'.l',Line,'.c',Column)
              else
                WriteStr(linestr,'.l',Line);
            end
          else
            linestr := '';
          if outfile_isset then
            WriteLn(outfile,sid,linestr,
              C_MessagePrefix[omSilent],' ',
              C_MessagePrefix[lvl],' ',s)
          else
            WriteLn(StdErr,sid,linestr,
              C_MessagePrefix[omSilent],' ',
              C_MessagePrefix[lvl],' ',s);
        end;
      if (lvl = omError) and
         (maxerrors > 0) then
        begin
          {inc error counter or change mode}
          Inc(reperrors,1);
          {add additional critical message (lvl > omSilent, so just put it)
           and set lvl to omCritical}
          if reperrors >= maxerrors then
            begin
              if Line >= 0 then
                begin
                  if Column >= 0 then
                    WriteStr(linestr,'.l',Line,'.c',Column)
                  else
                    WriteStr(linestr,'.l',Line);
                end
              else
                linestr := '';
              if outfile_isset then
                WriteLn(outfile,sid,linestr,
                  C_MessagePrefix[omSilent],' ',
                  C_MessagePrefix[omCritical],' maximum amount of Errors (',maxerrors,').')
              else
                WriteLn(StdErr,sid,linestr,
                  C_MessagePrefix[omSilent],' ',
                  C_MessagePrefix[omCritical],' maximum amount of Errors (',maxerrors,').');
              lvl := omCritical;
            end;
        end;
    end
  else
    begin
      {check also in silent mode}
      if (lvl = omError) and
         (maxerrors > 0) then
        begin
          Inc(reperrors,1);
          if reperrors >= maxerrors then
            lvl := omCritical;
        end;
    end;
  {critical/silent(silent termination) -> end}
  if lvl <= omCritical then
    begin
      reset_outfile;
{$IFNDEF CLEANSHUTDOWN}
      if failtestmode then
        Halt(0)
      else
        Halt(1);
{$ELSE}
      {double switch the exit codes,
        -> exception will be caught in program stub
        -> it runs shutdown code, which switches the exitcode
           for failtest (for normal termination,
           any crash in between is those a fault which is correct for clean
           shutdown)}
      if failtestmode then
        ExitCode := 1
      else
        ExitCode := 0;
      raise ECriticalException.Create('ignore me');
{$ENDIF}
    end;
end;

(******************************************************************************
 * Info and Error Output Init&Fin
 ******************************************************************************)

procedure eomsg_init;
begin
  failtestmode := false;
  currentFile := '';
  currentLine := -1;
  currentCol := -1;
  outfile_isset := false;
  outmode := omInfo;
  maxerrors := 10;
  reperrors := 0;
end;

procedure eomsg_fini;
begin
  reset_outfile;
end;

end.
