{   Unit for scan and scanstream subsystem

    Copyright (C) 2012  Matthias Karbe

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

unit subsscan;

{$mode objfpc}{$H+}

interface

uses SysUtils, commontl, ucfs, eomsg, socore, solnull, sstreams, fpath;

(* This serves both, as test for TSubSystemDispatcher and extension which allows
   to use sstream based streams and build scanners.

   1. we need a SubSystem which serves as central API (ios instancer and
      interface)
   2. we need an IO (Internal Object) for scan streams which provides most
      of the functionality for script side scan streaming
   3. we need an IO for scanners, which may or may not uses the scanstream IO
      directly or its own stream (future todo)
 *)

type
  (* Scan Stream Interface (using a n-Lookahead scan stream)
       // "lookahead 0"
       Next() -> Boolean
       Current -> length 1 string or nil
       // ^^ next/current is usable with foreach iteration
       NextChar() -> Boolean; alias for Next
       CurrentChar -> string; alias for Current
       CurrentOrd -> currentchar as integer or nil

       // lookahead n
       LookAhead(n:integer) -> Boolean [n-lookahead possible?]
       LookAheadChar(n:integer) -> string or nil
       LookAheadOrd(n:integer) ->  integer or nil

       // skipping
       FastForward(n:integer) -> Boolean [skip n symbols]

       // newline detection
       NewLineSkip -> Boolean [detected newline]
       NewLineHit -> on read: return current NewLine hit (windows/unix/mac or none)
       NewLineStyle -> read/write newline mode (windows/unix/mac/mixed/none)

       // position
       CurrentPos -> current stream position
       CurrentLine -> current line
       CurrentCol -> current column
   *)

  TScanStreamIO = class(TInternalObject)
    private
      FScanStream: TScannerStream;
    protected
      procedure SetupOn(AScanStream: TScannerStream);

      function MethNextChar( callinfo:PMethodCallInfo ): PSOInstance;
      function MethLookAhead( callinfo:PMethodCallInfo ): PSOInstance;
      function MethLookAheadChar( callinfo:PMethodCallInfo ): PSOInstance;
      function MethLookAheadOrd( callinfo:PMethodCallInfo ): PSOInstance;
      function MethFastForward( callinfo:PMethodCallInfo ): PSOInstance;

      function AttrCurrentChar( attrinfo: PAttributeInfo ): PSOInstance;
      function AttrCurrentOrd( attrinfo: PAttributeInfo ): PSOInstance;

      function AttrNewLineSkip( attrinfo: PAttributeInfo ): PSOInstance;
      function AttrNewLineHit( attrinfo: PAttributeInfo ): PSOInstance;
      function AttrNewLineStyle( attrinfo: PAttributeInfo ): PSOInstance;

      function AttrCurrentPos( attrinfo: PAttributeInfo ): PSOInstance;
      function AttrCurrentLine( attrinfo: PAttributeInfo ): PSOInstance;
      function AttrCurrentCol( attrinfo: PAttributeInfo ): PSOInstance;
    public
      constructor Create; override;
      destructor Destroy; override;
  end;

  (* Scan Stream/Scanner Constructor Interface
       // streamer
       MemoryScanStream(filename:string,lookahead:=1) -> full buffered streamer on file
       FileScanStream(filename:string,lookahead:=1) -> file scanner
       StringScanStream(sostring:string,lookahead:=1) -> "full buffered" streamer on string [lock]
   *)

  TSubSysScan = class(TSubSystemDispatcher)
    protected
      procedure OnSubSystemLoad; override;
    public
      function ScanStreamCon( callinfo: PMethodCallInfo ): PSOInstance;
  end;

procedure InitAndRegister;

implementation

procedure InitAndRegister;
begin
  RegisterInternalObjectName('scannerstream');
  solnull.RegisterSubsystemObject('Scanning',TSubSysScan,false);
end;

{ TScanStreamIO }

procedure TScanStreamIO.SetupOn(AScanStream: TScannerStream);
begin
  FScanStream := AScanStream;
end;

function TScanStreamIO.MethNextChar(callinfo:PMethodCallInfo): PSOInstance;
begin
  if callinfo^.argnum = 0 then
    begin
      if FScanStream.NextChar then
        Result := so_true
      else
        Result := so_false;
    end
  else
    Result := nil;
end;

function TScanStreamIO.MethLookAhead(callinfo:PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      if (argnum = 1) and
         (soargs^[0]^.IsType(so_integer_class)) then
        begin
          if FScanStream.LookAhead(so_integer_get(soargs^[0],true)) then
            Result := so_true
          else
            Result := so_false;
        end
      else
        Result := nil;
    end;
end;

function TScanStreamIO.MethLookAheadChar(callinfo:PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      if (argnum = 1) and
         (soargs^[0]^.IsType(so_integer_class)) then
        begin
          if FScanStream.LookAhead(so_integer_get(soargs^[0],true)) then
            Result := so_string_init_char(FScanStream.LookAheadChar(so_integer_get(soargs^[0],true)))
          else
            Result := so_nil;
        end
      else
        Result := nil;
    end;
end;

function TScanStreamIO.MethLookAheadOrd(callinfo:PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      if (argnum = 1) and
         (soargs^[0]^.IsType(so_integer_class)) then
        begin
          if FScanStream.LookAhead(so_integer_get(soargs^[0],true)) then
            Result := so_integer_init(Ord(FScanStream.LookAheadChar(so_integer_get(soargs^[0],true))))
          else
            Result := so_nil;
        end
      else
        Result := nil;
    end;
end;

function TScanStreamIO.MethFastForward(callinfo:PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      if (argnum = 1) and
         (soargs^[0]^.IsType(so_integer_class)) then
        begin
          if FScanStream.FastForward(so_integer_get(soargs^[0],true)) then
            Result := so_true
          else
            Result := so_false;
        end
      else
        Result := nil;
    end;
end;

function TScanStreamIO.AttrCurrentChar(attrinfo:PAttributeInfo): PSOInstance;
begin
  if not Assigned(attrinfo^.setter) then
    begin
      if FScanStream.LookAhead(0) then
        Result := so_string_init_char(FScanStream.CurrentChar)
      else
        Result := so_nil;
    end
  else
    Result := nil;
end;

function TScanStreamIO.AttrCurrentOrd(attrinfo:PAttributeInfo): PSOInstance;
begin
  if not Assigned(attrinfo^.setter) then
    begin
      if FScanStream.LookAhead(0) then
        Result := so_integer_init(Ord(FScanStream.CurrentChar))
      else
        Result := so_nil;
    end
  else
    Result := nil;
end;

function TScanStreamIO.AttrNewLineSkip(attrinfo:PAttributeInfo): PSOInstance;
begin
  if not Assigned(attrinfo^.setter) then
    begin
      if FScanStream.NewLineSkip then
        Result := so_true
      else
        Result := so_false;
    end
  else
    Result := nil;
end;

function nlmtostring( nlm: TNewLineMode ): String;
begin
  case nlm of
    nlmNone: Result := 'none';
    nlmWindows: Result := 'windows';
    nlmUnix: Result := 'unix';
    nlmMac: Result := 'mac';
    nlmMixed: Result := 'mixed';
  else
    put_internalerror(2012011201);
  end;
end;

function TScanStreamIO.AttrNewLineHit(attrinfo:PAttributeInfo): PSOInstance;
begin
  if not Assigned(attrinfo^.setter) then
    Result := so_string_init_utf8(nlmtostring(FScanStream.NewLineHit))
  else
    Result := nil;
end;

function TScanStreamIO.AttrNewLineStyle(attrinfo:PAttributeInfo): PSOInstance;
begin
  with attrinfo^ do
    begin
      if not Assigned(setter) then
        begin
          Result := so_string_init_utf8(nlmtostring(FScanStream.NewLineMode));
        end
      else
        begin
          Result := nil;
          if setter^.IsType(so_string_class) then
            begin
              if ucfs_compare_a7(so_string_get_ucfs(setter,false),'none') = 0 then
                begin
                  FScanStream.NewLineMode := nlmNone;
                  Result := setter;
                  Result^.IncRef;
                end
              else if ucfs_compare_a7(so_string_get_ucfs(setter,false),'windows') = 0 then
                begin
                  FScanStream.NewLineMode := nlmWindows;
                  Result := setter;
                  Result^.IncRef;
                end
              else if ucfs_compare_a7(so_string_get_ucfs(setter,false),'unix') = 0 then
                begin
                  FScanStream.NewLineMode := nlmUnix;
                  Result := setter;
                  Result^.IncRef;
                end
              else if ucfs_compare_a7(so_string_get_ucfs(setter,false),'mac') = 0 then
                begin
                  FScanStream.NewLineMode := nlmMac;
                  Result := setter;
                  Result^.IncRef;
                end
              else if ucfs_compare_a7(so_string_get_ucfs(setter,false),'mixed') = 0 then
                begin
                  FScanStream.NewLineMode := nlmMixed;
                  Result := setter;
                  Result^.IncRef;
                end;
            end;
        end;
    end;
end;

function TScanStreamIO.AttrCurrentPos(attrinfo:PAttributeInfo): PSOInstance;
begin
  if not Assigned(attrinfo^.setter) then
    Result := so_integer_init(FScanStream.CurrentPos)
  else
    Result := nil;
end;

function TScanStreamIO.AttrCurrentLine(attrinfo:PAttributeInfo): PSOInstance;
begin
  if not Assigned(attrinfo^.setter) then
    Result := so_integer_init(FScanStream.CurrentLine)
  else
    Result := nil;
end;

function TScanStreamIO.AttrCurrentCol(attrinfo:PAttributeInfo): PSOInstance;
begin
  if not Assigned(attrinfo^.setter) then
    Result := so_integer_init(FScanStream.CurrentCol)
  else
    Result := nil;
end;

constructor TScanStreamIO.Create;
begin
  inherited Create;
  self.RegisterMethod('NEXTCHAR',@MethNextChar);
  self.RegisterMethod('NEXT',@MethNextChar);
  self.RegisterMethod('FASTFORWARD',@MethFastForward);

  self.RegisterMethod('LOOKAHEAD',@MethLookAhead);
  self.RegisterMethod('LOOKAHEADCHAR',@MethLookAheadChar);
  self.RegisterMethod('LOOKAHEADORD',@MethLookAheadOrd);

  self.RegisterAttribute('CURRENTCHAR',@AttrCurrentChar);
  self.RegisterAttribute('CURRENT',@AttrCurrentChar);
  self.RegisterAttribute('CURRENTORD',@AttrCurrentOrd);

  self.RegisterAttribute('CURRENTPOS',@AttrCurrentPos);
  self.RegisterAttribute('CURRENTLINE',@AttrCurrentLine);
  self.RegisterAttribute('CURRENTCOLUMN',@AttrCurrentCol);

  self.RegisterAttribute('NEWLINESKIP',@AttrNewLineSkip);
  self.RegisterAttribute('NEWLINEHIT',@AttrNewLineHit);
  self.RegisterAttribute('NEWLINESTYLE',@AttrNewLineStyle);
end;

destructor TScanStreamIO.Destroy;
begin
  FScanStream.Free;
  inherited Destroy;
end;

{ TSubSysScan }

procedure TSubSysScan.OnSubSystemLoad;
begin
  RegisterMethod('MEMORYSCANSTREAM',@ScanStreamCon);
  RegisterMethod('FILESCANSTREAM',@ScanStreamCon);
  RegisterMethod('STRINGSCANSTREAM',@ScanStreamCon);
end;

function TSubSysScan.ScanStreamCon(callinfo: PMethodCallInfo): PSOInstance;
var LAChars: VMInt;
    fname: String;
    tmps: PUCFS32String;
    memblock: TMemoryBlock;
    scstream: TScannerStream;
    readerstream: TCharReaderStream;
begin
  with callinfo^ do
    begin
      if (argnum >= 1) and
         (argnum <= 2) then
        begin
          LAChars := 1;
          {check params n stuff}
          if soargs^[0]^.IsType(so_string_class) then
            begin
              if (argnum = 2) and
                 (soargs^[1]^.IsType(so_integer_class)) then
                begin
                  LAChars := so_integer_get(soargs^[1],true);
                  if (LAChars <= 0) or
                     (LAChars > 1024) then
                    begin
                      Result := init_invargvalue_error(soself,soargs^[1],2,name);
                      Exit(Result);
                    end;
                end
              else  if (argnum = 2) then
                begin
                  Result := init_invargtype_error(soself,soargs^[1],2,name);
                  Exit(Result);
                end;
            end
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);

          readerstream := nil;
          {checked, create the reader stream}
          if (ucfs_compare_a7(name,'FILESCANSTREAM')=0) or
             (ucfs_compare_a7(name,'MEMORYSCANSTREAM')=0) then
            begin
              fname := fpath_rel_to_abs(so_string_get_utf8(soargs^[0]));
              put_debug('open file for input: '+fname);
              if (Length(fname) <= 0) or
                 (not FileExists(fname)) then
                Exit(so_nil);
              if ucfs_compare_a7(name,'MEMORYSCANSTREAM')=0 then
                begin
                  memblock := TMemoryBlock.Create;
                  tmps := ucfs_utf8us(fname);
                  if memblock.ReadFromFile(tmps) then
                    begin
                      ucfs_release(tmps);
                      readerstream := TMemoryBlockReaderStream.Create(nil,memblock);
                    end
                  else
                    begin
                      ucfs_release(tmps);
                      memblock.Free;
                      Exit(so_nil);
                    end;
                end
              else
                begin
                  tmps := ucfs_utf8us(fname);
                  readerstream := TFileReaderStream.Create(nil, tmps);
                  ucfs_release(tmps);
                end;
            end
          else
            readerstream := TStringReaderStream.Create(nil,so_string_get_ucfs(soargs^[0],false));

          {create the scanner stream}
          scstream := TScannerStream.Create(LAChars,readerstream);

          {create IO}
          Result := so_create_internalobject('ScannerStream',TScanStreamIO);
          TScanStreamIO(so_internalobject_get(Result)).SetupOn(scstream);

          {done}
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

end.

