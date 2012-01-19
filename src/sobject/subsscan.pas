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

uses SysUtils, commontl, eomsg, socore, solnull, sstreams, fpath;

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

      function MethNextChar( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
      function MethLookAhead( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
      function MethLookAheadChar( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
      function MethLookAheadOrd( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
      function MethFastForward( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;

      function AttrCurrentChar( const aname: String; soself: PSOInstance; setter: PSOInstance ): PSOInstance;
      function AttrCurrentOrd( const aname: String; soself: PSOInstance; setter: PSOInstance ): PSOInstance;

      function AttrNewLineSkip( const aname: String; soself: PSOInstance; setter: PSOInstance ): PSOInstance;
      function AttrNewLineHit( const aname: String; soself: PSOInstance; setter: PSOInstance ): PSOInstance;
      function AttrNewLineStyle( const aname: String; soself: PSOInstance; setter: PSOInstance ): PSOInstance;

      function AttrCurrentPos( const aname: String; soself: PSOInstance; setter: PSOInstance ): PSOInstance;
      function AttrCurrentLine( const aname: String; soself: PSOInstance; setter: PSOInstance ): PSOInstance;
      function AttrCurrentCol( const aname: String; soself: PSOInstance; setter: PSOInstance ): PSOInstance;
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
      function ScanStreamCon( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
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

function TScanStreamIO.MethNextChar(const mname: String; soself: PSOInstance;
  soargs: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
begin
  if argnum = 0 then
    begin
      if FScanStream.NextChar then
        Result := so_true
      else
        Result := so_false;
    end
  else
    Result := nil;
end;

function TScanStreamIO.MethLookAhead(const mname: String; soself: PSOInstance;
  soargs: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
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

function TScanStreamIO.MethLookAheadChar(const mname: String;
  soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
begin
  if (argnum = 1) and
     (soargs^[0]^.IsType(so_integer_class)) then
    begin
      if FScanStream.LookAhead(so_integer_get(soargs^[0],true)) then
        Result := so_string_init(FScanStream.LookAheadChar(so_integer_get(soargs^[0],true)))
      else
        Result := so_nil;
    end
  else
    Result := nil;
end;

function TScanStreamIO.MethLookAheadOrd(const mname: String;
  soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
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

function TScanStreamIO.MethFastForward(const mname: String;
  soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
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

function TScanStreamIO.AttrCurrentChar(const aname: String;
  soself: PSOInstance; setter: PSOInstance): PSOInstance;
begin
  if not Assigned(setter) then
    begin
      if FScanStream.LookAhead(0) then
        Result := so_string_init(FScanStream.CurrentChar)
      else
        Result := so_nil;
    end
  else
    Result := nil;
end;

function TScanStreamIO.AttrCurrentOrd(const aname: String; soself: PSOInstance;
  setter: PSOInstance): PSOInstance;
begin
  if not Assigned(setter) then
    begin
      if FScanStream.LookAhead(0) then
        Result := so_integer_init(Ord(FScanStream.CurrentChar))
      else
        Result := so_nil;
    end
  else
    Result := nil;
end;

function TScanStreamIO.AttrNewLineSkip(const aname: String;
  soself: PSOInstance; setter: PSOInstance): PSOInstance;
begin
  if not Assigned(setter) then
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

function TScanStreamIO.AttrNewLineHit(const aname: String; soself: PSOInstance;
  setter: PSOInstance): PSOInstance;
begin
  if not Assigned(setter) then
    Result := so_string_init(nlmtostring(FScanStream.NewLineHit))
  else
    Result := nil;
end;

function TScanStreamIO.AttrNewLineStyle(const aname: String;
  soself: PSOInstance; setter: PSOInstance): PSOInstance;
begin
  if not Assigned(setter) then
    begin
      Result := so_string_init(nlmtostring(FScanStream.NewLineMode));
    end
  else
    begin
      Result := nil;
      if setter^.IsType(so_string_class) then
        begin
          if Length(so_string_get(setter)) <= 7 then
            begin
              if so_string_get(setter) = 'none' then
                begin
                  FScanStream.NewLineMode := nlmNone;
                  Result := setter;
                  Result^.IncRef;
                end
              else if so_string_get(setter) = 'windows' then
                begin
                  FScanStream.NewLineMode := nlmWindows;
                  Result := setter;
                  Result^.IncRef;
                end
              else if so_string_get(setter) = 'unix' then
                begin
                  FScanStream.NewLineMode := nlmUnix;
                  Result := setter;
                  Result^.IncRef;
                end
              else if so_string_get(setter) = 'mac' then
                begin
                  FScanStream.NewLineMode := nlmMac;
                  Result := setter;
                  Result^.IncRef;
                end
              else if so_string_get(setter) = 'mixed' then
                begin
                  FScanStream.NewLineMode := nlmMixed;
                  Result := setter;
                  Result^.IncRef;
                end;
            end;
        end;
    end;
end;

function TScanStreamIO.AttrCurrentPos(const aname: String; soself: PSOInstance;
  setter: PSOInstance): PSOInstance;
begin
  if not Assigned(setter) then
    Result := so_integer_init(FScanStream.CurrentPos)
  else
    Result := nil;
end;

function TScanStreamIO.AttrCurrentLine(const aname: String;
  soself: PSOInstance; setter: PSOInstance): PSOInstance;
begin
  if not Assigned(setter) then
    Result := so_integer_init(FScanStream.CurrentLine)
  else
    Result := nil;
end;

function TScanStreamIO.AttrCurrentCol(const aname: String; soself: PSOInstance;
  setter: PSOInstance): PSOInstance;
begin
  if not Assigned(setter) then
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

function TSubSysScan.ScanStreamCon(const mname: String; soself: PSOInstance;
  soargs: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
var LAChars: VMInt;
    fname: String;
    memblock: TMemoryBlock;
    scstream: TScannerStream;
    readerstream: TCharReaderStream;
begin
  if (argnum >= 1) and
     (argnum <= 2) then
    begin
      {check params n stuff}
      if soargs^[0]^.IsType(so_string_class) then
        begin
          if (argnum = 2) and
             (soargs^[1]^.IsType(so_integer_class)) then
            begin
              LAChars := so_integer_get(soargs^[1],true);
              if (LAChars <= 0) and
                 (LAChars > 1024) then
                begin
                  Result := init_invargvalue_error(soself,soargs^[1],2,mname);
                  Exit(Result);
                end;
            end
          else  if (argnum = 2) then
            begin
              Result := init_invargtype_error(soself,soargs^[1],2,mname);
              Exit(Result);
            end
          else
            LAChars := 1;
        end
      else
        Result := init_invargtype_error(soself,soargs^[0],1,mname);

      {checked, create the reader stream}
      if (Upcase(mname) = 'FILESCANSTREAM') or
         (Upcase(mname) = 'MEMORYSCANSTREAM') then
        begin
          fname := fpath_rel_to_abs(so_string_get(soargs^[0]));
          put_debug('open file for input: '+fname);
          if (Length(fname) <= 0) or
             (not FileExists(fname)) then
            Exit(so_nil);
          if Upcase(mname) = 'MEMORYSCANSTREAM' then
            begin
              memblock := TMemoryBlock.Create;
              if memblock.ReadFromFile(fname) then
                readerstream := TMemoryBlockReaderStream.Create(so_string_get(soargs^[0]),memblock)
              else
                begin
                  memblock.Free;
                  Exit(so_nil);
                end;
            end
          else
            readerstream := TFileReaderStream.Create(so_string_get(soargs^[0]), fname);
        end
      else
        readerstream := TStringReaderStream.Create('string',so_string_get(soargs^[0]));

      {create the scanner stream}
      scstream := TScannerStream.Create(LAChars,readerstream);

      {create IO}
      Result := so_create_internalobject('ScannerStream',TScanStreamIO);
      TScanStreamIO(so_internalobject_get(Result)).SetupOn(scstream);

      {done}
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;

end.

