{   Unit for character wise file and memory streaming for scanners

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

unit sstreams;

{$mode objfpc}{$H+}

interface

uses SysUtils, commontl;

type
  //DOC>> Abstract Character (Reader) Stream
  TCharReaderStream = class
    public
      //DOC>> Load Next Char @returns @false on end of stream
      function NextChar: Boolean; virtual; abstract;
      {DOC>> @returns Current Character after NextChar was called and
             returned @true, undefined (behavior) otherwise}
      function CurrentChar: Char; virtual; abstract;
      {DOC>> @returns Stream Identifier}
      function StreamID: String; virtual; abstract;
  end;

  //DOC>> TScannerStream NewLine (LineEnding) detection Modes
  TNewLineMode = (
    //DOC>> only Position - ignore Line/Column
    nlmNone,   
    //DOC>> detect CRLF Sequence
    nlmWindows,
    //DOC>> detect LF Sequence
    nlmUnix,
    //DOC>> detect CR Sequence
    nlmMac,
    //DOC>> detect LF/CR/CRLF - CRLF Sequence is one LineEnding, not two
    nlmMixed
  );

  //DOC>> Char Stream Wrapper with NewLine Detection and (fixed) LookAhead
  TScannerStream = class( TCharReaderStream )
    private
      {Wrapped Stream}
      //DOC>> Wrapped Stream
      FCharStream: TCharReaderStream;
      //DOC>> wether the Wrapped Stream is Owned (Free on Destroy)
      FStreamOwner: Boolean;

      {LookAhead Support}
      //DOC>> LookAhead RingBuffer
      FRBuffer: array of Char;
      //DOC>> RingBuffer Index for last loaded LookAhead Symbol (Char)
      FLAIndex: Integer;
      //DOC>> Keeps track of the number of loaded LookAhead Symbols
      FLAFillCounter: Integer;
      //DOC>> RingBuffer Index for Current Symbol (Char)
      FCIndex: Integer;

      {NewLine Support}
      //DOC>> Current Newline detection Mode
      FNewLineMode: TNewLineMode;
      //DOC>> Current detected NewLine Sequence (no nlmMixed)
      FNewLineHit: TNewLineMode;
      FCurrentLine: StreamInt;
      FCurrentCol: StreamInt;
      FCurrentPos: StreamInt;
      FNewLineSkip: Boolean;
    public

      {NewLine Support}
      //DOC>> true if Last NextChar Call Incremented Line Counter
      property NewLineSkip: Boolean read FNewLineSkip;
      //DOC>> Detected NewLine (only valid when NewLineSkip=true)
      //DOC>> is never nlmMixed, only nlmMac,nlmUnix,nlmWindows according to hit
      property NewLineHit: TNewLineMode read FNewLineHit;
      //DOC>> Current Position in Stream (LookAhead is not Accounted)
      property CurrentPos: StreamInt read FCurrentPos;
      //DOC>> Current Line (LookAhead is not Accounted)
      //DOC>> The first Line is 1, any detected NewLine Sequence is part
      //DOC>> of the Line
      property CurrentLine: StreamInt read FCurrentLine;
      //DOC>> Current Column (LookAhead is not Accounted)
      //DOC>> The first Column is 1
      property CurrentCol: StreamInt read FCurrentCol;
      //DOC>> NewLine Mode, Defaults to nlmMixed
      //DOC>> This setter does NOT correct the state, so changing
      //DOC>> Mode wont affect the Current Counters and detection
      //DOC>> may detect only NL Sequences read after switching
      property NewLineMode: TNewLineMode read FNewLineMode write FNewLineMode;

      {LookAhead Support}
      {DOC>> Check wether n LookAhead is possible
             @returns @false if n is out of possible LookAhead Range}
      function LookAhead( n: Integer ): Boolean;
      {DOC>> Get the n'th LookAhead Char
             @returns Char if n is in (possible) LookAhead Range, 
             otherwise undefined}
      function LookAheadChar( n: Integer ): Char;
      //DOC>> Skip n Chars @returns @false if end of stream is hit
      function FastForward( n: Integer ): Boolean;

      {Char Reader Part}
      function StreamID: String; override;
      //DOC>> Set the Next Char (and Reads the Next LookAhead Symbol)
      //DOC>> Update Pos/Line/Col and CurrentChar
      //DOC>> @returns @false if there is no Next Char
      function NextChar: Boolean; override;
      function CurrentChar: Char; override;
      //DOC>> @param ALASize Max LookAhead, will be preloaded
      //DOC>> @param ACharStream Stream to read from (Wrapped Stream)
      //DOC>> @param AStreamOwner Wether ACharStream should be Freed on Destroy
      constructor Create( ALASize: Integer;
                          ACharStream: TCharReaderStream;
                          AStreamOwner: Boolean = True );
      destructor Destroy; override;
  end;

  //DOC>> Concrete Character (Reader) FileStream
  TFileReaderStream = class( TCharReaderStream )
    private
      FID: String;
      FFileName: String;
      FFileHandle: THandle;
      FOpen: Boolean;
      FBuffer: packed array of Char;
      FBufMax: Integer;
      FBufSize: Integer;
      FBufPos: Integer;
      function BufferNext: Boolean;
    public
      property FileName: String read FFileName;
      function NextChar: Boolean; override;
      function CurrentChar: Char; override;
      function StreamID: String; override;
      constructor Create( const AID, AFileName: String; ABufMax: Integer = CL_Default_FileBuffer );
      destructor Destroy; override;
  end;

  TMemoryChunk = array of Char;

  TMemoryBlock = class
    private
      FChunkTable: array of TMemoryChunk;
      FChunkSize: Integer;
      FSize: StreamInt;
    public
      procedure Reset;
      function ReadFromFile( const AFileName: String ): Boolean;

      function Size: StreamInt;
      function Read( index: StreamInt ): Char;
      constructor Create( AChunkSize: Integer = CL_Default_FileBuffer );
      destructor Destroy; override;
  end;

  TMemoryBlockReaderStream = class( TCharReaderStream )
    private
      FID: String;
      FMemBlock: TMemoryBlock;
      FSRead: SizeInt;
      FChar: Char;
      FBlockOwner: Boolean;
    public
      function CurrentChar: Char; override;
      function NextChar: Boolean; override;
      function StreamID: String; override;
      constructor Create( const AID: String; AMemBlock: TMemoryBlock; ABlockOwner: Boolean = True );
      destructor Destroy; override;
  end;

  TStringReaderStream = class( TCharReaderStream )
    private
      FID: String;
      FReadString: String;
      FPos: Integer;
      FChar: Char;
    public
      function CurrentChar: Char; override;
      function NextChar: Boolean; override;
      function StreamID: String; override;
      constructor Create( const AID: String; const AReadString: String );
  end;

implementation

{ TStringReaderStream }

function TStringReaderStream.CurrentChar: Char;
begin
  Result := FChar;
end;

function TStringReaderStream.NextChar: Boolean;
begin
  if FPos < Length(FReadString) then
    begin
      Inc(FPos,1);
      FChar := FReadString[ FPos ];
      Result := true;
    end
  else
    Result := false;
end;

function TStringReaderStream.StreamID: String;
begin
  Result := FID;
end;

constructor TStringReaderStream.Create(const AID: String;
  const AReadString: String);
begin
  FID := AID;
  FReadString := AReadString;
  FPos := 0;
  FChar := #0;
end;

{ TMemoryBlock }

procedure TMemoryBlock.Reset;
var i: Integer;
begin
  for i := Low(FChunkTable) to High(FChunkTable) do
    SetLength(FChunkTable[i],0);
  SetLength(FChunkTable,0);
  FSize := 0;
end;

function TMemoryBlock.ReadFromFile(const AFileName: String): Boolean;
var fhandle: THandle;
    fread: Integer;
begin
  Reset;
  if FileExists(AFileName) then
    begin
      FSize := 0;
      fhandle := FileOpen( AFileName, fmOpenRead );
      repeat
        SetLength(FChunkTable,Length(FChunkTable)+1);
        SetLength(FChunkTable[High(FChunkTable)],FChunkSize);
        fread := FileRead(fhandle,FChunkTable[High(FChunkTable)][0],FChunkSize);
        if fread < 0 then
          begin
            Reset;
            FileClose(fhandle);
            Exit(false);
          end;
        if fread < FChunkSize then
          begin
            SetLength(FChunkTable[High(FChunkTable)],fread);
            if fread <= 0 then
              SetLength(FChunkTable,Length(FChunkTable)-1);
          end;
        Inc(FSize,fread);
      until fread < FChunkSize;
      Result := true;
    end
  else
    Result := false;
end;

function TMemoryBlock.Size: StreamInt;
begin
  Result := FSize;
end;

function TMemoryBlock.Read(index: StreamInt): Char;
begin
  Result := FChunkTable[ index div FChunkSize ][ index mod FChunkSize ];
end;

constructor TMemoryBlock.Create(AChunkSize: Integer);
begin
  SetLength(FChunkTable,0);
  FChunkSize := AChunkSize;
  FSize := 0;
end;

destructor TMemoryBlock.Destroy;
begin
  Reset;
  inherited Destroy;
end;

{ TMemoryBlockReaderStream }

function TMemoryBlockReaderStream.CurrentChar: Char;
begin
  Result := FChar;
end;

function TMemoryBlockReaderStream.NextChar: Boolean;
begin
  if FSRead < FMemBlock.Size then
    begin
      FChar := FMemBlock.Read(FSRead);
      Inc(FSRead,1);
      Result := true;
    end
  else
    Result := false;
end;

function TMemoryBlockReaderStream.StreamID: String;
begin
  Result := FID;
end;

constructor TMemoryBlockReaderStream.Create(const AID: String;
  AMemBlock: TMemoryBlock; ABlockOwner: Boolean);
begin
  FID := AID;
  FMemBlock := AMemBlock;
  FBlockOwner := ABlockOwner;
  FSRead := 1;
end;

destructor TMemoryBlockReaderStream.Destroy;
begin
  if FBlockOwner then
    FMemBlock.Free;
  inherited Destroy;
end;

{ TScannerStream }

function TScannerStream.StreamID: String;
begin
  Result := FCharStream.StreamID;
end;

function TScannerStream.LookAhead(n: Integer): Boolean;
begin
  Result := (0<=n) and (n<=FLAFillCounter);
end;

function TScannerStream.LookAheadChar(n: Integer): Char;
begin
  if not LookAhead(n) then
    raise ERangeError.Create('LA not in buffered Range');
  Result := FRBuffer[(FCIndex+n) mod Length(FRBuffer)];
end;

function TScannerStream.FastForward(n: Integer): Boolean;
begin
  while (n > 0) and NextChar do
    Dec(n,1);
  Result := n <= 0;
end;

function TScannerStream.NextChar: Boolean;
begin
  Result := FLAIndex <> FCIndex;
  FNewLineSkip := false;
  if Result then
    begin
      {Set Current Char}
      Inc(FCIndex,1);
      if FCIndex >= Length(FRBuffer) then
        FCIndex := 0;
      {Refill LookAhead Buffer}
      if FCharStream.NextChar then
        begin
          Inc(FLAIndex,1);
          if FLAIndex >= Length(FRBuffer) then
            FLAIndex := 0;
          FRBuffer[FLAIndex] := FCharStream.CurrentChar;
        end
      else
        Dec(FLAFillCounter,1);
      {Check NewLine State}
      Inc(FCurrentPos,1);
      if FNewLineMode <> nlmNone then
        begin
          Inc(FCurrentCol,1);
          if (FNewLineHit <> nlmNone) and
             ((FNewLineHit = FNewLineMode) or
              ((FNewLineMode = nlmMixed) and
               ((FNewLineHit <> nlmMac) or
                (Byte(CurrentChar) <> 10)))) then
          begin
            {-> either NewLineHit = Mode or
                Mixed Mode and NO crlf (where lf is the current char) which will
                be replaced by nlmWindows and hit on next NextChar}
            Inc(FCurrentLine,1);
            FNewLineSkip := true;
            FCurrentCol := 1;
          end;
        end;
      {Update NewLine}
      case Byte(CurrentChar) of
        10:
          begin
            if FNewLineHit <> nlmMac then
              FNewLineHit := nlmUnix
            else
              FNewLineHit := nlmWindows;
          end;
        13:
          begin
            FNewLineHit := nlmMac;
          end
        else
          FNewLineHit := nlmNone;
      end;
    end;
end;

function TScannerStream.CurrentChar: Char;
begin
  Result := FRBuffer[FCIndex];
end;

constructor TScannerStream.Create(ALASize: Integer;
  ACharStream: TCharReaderStream; AStreamOwner: Boolean);
begin
  if ALASize < 1 then
    ALASize := 1;
  SetLength(FRBuffer,ALASize+1);
  FCharStream := ACharStream;
  FStreamOwner := AStreamOwner;
  FLAIndex := -1;
  FCIndex := -1;
  FLAFillCounter := 0;
  FNewLineMode := nlmMixed;
  FNewLineHit := nlmNone;
  FCurrentLine := 1;
  FCurrentCol := 0;
  FCurrentPos := 0;
  FNewLineSkip := false;
  {prebuffer}
  while (FLAFillCounter < ALASize) and
        FCharStream.NextChar do
    begin
      Inc(FLAIndex,1);
      Inc(FLAFillCounter,1);
      FRBuffer[FLAIndex] := FCharStream.CurrentChar;
    end;
end;

destructor TScannerStream.Destroy;
begin
  SetLength(FRBuffer,0);
  if FStreamOwner then
    FCharStream.Free;
  inherited Destroy;
end;

{ TFileReaderStream }

function TFileReaderStream.BufferNext: Boolean;
begin
  FBufSize := FileRead( FFileHandle, FBuffer[ 0 ], FBufMax );
  if FBufSize <= 0 then
  begin
    if FOpen then
    begin
      FileClose( FFileHandle );
      FOpen := false;
    end;
    Result := false;
  end else Result := true;
  FBufPos := 0;
end;

function TFileReaderStream.NextChar: Boolean;
begin
  if FOpen then
  begin
    Inc( FBufPos, 1 );
    Result := ( FBufPos < FBufSize ) or BufferNext;
  end else Result := false;
end;

function TFileReaderStream.CurrentChar: Char;
begin
  Result := FBuffer[ FBufPos ];
end;

function TFileReaderStream.StreamID: String;
begin
  Result := FID;
end;

constructor TFileReaderStream.Create(const AID, AFileName: String; ABufMax: Integer);
begin
  inherited Create;
  FID := AID;
  FFileName := AFileName;
  SetLength( FBuffer, ABufMax );
  FBufMax := ABufMax;
  FBufSize := 0;
  FBufPos := 0;
  if FileExists( AFileName ) then
    begin
      FFileHandle := FileOpen( AFileName, fmOpenRead );
      BufferNext;
      FOpen := FBufSize >= 0;
      FBufPos := -1;
    end
  else
    FOpen := false;
end;

destructor TFileReaderStream.Destroy;
begin
  SetLength( FBuffer, 0 );
  if FOpen then
    FileClose( FFileHandle );
  inherited Destroy;
end;


end.

