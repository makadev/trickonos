{   Unit for scanner and basic scan interface routines

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

unit cscan;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, commontl, ccbase, csyms, sstreams, eomsg;

const
  Max_IntroOutro = 24;
  C_WhiteSpace = [#9,#10,#13,#32];

// Streaming interface

procedure StreamScannerSetMarker( const introMark, outroMark: String ); inline;
procedure StreamScannerSetECComment( const intro, outro: String ); inline;
procedure StreamScannerSetLComment( const starter: String ); inline;

function CurrentStreamScanMarkerIntro: String; inline;
function CurrentStreamScanMarkerOutro: String; inline;

function StreamScan: TScanRecord;
function CurrentStreamID: String; inline;
function CurrentStreamLine: StreamInt; inline;

// Parser Lookahead

function NextToken( AReleaseLastTok: Boolean = false ): Boolean;
function CurrentToken: TScanRecord;
function ExpectSym( s: TSMESymbol; noterminate: Boolean = false ): Boolean;
function ExpectSymSet( s: TSMESSymSet; noterminate: Boolean = false ): Boolean;

// Init&Fin

procedure InitScanner( usemode: Boolean; const shortname, fullname: String );
procedure ReleaseScanner;

procedure SetScanDebugMode( switch: Boolean );

implementation

var
  {scanner marker}
  IntroMarker: String;
  OutroMarker: String;
  {in code comment marker}
  ECCIntro: String;
  ECCOutro: String;
  LCMark: String;
  {streamer}
  scanstream: TScannerStream;
  {scanner infos}
  SMESIgnoreSet: TSMESSymSet;
  isusemode: Boolean;
  scanincode: Boolean;
  preloadedchar: Boolean;
  {pstate}
  parsetk: TScanRecord;
  scandebug: Boolean;

procedure SetScanDebugMode( switch: Boolean );
begin
  scandebug := switch;
end;

function CurrentStreamID: String;
begin
  Result := scanstream.StreamID;
end;

function CurrentStreamLine: StreamInt;
begin
  Result := scanstream.CurrentLine;
end;

procedure StreamScannerSetMarker(const introMark, outroMark: String);
begin
  if (Length(introMark) <= 0) or
     (Length(outroMark) <= 0) or
     (Length(introMark) > Max_IntroOutro) or
     (Length(outroMark) > Max_IntroOutro) then
    put_critical('Intro/Outro Marker Length Missmatch');
  IntroMarker := introMark;
  OutroMarker := outroMark;
end;

procedure StreamScannerSetECComment(const intro, outro: String);
begin
  if (Length(intro) > Max_IntroOutro) or
     (Length(outro) > Max_IntroOutro) then
    put_critical('Encapsulated Comment Marker Length Missmatch');
  ECCIntro := intro;
  ECCOutro := outro;
end;

procedure StreamScannerSetLComment(const starter: String);
begin
  if Length(starter) > Max_IntroOutro then
    put_critical('Line Comment Marker Length Missmatch');
  LCMark := starter;
end;

function NextToken(AReleaseLastTok: Boolean): Boolean;
var
  sout: String;
begin
  if AReleaseLastTok then
    parsetk.Free;
  parsetk := StreamScan;
  Result := not (parsetk.TokenType in [SMES_ERROR,SMES_EOS]);
  if scandebug and debug_mode then
    begin
      WriteStr(sout,'TOKEN <',parsetk.TokenType,',"',parsetk.Pattern,'">');
      put_debug_for(parsetk.Line,parsetk.Column,CurrentStreamID,sout);
    end;
  if (not Result) and
     (parsetk.TokenType = SMES_ERROR) then
    put_critical_for(parsetk.Line,parsetk.Column,CurrentStreamID,'Unexpected Scanner Error: '+parsetk.Pattern);
end;

function CurrentToken: TScanRecord;
begin
  Result := parsetk;
end;

function ExpectSym(s: TSMESymbol; noterminate: Boolean = false): Boolean;
var e: String;
begin
  Result := parsetk.TokenType = s;
  if not Result then
    begin
      WriteStr(e,'Expected ',s,', Got ', parsetk.TokenType);
      if noterminate then
        put_error_for(parsetk.Line,parsetk.Column,CurrentStreamID,e)
      else
        put_critical_for(parsetk.Line,parsetk.Column,CurrentStreamID,e)
    end;
end;

function ExpectSymSet(s: TSMESSymSet; noterminate: Boolean = false): Boolean;
var e,es: String;
    i: TSMESymbol;
    sc: Boolean;
begin
  Result := parsetk.TokenType in s;
  if not Result then
    begin
      e := '';
      sc := false;
      for i := TSMESymbol(0) to S__TERMINATOR do
        if i in s then
          begin
            if sc then
              WriteStr(es,',',i)
            else
              begin
                WriteStr(es,i);
                sc := true;
              end;
            e := e + es;
          end;
      es := e;
      WriteStr(e,'Expected [',es,'], Got ', parsetk.TokenType);
      if noterminate then
        put_error_for(parsetk.Line,parsetk.Column,CurrentStreamID,e)
      else
        put_critical_for(parsetk.Line,parsetk.Column,CurrentStreamID,e)
    end;
end;

procedure InitScanner(usemode: Boolean; const shortname, fullname: String);
var fstream: TFileReaderStream;
begin
  fstream := TFileReaderStream.Create(shortname,fullname);
  scanstream := TScannerStream.Create(Max_IntroOutro+1,fstream,true);
  isusemode := usemode;
  scanincode := usemode;
  preloadedchar := false;
  SMESIgnoreSet := [SMES_CODE_INTRO,SMES_CODE_OUTRO];
end;

procedure ReleaseScanner;
begin
  if Assigned(scanstream) then
    begin
      scanstream.Free;
      scanstream := nil;
    end;
end;

function MarkerCheck( const m: String; ff: Boolean ): Boolean;
{utilise the lookahead and check if we are at a Marker Sequence}
var i: MachineInt;
begin
  Result := false;
  if scanstream.CurrentChar = m[1] then
    begin
      i := 2;
      while (i <= Length(m)) and
            scanstream.LookAhead(i-1) and
            (scanstream.LookAheadChar(i-1) = m[i]) do
        Inc(i,1);
      if i > Length(m) then
        begin
          Result := true;
          {FF to Last Char of Marker}
          if ff then
            if not scanstream.FastForward(Length(m)-1) then
              put_internalerror(201110202);
        end;
    end;
end;


function ScanToken: TScanRecord;
var CChar, LAChar: Char;
    ptbuf: String;

  function LookAhead: Boolean; inline;
  begin
    LookAhead := scanstream.LookAhead(1);
    if LookAhead then
      LAChar := scanstream.LookAheadChar(1);
  end;

  function NextChar: Boolean; inline;
  begin
    NextChar := scanstream.NextChar;
    if NextChar then
      CChar := scanstream.CurrentChar;
  end;

  procedure PatternAddCurrent; inline;
  begin
    ptbuf := ptbuf + CChar;
  end;

  procedure ResetPos; inline;
  begin
    Result.Line := scanstream.CurrentLine;
    Result.Column := scanstream.CurrentCol;
  end;

begin
  Result := TScanRecord.Create;

  CChar := scanstream.CurrentChar;

  {check whites, may happen when scanner is used from code}
  if CChar in C_WhiteSpace then
    begin
      {consume whites if existend - error on EOS}
      while (CChar in C_WhiteSpace) do
        if not NextChar then
          begin
            Result.TokenType := SMES_ERROR;
            Result.Pattern := 'Unexpected End of Stream';
            ResetPos;
            Exit(Result);
          end;
    end;

  ResetPos;
  ptbuf := '';

  {check type}
  case CChar of
    {ID}
    '_','a'..'z','A'..'Z':
      begin
        PatternAddCurrent;
        while LookAhead and (LAChar in ['a'..'z','A'..'Z','_','0'..'9']) do
          begin
            NextChar;
            PatternAddCurrent;
          end;
        Result.TokenType := IdToSym(ptbuf);
      end;
    {INT}
    '0'..'9','$','%','&':
      begin
        Result.TokenType := SMES_Error;
        case scanstream.CurrentChar of
          '$':
            begin
              if (not LookAhead) or (not(LAChar in ['a'..'f','A'..'F','0'..'9'])) then
                begin
                  Result.Pattern := 'Expected Hexadecimal Digit';
                  Exit(Result);
                end;
              Result.TokenType := SMES_IntHex;
              while LookAhead and (LAChar in ['a'..'f','A'..'F','0'..'9']) do
              begin
                NextChar;
                PatternAddCurrent;
              end;
            end;
          '&':
            begin
              if (not LookAhead) or (not(LAChar in ['0'..'7'])) then
                begin
                  Result.Pattern := 'Expected Octal Digit';
                  Exit(Result);
                end;
              Result.TokenType := SMES_IntOct;
              while LookAhead and (LAChar in ['0'..'7']) do
              begin
                NextChar;
                PatternAddCurrent;
              end;
            end;
          '%':
            begin
              if (not LookAhead) or (not(LAChar in ['0'..'1'])) then
                begin
                  Result.Pattern := 'Expected Binary Digit';
                  Exit(Result);
                end;
              Result.TokenType := SMES_IntBin;
              while LookAhead and (LAChar in ['0'..'1']) do
              begin
                NextChar;
                PatternAddCurrent;
              end;
            end
          else
            begin
              Result.TokenType := SMES_Int;
              PatternAddCurrent;
              while LookAhead and (LAChar in ['0'..'9']) do
              begin
                NextChar;
                PatternAddCurrent;
              end;
            end;
        end;
      end;
    {STRING_Q}
    '''':
      begin
        Result.TokenType := SMES_Error;
        if not LookAhead then
          begin
            Result.Pattern := 'Unexpected End of Stream (broken quoted String)';
            Exit(Result);
          end;
        repeat
          {scan inner chars}
          while LookAhead and (LAChar <> '''') do
          begin
            NextChar;
            PatternAddCurrent;
          end;
          {expect end}
          if LookAhead and (LAChar = '''') then
            NextChar
          else
            begin
              Result.Pattern := 'Unexpected end of Stream (broken quoted String)';
              Exit(Result);
            end;
          {check for double "escape"}
          if LookAhead and (LAChar = '''') then
            begin
              PatternAddCurrent;
              NextChar;
            end
          else
            Result.TokenType := SMES_String;
        until Result.TokenType = SMES_String;
      end;
    {STRING_E}
    '"':
      begin
        Result.TokenType := SMES_Error;
        if not LookAhead then
          begin
            Result.Pattern := 'Unexpected End of Stream (broken escaped String)';
            Exit(Result);
          end;
        repeat
          {scan inner chars}
          while LookAhead and
                (LAChar <> '"') and
                (LAChar <> '\') do
          begin
            NextChar;
            PatternAddCurrent;
          end;
          {expect end or escape}
          if LookAhead and (LAChar = '\') then
            begin
              // load '\' which is currently lookahead
              NextChar;
              // load escape symbol, may not exist if escape was at
              // end of file -> check nextchar
              if not NextChar then
                begin
                  Result.Pattern := 'Unexpected end of Stream (broken escaped String)';
                  Exit(Result);
                end;
              {convert known escapes}
              case CChar of
                'n': CChar := #10;
                't': CChar := #9;
                'l': CChar := #13;
                '"', '\': {no need for conversion};
                else
                  begin
                    {hmm, unknown escape -> error}
                    Result.Pattern := 'Unknown Escape Sequence';
                    Exit(Result);
                  end;
              end;
              {add cchar to pattern}
              PatternAddCurrent;
            end
          else
            begin
              if LookAhead and (LAChar = '"') then
                begin
                  {hit end of string, set end and break condition}
                  NextChar;
                  Result.TokenType := SMES_String;
                end
              else
                begin
                  {neither escape nor end of string -> unexpected end (no lookahead)}
                  Result.Pattern := 'Unexpected end of Stream (broken escaped String)';
                  Exit(Result);
                end;
            end;
        until Result.TokenType = SMES_String;
      end;
    {MISC/MIXED}
    '+':
      begin
        Result.TokenType := SMES_Plus;
        PatternAddCurrent;
      end;
    '-':
      begin
        Result.TokenType := SMES_Minus;
        PatternAddCurrent;
      end;
    '*':
      begin
        Result.TokenType := SMES_Star;
        PatternAddCurrent;
      end;
{    '/':
      begin
        Result.TokenType := SMES_Slash;
        PatternAddCurrent;
      end;                }
    '.':
      begin
        Result.TokenType := SMES_Dot;
        PatternAddCurrent;
      end;
    ',':
      begin
        Result.TokenType := SMES_Comma;
        PatternAddCurrent;
      end;
    ';':
      begin
        Result.TokenType := SMES_SColon;
        PatternAddCurrent;
      end;
    ':':
      begin
        Result.TokenType := SMES_Colon;
        PatternAddCurrent;
        if LookAhead and (LAChar = '=') then
          begin
            Result.TokenType := SMES_Assign;
            NextChar;
            PatternAddCurrent;
          end;
      end;
    '(':
      begin
        Result.TokenType := SMES_ORound;
        PatternAddCurrent;
      end;
    ')':
      begin
        Result.TokenType := SMES_CRound;
        PatternAddCurrent;
      end;
    '[':
      begin
        Result.TokenType := SMES_OSquare;
        PatternAddCurrent;
      end;
    ']':
      begin
        Result.TokenType := SMES_CSquare;
        PatternAddCurrent;
      end;
    '{':
      begin
        Result.TokenType := SMES_OCurly;
        PatternAddCurrent;
      end;
    '}':
      begin
        Result.TokenType := SMES_CCurly;
        PatternAddCurrent;
      end;
    '<':
      begin
        Result.TokenType := SMES_OAngel;
        PatternAddCurrent;
        if LookAhead and (LAChar = '=') then
          begin
            Result.TokenType := SMES_LE;
            NextChar;
            PatternAddCurrent;
          end
        else if LookAhead and (LAChar = '>') then
          begin
            Result.TokenType := SMES_NEq;
            NextChar;
            PatternAddCurrent;
          end
        else if LookAhead and (LAChar = '<') then
          begin
            Result.TokenType := SMES_SHL;
            NextChar;
            PatternAddCurrent;
            if LookAhead and (LAChar = '<') then
              begin
                Result.TokenType := SMES_ROL;
                NextChar;
                PatternAddCurrent;
              end;
          end;
      end;
    '>':
      begin
        Result.TokenType := SMES_CAngel;
        PatternAddCurrent;
        if LookAhead and (LAChar = '=') then
          begin
            Result.TokenType := SMES_GE;
            NextChar;
            PatternAddCurrent;
          end
        else if LookAhead and (LAChar = '>') then
          begin
            Result.TokenType := SMES_SHR;
            NextChar;
            PatternAddCurrent;
            if LookAhead and (LAChar = '>') then
              begin
                Result.TokenType := SMES_ROR;
                NextChar;
                PatternAddCurrent;
              end;
          end;
      end;
    '=':
      begin
        Result.TokenType := SMES_Eq;
        PatternAddCurrent;
      end;
    else
      begin
        Result.TokenType := SMES_Error;
        Result.Pattern := 'Expected Character';
        Exit(Result);
      end;
  end;

  Result.Pattern := ptbuf;
end;

function CurrentStreamScanMarkerIntro: String;
begin
  Result := IntroMarker;
end;

function CurrentStreamScanMarkerOutro: String;
begin
  Result := OutroMarker;
end;

function StreamScan: TScanRecord;
var l,c: StreamInt;
    iseos, iscomment: Boolean;
    linebuffer: String;

begin
  repeat
    Result := nil;

    if (not preloadedchar) and (not scanstream.NextChar) then
      begin
        Result := TScanRecord.Create;
        Result.TokenType := SMES_EOS;
        Result.Pattern := 'EOS';
        Result.Line := scanstream.CurrentLine;
        Result.Column := scanstream.CurrentCol;
        Exit(Result);
      end;

    if preloadedchar then
      preloadedchar := false;

    if scanincode then
      begin
        iseos := false;

        repeat
          {consume whitespaces}
          while (not iseos) and
                (scanstream.CurrentChar in C_WhiteSpace) do
            iseos := not scanstream.NextChar;

          {consume comments}
          iscomment := false;
          if not iseos then
            begin
              // check comment sequences
              if MarkerCheck(ECCIntro, true) then
                begin
                  // drop until ECCOutro
                  // drop character afterwards, since MarkerCheck fowards to
                  // the last character
                  iscomment := true;
                  while (not iseos) and
                        (not MarkerCheck(ECCOutro, true)) do
                    iseos := not scanstream.NextChar;
                  iseos := not scanstream.NextChar;
                end
              else if MarkerCheck(LCMark, true) then
                begin
                  // drop until newline
                  // skip is on Next Char after Newline,
                  // so >dont< drop the char afterwards
                  iscomment := true;
                  while (not iseos) and
                        (not scanstream.NewLineSkip) do
                    iseos := not scanstream.NextChar;
                end;
            end;
        until (not iscomment) or iseos;

        l := scanstream.CurrentLine;
        c := scanstream.CurrentCol;

        {Scan Tokens until OutroMarker is Hit}
        if (not iseos) and
           ((not MarkerCheck(OutroMarker,true)) or
            isusemode) then
          begin
            Result := ScanToken;
          end
        else
          begin
            if iseos and (not isusemode) then
              put_warning_for(scanstream.CurrentLine,
                              scanstream.CurrentCol,
                              scanstream.StreamID,
                              'Missing Outro Sequence on EOS, Inserted.');
            if not isusemode then
              begin
                {emit missing outro}
                scanincode := false;
                Result := TScanRecord.Create;
                Result.TokenType := SMES_CODE_OUTRO;
                Result.Pattern := OutroMarker;
                Result.Line := l;
                Result.Column := c;
              end
            else
              begin
                {in usemode, emit eos}
                Result := TScanRecord.Create;
                Result.TokenType := SMES_EOS;
                Result.Pattern := 'EOS';
                Result.Line := l;
                Result.Column := c;
              end;
          end;
      end
    else
      begin
        l := scanstream.CurrentLine;
        c := scanstream.CurrentCol;

        {Scan a Single Line but check for IntroMarker}
        linebuffer := '';
        iseos := false;
        while (not iseos) and
              ((scanstream.CurrentChar <> IntroMarker[1]) or
               (not (MarkerCheck(IntroMarker,false)))) and
              (l = scanstream.CurrentLine) do
          begin
            linebuffer := linebuffer + scanstream.CurrentChar;
            iseos := not scanstream.NextChar;
          end;

        if length(linebuffer) > 0 then
          begin
            {hit line switch, eos or intromarker}
            Result := TScanRecord.Create;
            Result.TokenType := SMES_PUT;
            Result.Pattern := linebuffer;
            Result.Line := l;
            Result.Column := c;
            {since nextchar is used, the next char is already loaded except
            when eos was hit. Set preloaded so scanner wont skip this char on
            next entry}
            preloadedchar := not iseos;
          end
        else
          begin
            {linebuffer empty -> eos and l<>scanstream.Currentline cant be
            since nextchar was not called so only intromarkercheck=true possible,
            and no char before, so this is the intromarker token}
            if not MarkerCheck(IntroMarker,true) then
              put_internalerror(2011112801);
            Result := TScanRecord.Create;
            Result.TokenType := SMES_CODE_INTRO;
            Result.Pattern := IntroMarker;
            Result.Line := l;
            Result.Column := c;
            scanincode := true;
          end;
      end;

    if not Assigned(Result) then
      put_internalerror(2011112802);

  until not(Result.TokenType in SMESIgnoreSet);
end;

initialization
{$ifdef debug}
  scandebug := false;
{$endif}
  scanstream := nil;
  StreamScannerSetECComment( '/*', '*/' );
  StreamScannerSetLComment( '//' );
  StreamScannerSetMarker('{?','?}');

end.

