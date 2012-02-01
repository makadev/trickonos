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
  SysUtils, commontl, ucfs, ccbase, csyms, sstreams, eomsg;

const
  {DOC>> Maximum Token Size (Scanner Internal Buffer Limit), Scanner will use
         a String of 4*TmpBuffer size for internal buffering}
  Max_ScanLimit = CL_MAX_TmpBufferSize div 4;
  //DOC>> Maximum Intro/Outro/Comment Marker Length (needed for lookahead)
  Max_IntroOutro = 24;
  //DOC>> Withespaces to be ignored in in-code: tab/newline/linefeed/space
  C_WhiteSpace = [9,10,13,32];

(*******************************************************************************
 * Streaming Control
 ******************************************************************************)

{DOC>> Sets Intro/Outro Marker Sequence for scanning Templates.
       intro/outroMark will be copied.}
procedure cscan_setcodemarker( introMark, outroMark: PUCFS32String ); inline;
{DOC>> Sets Intro/Outro Sequence for scanning in code multiline Comments.
       intro/outro will be copied.}
procedure cscan_seteccmarker( intro, outro: PUCFS32String ); inline;
{DOC>> Sets Intro Sequence for scanning in code single line Comments.
       starter will be copied.}
procedure cscan_setlcmarker( starter: PUCFS32String ); inline;

//DOC>> return current code intro sequence (no copy)
function cscan_codeintro: PUCFS32String; inline;
//DOC>> return current code outro sequence (no copy)
function cscan_codeoutro: PUCFS32String; inline;

{DOC>> scan current file, return next token/eos/error symbol (scanrecord).
       scanner/compiler subsystem must be initialized before use.}
function cscan_scan: TScanRecord;

//DOC>> get streamers id
function cscan_streamid: PUCFS32String; inline;
//DOC>> get streamers position in stream (line)
function cscan_line: StreamInt; inline;

(*******************************************************************************
 * Parser Interface
 ******************************************************************************)

{DOC>> Read Next Token, @returns @false if there is no next token
       (scanner returned end of stream), creates critical on scanner error
       if @true, then CurrentToken can be used to access this Token}
function NextToken( AReleaseLastTok: Boolean = false ): Boolean;
{DOC>> Access last read token}
function CurrentToken: TScanRecord;
{DOC>> Expect the current token to be a specific symbol.
       @returns @false if noterminate=true and current token is not given
       symbol. if noterminate=false (default) this produces a critical.
       returns @true if currentoken is symbol s}
function ExpectSym( s: TSMESymbol; noterminate: Boolean = false ): Boolean;
{DOC>> Expect the current token to be a specific symbol set.
       @returns @false if noterminate=true and current token is not in given
       symbol set. if noterminate=false (default) this produces a critical.
       returns @true if currentoken is in symbol set s.}
function ExpectSymSet( s: TSMESSymSet; noterminate: Boolean = false ): Boolean;

(*******************************************************************************
 * Init&Fin (&Stuff)
 ******************************************************************************)

{DOC>> Initialize Scanner for file fullname, set id to shortname (will be seen)
       on error/output and the mode. Usemode=@true scans the file in code
       mode (only code allowed), Usemode=@false scans the file in template
       mode and allows code intro/outro sequences to switch between output
       data and code.
       shortname/fullname are not copied.}
procedure InitScanner( usemode: Boolean; shortname, fullname: PUCFS32String );
procedure InitScanner( usemode: Boolean; const shortname, fullname: String ); deprecated;
{DOC>> Release Scanner (internal buffers and scan stream).}
procedure ReleaseScanner;

//DOC>> switch into debug mode, this pumps out symbols read by nexttoken
procedure SetScanDebugMode( switch: Boolean );

implementation

(*******************************************************************************
 * Parser Interface
 ******************************************************************************)

var
 scandebug: Boolean;

procedure SetScanDebugMode( switch: Boolean );
begin
  scandebug := switch;
end;

(******************************************************************************)

var
  {pstate}
  parsetk: TScanRecord;

function NextToken(AReleaseLastTok: Boolean): Boolean;
var
  sout: String;
begin
  if AReleaseLastTok and
     Assigned(parsetk) then
    parsetk.Free;
  parsetk := cscan_scan;
  Result := not (parsetk.TokenType in [SMES_ERROR,SMES_EOS]);
  if scandebug and debug_mode then
    begin
      WriteStr(sout,'TOKEN <',parsetk.TokenType,',"',ucfs_to_utf8string(parsetk.Pattern),'">');
      put_debug_for(parsetk.Line,parsetk.Column,ucfs_to_utf8string(cscan_streamid),sout);
    end;
  if (not Result) and
     (parsetk.TokenType = SMES_ERROR) then
    put_critical_for(parsetk.Line,parsetk.Column,ucfs_to_utf8string(cscan_streamid),
      'Unexpected Scanner Error: '+ucfs_to_utf8string(parsetk.Pattern));
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
        put_error_for(parsetk.Line,parsetk.Column,ucfs_to_utf8string(cscan_streamid),e)
      else
        put_critical_for(parsetk.Line,parsetk.Column,ucfs_to_utf8string(cscan_streamid),e)
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
        put_error_for(parsetk.Line,parsetk.Column,ucfs_to_utf8string(cscan_streamid),e)
      else
        put_critical_for(parsetk.Line,parsetk.Column,ucfs_to_utf8string(cscan_streamid),e)
    end;
end;

(*******************************************************************************
 * Streaming Control
 ******************************************************************************)

var
  {scanner marker}
  IntroMarker: PUCFS32String;
  OutroMarker: PUCFS32String;

procedure cscan_setcodemarker(introMark, outroMark: PUCFS32String);
begin
  if (ucfs_length(introMark) <= 0) or
     (ucfs_length(outroMark) <= 0) or
     (ucfs_length(introMark) > Max_IntroOutro) or
     (ucfs_length(outroMark) > Max_IntroOutro) then
    put_critical('Intro/Outro Marker Length Missmatch');
  ucfs_release(IntroMarker);
  ucfs_release(OutroMarker);
  IntroMarker := ucfs_incref(introMark);
  OutroMarker := ucfs_incref(outroMark);
end;

function cscan_codeintro: PUCFS32String;
begin
  Result := IntroMarker;
end;

function cscan_codeoutro: PUCFS32String;
begin
  Result := OutroMarker;
end;

(******************************************************************************)

var
  {in code comment marker}
  ECCIntro: PUCFS32String;
  ECCOutro: PUCFS32String;
  LCMark: PUCFS32String;

procedure cscan_seteccmarker(intro, outro: PUCFS32String);
begin
  if (ucfs_length(intro) > Max_IntroOutro) or
     (ucfs_length(outro) > Max_IntroOutro) then
    put_critical('Encapsulated Comment Marker Length Missmatch');
  ucfs_release(ECCIntro);
  ucfs_release(ECCOutro);
  ECCIntro := ucfs_incref(intro);
  ECCOutro := ucfs_incref(outro);
end;

procedure cscan_setlcmarker(starter: PUCFS32String);
begin
  if ucfs_length(starter) > Max_IntroOutro then
    put_critical('Line Comment Marker Length Missmatch');
  ucfs_release(LCMark);
  LCMark := ucfs_incref(starter);
end;

(******************************************************************************)

var
  {streamer}
  scanstream: TScannerStream;

function cscan_streamid: PUCFS32String;
begin
  Result := scanstream.StreamID;
end;

function cscan_line: StreamInt;
begin
  Result := scanstream.CurrentLine;
end;

(******************************************************************************)

var
  {internal token buffer}
  InternalBuffer: PUCFS32String;
  IBPtr: VMInt;

function BufferCopy: PUCFS32String;
begin
  if IBPtr > 0 then
    Result := ucfs_cpy(InternalBuffer,1,IBPtr)
  else
    Result := nil;
end;

procedure BufferChar( uc: TUCFS32Char );
begin
  Inc(IBPtr,1);
  if IBPtr <= Max_ScanLimit then
    begin
      if uc < 0 then
        put_critical_for(scanstream.CurrentLine,
                         scanstream.CurrentCol,
                         ucfs_to_utf8string(scanstream.StreamID),
                         'Utf8 Decode Fault');
      ucfs_setc(InternalBuffer,IBPtr,uc);
    end
  else
    put_critical_for(scanstream.CurrentLine,
                     scanstream.CurrentCol,
                     ucfs_to_utf8string(scanstream.StreamID),
                     'Token Size Limit reached (internal buffer limit)');
end;

function BufferdLen: VMInt;
begin
  Result := IBPtr;
end;

procedure BufferReset;
begin
  IBPtr := 0;
end;

procedure BufferInit;
begin
  ASSERT(InternalBuffer = nil);
  InternalBuffer := ucfs_alloc(Max_ScanLimit,4);
  BufferReset;
end;

procedure BufferRelease;
begin
  ucfs_release(InternalBuffer);
  InternalBuffer := nil;
end;

(******************************************************************************)

var
  CChar, LAChar: TUCFS32Char;

function LookAhead: Boolean; inline;
begin
  LookAhead := scanstream.LookAhead(1);
  if LookAhead then
    begin
      LAChar := scanstream.LookAheadChar(1);
      if LAChar < 0 then
        put_critical_for(scanstream.CurrentLine,
                         scanstream.CurrentCol,
                         ucfs_to_utf8string(scanstream.StreamID),
                         'UTF8 Sequence Error in Lookahead(1)');
    end;
end;

function NextChar: Boolean; inline;
begin
  NextChar := scanstream.NextChar;
  if NextChar then
    begin
      CChar := scanstream.CurrentChar;
      if CChar < 0 then
        put_critical_for(scanstream.CurrentLine,
                         scanstream.CurrentCol,
                         ucfs_to_utf8string(scanstream.StreamID),
                         'UTF8 Sequence Error');
    end;
end;

procedure PatternAddCurrent; inline;
begin
  BufferChar(CChar);
end;

function ScanToken: TScanRecord;
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
            Result.PatternSetS('Unexpected End of Stream');
            Result.Line := scanstream.CurrentLine;
            Result.Column := scanstream.CurrentCol;
            Exit(Result);
          end;
    end;

  Result.Line := scanstream.CurrentLine;
  Result.Column := scanstream.CurrentCol;
  BufferReset;

  if CChar < 0 then
    put_critical_for(scanstream.CurrentLine,
                     scanstream.CurrentCol,
                     ucfs_to_utf8string(scanstream.StreamID),
                     'UTF8 Sequence Error');

  {check type}
  case CChar of
    {ID a-zA-Z_ (a-zA-Z0-9_)* }
    C_Char_Underscore,
    C_Char_Small_a..C_Char_Small_z,
    C_Char_Capital_a..C_Char_Capital_z:
      begin
        CChar := ucfs_u32c_upcase(CChar);
        PatternAddCurrent;
        while LookAhead and (LAChar in C_Set_ID_Follow) do
          begin
            NextChar;
            CChar := ucfs_u32c_upcase(CChar);
            PatternAddCurrent;
          end;
        {$INFO should be unified with info in ident}
        Result.PatternSetU(BufferCopy,false);
        Result.TokenType := IdToSym(Result.Pattern);
      end;
    {INT [$,%,&] (:digset:)+ [q[:digset]]}
    C_Char_Digit_Zero..C_Char_Digit_Nine,
    C_Char_Dollar,
    C_Char_Percent,
    C_Char_Ampersand:
      begin
        Result.TokenType := SMES_Error;
        case CChar of
          C_Char_Dollar:
            begin
              if (not LookAhead) or (not(LAChar in C_Set_HexNum_Follow)) then
                begin
                  Result.PatternSetS('Expected Hexadecimal Digit');
                  Exit(Result);
                end;
              Result.TokenType := SMES_IntHex;
              while LookAhead and (LAChar in C_Set_HexNum_Follow) do
              begin
                NextChar;
                PatternAddCurrent;
                if LookAhead and (LAChar = C_Char_Underscore) then
                  begin
                    NextChar;
                    if LookAhead and  (LAChar = C_Char_BackSlash) then
                      begin
                        NextChar;
                        while LookAhead and
                              (LAChar in C_WhiteSpace) do
                          NextChar;
                        if (not LookAhead) or
                           (not((LAChar in C_Set_HexNum_Follow) or
                                (LAChar = C_Char_Small_q))) then
                          begin
                            Result.TokenType := SMES_ERROR;
                            Result.PatternSetS('Unexpected End of Multiline Number');
                            Exit(Result);
                          end;
                      end;
                  end;
              end;
            end;
          C_Char_Ampersand:
            begin
              if (not LookAhead) or (not(LAChar in C_Set_OctNum_Follow)) then
                begin
                  Result.PatternSetS('Expected Octal Digit');
                  Exit(Result);
                end;
              Result.TokenType := SMES_IntOct;
              while LookAhead and (LAChar in C_Set_OctNum_Follow) do
              begin
                NextChar;
                PatternAddCurrent;
                if LookAhead and (LAChar = C_Char_Underscore) then
                  begin
                    NextChar;
                    if LookAhead and  (LAChar = C_Char_BackSlash) then
                      begin
                        NextChar;
                        while LookAhead and
                              (LAChar in C_WhiteSpace) do
                          NextChar;
                        if (not LookAhead) or
                           (not((LAChar in C_Set_OctNum_Follow) or
                                (LAChar = C_Char_Small_q))) then
                          begin
                            Result.TokenType := SMES_ERROR;
                            Result.PatternSetS('Unexpected End of Multiline Number');
                            Exit(Result);
                          end;
                      end;
                  end;
              end;
            end;
          C_Char_Percent:
            begin
              if (not LookAhead) or (not(LAChar in C_Set_BinNum_Follow)) then
                begin
                  Result.PatternSetS('Expected Binary Digit');
                  Exit(Result);
                end;
              Result.TokenType := SMES_IntBin;
              while LookAhead and (LAChar in C_Set_BinNum_Follow) do
              begin
                NextChar;
                PatternAddCurrent;
                if LookAhead and (LAChar = C_Char_Underscore) then
                  begin
                    NextChar;
                    if LookAhead and  (LAChar = C_Char_BackSlash) then
                      begin
                        NextChar;
                        while LookAhead and
                              (LAChar in C_WhiteSpace) do
                          NextChar;
                        if (not LookAhead) or
                           (not((LAChar in C_Set_BinNum_Follow) or
                                (LAChar = C_Char_Small_q))) then
                          begin
                            Result.TokenType := SMES_ERROR;
                            Result.PatternSetS('Unexpected End of Multiline Number');
                            Exit(Result);
                          end;
                      end;
                  end;
              end;
            end
          else
            begin
              Result.TokenType := SMES_Int;
              PatternAddCurrent;
              while LookAhead and (LAChar in C_Set_Num_Follow) do
              begin
                NextChar;
                PatternAddCurrent;
                if LookAhead and (LAChar = C_Char_Underscore) then
                  begin
                    NextChar;
                    if LookAhead and  (LAChar = C_Char_BackSlash) then
                      begin
                        NextChar;
                        while LookAhead and
                              (LAChar in C_WhiteSpace) do
                          NextChar;
                        if (not LookAhead) or
                           (not((LAChar in C_Set_Num_Follow) or
                                (LAChar = C_Char_Small_q))) then
                          begin
                            Result.TokenType := SMES_ERROR;
                            Result.PatternSetS('Unexpected End of Multiline Number');
                            Exit(Result);
                          end;
                      end;
                  end;
              end;
            end;
        end;
        if Result.TokenType <> SMES_ERROR then
          begin
            if LookAhead and
               (LAChar = C_Char_Small_q) then
              begin
                NextChar;
                PatternAddCurrent;
                if LookAhead and
                   (LAChar in C_Set_Num_Follow) then
                  begin
                    while LookAhead and (LAChar in C_Set_Num_Follow) do
                    begin
                      NextChar;
                      PatternAddCurrent;
                    end;
                  end
                else
                  begin
                    Result.TokenType := SMES_ERROR;
                    Result.PatternSetS('Expected Decimal Q Number');
                    Exit(Result);
                  end;
              end;
          end;
        Result.PatternSetU(BufferCopy,false);
      end;
    {STRING_Q}
    C_Char_SQuote:
      begin
        Result.TokenType := SMES_Error;
        if not LookAhead then
          begin
            Result.PatternSetS('Unexpected End of Stream (broken quoted String)');
            Exit(Result);
          end;
        repeat
          {scan inner chars}
          while LookAhead and (LAChar <> C_Char_SQuote) do
          begin
            NextChar;
            PatternAddCurrent;
          end;
          {expect end}
          if LookAhead and (LAChar = C_Char_SQuote) then
            NextChar
          else
            begin
              Result.PatternSetS('Unexpected end of Stream (broken quoted String)');
              Exit(Result);
            end;
          {check for double "escape"}
          if LookAhead and (LAChar = C_Char_SQuote) then
            begin
              PatternAddCurrent;
              NextChar;
            end
          else
            Result.TokenType := SMES_String;
        until Result.TokenType = SMES_String;
        Result.PatternSetU(BufferCopy,false);
      end;
    {STRING_E}
    C_Char_DQuote:
      begin
        Result.TokenType := SMES_Error;
        if not LookAhead then
          begin
            Result.PatternSetS('Unexpected End of Stream (broken escaped String)');
            Exit(Result);
          end;
        repeat
          {scan inner chars}
          while LookAhead and
                (LAChar <> C_Char_DQuote) and
                (LAChar <> C_Char_BackSlash) do
          begin
            NextChar;
            PatternAddCurrent;
          end;
          {expect end or escape}
          if LookAhead and (LAChar = C_Char_BackSlash) then
            begin
              // load '\' which is currently lookahead
              NextChar;
              // load escape symbol, may not exist if escape was at
              // end of file -> check nextchar
              if not NextChar then
                begin
                  Result.PatternSetS('Unexpected end of Stream (broken escaped String)');
                  Exit(Result);
                end;
              {convert known escapes}
              case CChar of
                C_Char_Small_n: CChar := 10;
                C_Char_Small_t: CChar := 9;
                C_Char_Small_l: CChar := 13;
                C_Char_DQuote, C_Char_BackSlash: {no need for conversion};
                else
                  begin
                    {hmm, unknown escape -> error}
                    Result.PatternSetS('Unknown Escape Sequence');
                    Exit(Result);
                  end;
              end;
              {add cchar to pattern}
              PatternAddCurrent;
            end
          else
            begin
              if LookAhead and (LAChar = C_Char_DQuote) then
                begin
                  {hit end of string, set end and break condition}
                  NextChar;
                  Result.TokenType := SMES_String;
                end
              else
                begin
                  {neither escape nor end of string -> unexpected end (no lookahead)}
                  Result.PatternSetS('Unexpected end of Stream (broken escaped String)');
                  Exit(Result);
                end;
            end;
        until Result.TokenType = SMES_String;
        Result.PatternSetU(BufferCopy,false);
      end;
    {MISC/MIXED}
    C_Char_Plus: Result.TokenType := SMES_Plus;
    C_Char_Minus: Result.TokenType := SMES_Minus;
    C_Char_Star: Result.TokenType := SMES_Star;
    C_Char_Dot: Result.TokenType := SMES_Dot;
    C_Char_Comma: Result.TokenType := SMES_Comma;
    C_Char_SColon: Result.TokenType := SMES_SColon;
    C_Char_Colon:
      begin
        Result.TokenType := SMES_Colon;
        if LookAhead and (LAChar = C_Char_Equ) then
          begin
            Result.TokenType := SMES_Assign;
            NextChar;
          end;
      end;
    C_Char_LRPar: Result.TokenType := SMES_ORound;
    C_Char_RRPar: Result.TokenType := SMES_CRound;
    C_Char_LSPar: Result.TokenType := SMES_OSquare;
    C_Char_RSPar: Result.TokenType := SMES_CSquare;
    C_Char_LCPar: Result.TokenType := SMES_OCurly;
    C_Char_RCPar: Result.TokenType := SMES_CCurly;
    C_Char_LAngel:
      begin
        Result.TokenType := SMES_OAngel;
        if LookAhead and (LAChar = C_Char_Equ) then
          begin
            Result.TokenType := SMES_LE;
            NextChar;
          end
        else if LookAhead and (LAChar = C_Char_RAngel) then
          begin
            Result.TokenType := SMES_NEq;
            NextChar;
          end
        else if LookAhead and (LAChar = C_Char_LAngel) then
          begin
            Result.TokenType := SMES_SHL;
            NextChar;
            if LookAhead and (LAChar = C_Char_LAngel) then
              begin
                Result.TokenType := SMES_ROL;
                NextChar;
              end;
          end;
      end;
    C_Char_RAngel:
      begin
        Result.TokenType := SMES_CAngel;
        if LookAhead and (LAChar = C_Char_Equ) then
          begin
            Result.TokenType := SMES_GE;
            NextChar;
          end
        else if LookAhead and (LAChar = C_Char_RAngel) then
          begin
            Result.TokenType := SMES_SHR;
            NextChar;
            if LookAhead and (LAChar = C_Char_RAngel) then
              begin
                Result.TokenType := SMES_ROR;
                NextChar;
              end;
          end;
      end;
    C_Char_Equ: Result.TokenType := SMES_Eq;
    else
      begin
        Result.TokenType := SMES_Error;
        Result.PatternSetS('Expected Character');
        Exit(Result);
      end;
  end;
end;

(******************************************************************************)

var
  {scanner infos}
  SMESIgnoreSet: TSMESSymSet;
  isusemode: Boolean;
  scanincode: Boolean;
  preloadedchar: Boolean;

procedure InitScanner(usemode: Boolean; shortname, fullname: PUCFS32String);
var fstream: TFileReaderStream;
begin
  fstream := TFileReaderStream.Create(shortname,fullname);
  scanstream := TScannerStream.Create(Max_IntroOutro+1,fstream,true);
  isusemode := usemode;
  scanincode := usemode;
  preloadedchar := false;
  parsetk := nil;
  SMESIgnoreSet := [SMES_CODE_INTRO,SMES_CODE_OUTRO];
  BufferInit;
end;

procedure InitScanner(usemode: Boolean; const shortname, fullname: String);
var tmps,tmpf: PUCFS32String;
begin
  tmps := ucfs_utf8us(shortname);
  tmpf := ucfs_utf8us(fullname);
  InitScanner(usemode,tmps,tmpf);
  ucfs_release(tmps);
  ucfs_release(tmpf);
end;

procedure ReleaseScanner;
begin
  if Assigned(scanstream) then
    begin
      BufferRelease;
      scanstream.Free;
      scanstream := nil;
    end;
end;

function MarkerCheck( mark: PUCFS32String; ff: Boolean ): Boolean;
{utilise the lookahead and check if we are at a Marker Sequence}
var i: VMInt;
begin
  Result := false;
  if scanstream.CurrentChar = ucfs_getc(mark,1) then
    begin
      i := 2;
      while (i <= ucfs_length(mark)) and
            scanstream.LookAhead(i-1) and
            (scanstream.LookAheadChar(i-1) = ucfs_getc(mark,i)) do
        Inc(i,1);
      if i > ucfs_length(mark) then
        begin
          Result := true;
          {FF to Last Char of Marker}
          if ff then
            if not scanstream.FastForward(ucfs_length(mark)-1) then
              put_internalerror(201110202);
        end;
    end;
end;


function cscan_scan: TScanRecord;
var l,c: StreamInt;
    iseos, iscomment: Boolean;

begin
  Result := nil;

  repeat
    if Assigned(Result) then
      begin
        {unused/ignored token -> free mem since it never enters parser stage}
        Result.Free;
        Result := nil;
      end;

    if (not preloadedchar) and
       (not scanstream.NextChar) then
      begin
        Result := TScanRecord.Create;
        Result.TokenType := SMES_EOS;
        Result.PatternSetS('EOS');
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
                              ucfs_to_utf8string(scanstream.StreamID),
                              'Missing Outro Sequence on EOS, Inserted.');
            if not isusemode then
              begin
                {emit missing outro}
                scanincode := false;
                Result := TScanRecord.Create;
                Result.TokenType := SMES_CODE_OUTRO;
                Result.Line := l;
                Result.Column := c;
              end
            else
              begin
                {in usemode, emit eos}
                Result := TScanRecord.Create;
                Result.TokenType := SMES_EOS;
                Result.PatternSetS('EOS');
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
        BufferReset;
        iseos := false;
        while (not iseos) and
              ((scanstream.CurrentChar <> ucfs_getc(IntroMarker,1)) or
               (not (MarkerCheck(IntroMarker,false)))) and
              (l = scanstream.CurrentLine) do
          begin
            BufferChar(scanstream.CurrentChar);
            iseos := not scanstream.NextChar;
          end;

        if BufferdLen > 0 then
          begin
            {hit line switch, eos or intromarker}
            Result := TScanRecord.Create;
            Result.TokenType := SMES_PUT;
            Result.PatternSetU(BufferCopy,false);
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
            Result.Line := l;
            Result.Column := c;
            scanincode := true;
          end;
      end;

    ASSERT(Assigned(Result));

  until not(Result.TokenType in SMESIgnoreSet);
end;

initialization
{$ifdef debug}
  scandebug := false;
{$endif}
  scanstream := nil;
  IntroMarker := ucfs_utf8us('{?');
  OutroMarker := ucfs_utf8us('?}');
  ECCIntro := ucfs_utf8us('/*');
  ECCOutro := ucfs_utf8us('*/');
  LCMark := ucfs_utf8us('//');
  InternalBuffer := nil;

finalization
  ucfs_release(IntroMarker);
  ucfs_release(OutroMarker);
  ucfs_release(ECCIntro);
  ucfs_release(ECCOutro);
  ucfs_release(LCMark);

end.

