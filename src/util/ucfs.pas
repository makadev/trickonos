{   Unit for UCS4 _compatible_ string handling

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

unit ucfs;

{$MODE OBJFPC}{$H+}

interface

uses SysUtils, commontl;

const
  C_DEFAULT_FAULT_REPLACE = '?';

  MaxUtf8Len = 6; // max length in bytes per character, 6bytes -> utf32 compat

  Utf8ByteFirstMarker: array[ 0 .. MaxUtf8Len-1 ] of Byte = (
    0,
    %11000000,
    %11100000,
    %11110000,
    %11111000,
    %11111100 
  );

  Utf8ByteFirstMask: array[ 0 .. MaxUtf8Len-1 ] of Byte = (
    %01111111,
    %00011111,
    %00001111,
    %00000111,
    %00000011,
    %00000001 
  );
  
  Utf8ByteSequenceMarker     = %10000000;
  Utf8ByteSequenceMarkerMask = %00111111;

type
  PFSUtf8Char = ^TFSUtf8Char;
  TFSUtf8Char = packed record
    len: Byte;
    cbytes: array[ 0 .. MaxUtf8Len-1 ] of Byte;
  end;
  
  TUCFS32Char = VMInt;
  
  TFSUBytes = array[ 0 .. CL_SO_MaxShortBuffer-1 ] of Byte;
  TFSUWords = array[ 0 .. CL_SO_MaxShortBuffer-1 ] of Word;
  TFSULongs = array[ 0 .. CL_SO_MaxShortBuffer-1 ] of VMInt;

  TUCFS32StringInfo = packed object
    private
      refcnt: VMWord;  // refcount
      charlen: VMWord; // length in Chars
      procedure init( cc: Byte; len: VMWord; initr: VMWord = 1 );
    public
      function getrefs: VMWord;
      function getlen: VMWord;
      function getcharsize: VMWord;
      function getbufferlen: VMWord;
  end;
  
  PUCFS32String = ^TUCFS32String;
  TUCFS32String = packed record
    info: TUCFS32StringInfo;
    case Byte of
      1: (bchars: TFSUBytes;);
      2: (wchars: TFSUWords;);
      3: (lchars: TFSULongs;);
  end;

(*******************************************************************************
 * UTF8 Character Handling/Conversion
 ******************************************************************************)

{DOC>> decode first byte of utf8 sequence, 
       returns 0=invalid, 1-6 for valid utf8}
function ucfs_firstbyte_to_charlength( fsb: Byte ): Byte;
{DOC>> check if follow byte is valid -> leading %10 sequence}
function ucfs_valid_follow( fsb: Byte ): Boolean; inline;

{DOC>> convert utf8char to 32bit char}
function ucfs_utf8c_to_u32c( pc: PFSUtf8Char ): TUCFS32Char;
{DOC>> convert 32bit char to utf8char}
procedure ucfs_u32c_to_utf8c( uc: TUCFS32Char; pc: PFSUtf8Char );

{DOC>> roundtrip conversion test - convert utf8 in 32bit in utf8 -> if utf8
       sequence doesn't match, it is not the shortest possible code and those
       invalid}
function ucfs_valid_utf8c( pc: PFSUtf8Char ): Boolean;

{DOC>> calculate utf8 sequence length in bytes needed to encode the u32char}
function ucfs_u32c_utf8len( uc: TUCFS32Char ): VMInt;
{DOC>> bytelen needed to fit uc}
function ucfs_u32c_bytelen( uc: TUCFS32Char ): VMInt; inline;

{DOC>> upcase letter (only a..z)}
function ucfs_u32c_upcase( uc: TUCFS32Char ): TUCFS32Char; inline;

{DOC>> lowercase letter (only A..Z)}
function ucfs_u32c_lowercase( uc: TUCFS32Char ): TUCFS32Char; inline;

(*******************************************************************************
 * UCF Strings (basics)
 *   RULE 1: 0 byte string = nil
 *   RULE 2: bare code, (only few checks, mostly asserts - so check stuff)
 ******************************************************************************)

{DOC>> ucfs_setc_check + cow_check }
procedure ucfs_setc( var ps: PUCFS32String; p: VMInt; uc: TUCFS32Char );

{DOC>> get character at position p in ps
       -> ps must be <> nil (nonempty)}
function ucfs_getc( ps: PUCFS32String; p: VMInt ): TUCFS32Char;

{DOC>> get length of strings in characters}
function ucfs_length( ps: PUCFS32String ): VMInt; inline;
{DOC>> get byte length (length for 1 character in bytes)}
function ucfs_charsize( ps: PUCFS32String ): VMInt; inline;

{DOC>> convert ansi utf8 string into utf8string. @returns position of
       wrong utf8 code in s or -1 if conversion was successfull
       lows may be 1,2 or 4 - the lowest character size for conversion}
function ucfs_from_utf8string( const s: String; out ps: PUCFS32String; lows: VMInt = 1 ): VMInt;
{DOC>> ucfs_from_utf8string, triggers IE on fault}
function ucfs_utf8us( const s: String; lows: VMInt = 1 ): PUCFS32String;
{DOC>> convert ucfstring into utf8 encoded string}
function ucfs_to_utf8string( ps: PUCFS32String ): String;

{DOC>> alloc string with len charlen, and bytesize blen (must be 1,2 or 4)}
function ucfs_alloc( charlen: VMInt; blen: VMInt = 1 ): PUCFS32String; inline;
{DOC>> decref or free string}
procedure ucfs_release( ps: PUCFS32String ); inline;

{DOC>> Copy before Write - Check
       Checks if string is referenced more than once, if so it will create
       a new copy and return @true, otherwise nothing changes and it
       returns @false.}
function ucfs_cbw_check( var ps: PUCFS32String; b: VMInt = 0 ): Boolean;

{DOC>> (explicit) copy (part of) string, expand to bytesize b or compress (b=0)}
function ucfs_cpy( ps: PUCFS32String; start, len: VMInt; b: VMInt = 0 ): PUCFS32String;
{DOC>> (explicit) copy (whole) string, expand to bytesize b or compress (b=0)}
function ucfs_cpy( ps: PUCFS32String; b: VMInt = 0 ): PUCFS32String; inline;

{DOC>> (ref) copy string}
function ucfs_incref( ps: PUCFS32String ): PUCFS32String; inline;

(*******************************************************************************
 * UCF String OPS
 ******************************************************************************)

{DOC>> concat 2 strings, result is a new string}
function ucfs_concat( psl, psr: PUCFS32String ): PUCFS32String;
{DOC>> qlex compare}
function ucfs_compare( psl, psr: PUCFS32String ): VMInt;
{DOC>> qlex compare directly with ascii 7b string}
function ucfs_compare_a7( psl: PUCFS32String; const s: String ): VMInt;
{DOC>> check if substring pss matches ps at pos p..p+len-1}
function ucfs_submatch( ps, pss: PUCFS32String; p, len: VMInt ): Boolean;
{DOC>> move src/sub string pss into psdest at pos destp..destp+srclen-1
       returns false if psdestp must be regrown due to bytelen missmatch
       invalid srclen/destp -> assert}
procedure ucfs_submove( pss: PUCFS32String; var psdest: PUCFS32String; destp, srclen: VMInt); inline;

implementation

uses eomsg;

(*******************************************************************************
 * UCF String OPS
 ******************************************************************************)

function ucfs_compare_a7(psl: PUCFS32String; const s: String): VMInt;
var l, i: VMInt;
begin
  l := ucfs_length(psl);
  Result := l - Length(s);
  if (Result = 0) and
     (l > 0) then
    begin
      i := 1;
      while (i <= l) and
            (Result = 0) do
        begin
          ASSERT(Ord(s[i]) <= $7F);
          Result := ucfs_getc(psl,i) - Ord(s[i]);
          Inc(i,1);
        end;
    end;
end;

function ucfs_submatch( ps, pss: PUCFS32String; p, len: VMInt ): Boolean;
var i: VMInt;
begin
  ASSERT(p > 0);
  ASSERT(len >= 0);
  {check if substr pss fits}
  if len > ucfs_length(pss) then
    len := ucfs_length(pss);
  Result := ((p+len)-1) <= ucfs_length(ps);
  if Result then
    begin
      for i := 1 to len do
        begin
          if ucfs_getc(ps,(i-1)+p) <> ucfs_getc(pss,i) then
            Exit(false);
        end;
    end;
end;

function ucfs_submove_check(pss, psdest: PUCFS32String; destp, srclen: VMInt
  ): Boolean;
var i: VMInt;
begin
  ASSERT(srclen >= 0);
  ASSERT(srclen <= ucfs_length(pss));
  ASSERT(destp >= 0);
  ASSERT(((destp+srclen)-1) <= ucfs_length(psdest));
  Result := true;
  if pss^.info.getcharsize = psdest^.info.getcharsize then
    begin
      Move( pss^.bchars[0], psdest^.bchars[(destp-1) * psdest^.info.getcharsize], srclen*psdest^.info.getcharsize);
    end
  else if pss^.info.getcharsize < psdest^.info.getcharsize then
    begin
      for i := 0 to srclen-1 do
        ucfs_setc(psdest,i+destp,ucfs_getc(pss,i+1));
    end
  else
    Result := false;
end;

procedure ucfs_submove(pss: PUCFS32String; var psdest: PUCFS32String; destp, srclen: VMInt);
var tmp: PUCFS32String;
begin
  if (srclen <= 0) then
    Exit;
  ucfs_cbw_check(psdest,ucfs_charsize(psdest));
  if not ucfs_submove_check(pss,psdest,destp,srclen) then
    begin
      tmp := ucfs_cpy(psdest,pss^.info.getcharsize);
      ucfs_release(psdest);
      psdest := tmp;
      if not ucfs_submove_check(pss,psdest,destp,srclen) then
        put_internalerror(12012801);
    end;
end;

function ucfs_compare( psl, psr: PUCFS32String ): VMInt;
var l, i: VMInt;
begin
  Result := 0;
  if psl<>psr then
    begin
      l := ucfs_length(psl);
      Result := l - ucfs_length(psr);
      if Result = 0 then // -> l>0 since psl<>psr
        begin
          {same length, check for same char size (simply compare
           both packed charlen fields)}
          if psl^.info.charlen = psr^.info.charlen then
            begin
              if psl^.info.getcharsize = 1 then
                Result := CompareByte(psl^.bchars[0],psr^.bchars[0],l)
              else if psl^.info.getcharsize = 2 then
                Result := CompareWord(psl^.wchars[0],psr^.wchars[0],l)
              else
                Result := CompareDWord(psl^.lchars[0],psr^.lchars[0],l);
            end
          else
            begin
              i := 1;
              while (i <= l) and
                    (Result = 0) do
                begin
                  // u32c has 31 bit max -> wont overflow (if uc32c is correct)
                  Result := ucfs_getc(psl,i) - ucfs_getc(psr,i);
                  Inc(i,1);
                end;
            end;
        end;
    end;
end;

function ucfs_concat( psl, psr: PUCFS32String ): PUCFS32String;
begin
  Result := nil;
  if Assigned(psl) and
     Assigned(psr) then
    begin
      ASSERT( (psl^.info.getlen+psr^.info.getlen) <= CL_SO_MaxShortBuffer );
      {alloc}
      if psl^.info.getcharsize > psr^.info.getcharsize then
        Result := ucfs_alloc( psl^.info.getlen + psr^.info.getlen, psl^.info.getcharsize )
      else
        Result := ucfs_alloc( psl^.info.getlen + psr^.info.getlen, psr^.info.getcharsize );
      {move into result}
      ucfs_submove(psl,Result,1,psl^.info.getlen);
      ucfs_submove(psr,Result,psl^.info.getlen+1,psr^.info.getlen);
    end
  else
    begin
      {either left/right/nil - use downgrading}
      if Assigned(psl) then
        Result := ucfs_incref(psl);
      if Assigned(psr) then
        Result := ucfs_incref(psr);
    end;
end;

(*******************************************************************************
 * UCF Strings
 ******************************************************************************)

function ucfs_to_utf8string( ps: PUCFS32String ): String;
{decode to string}
var i,l: VMInt;
    uc: TUCFS32Char;
    utf8c: TFSUtf8Char;
begin
  if ucfs_length(ps) <= 0 then
    Exit('');

  {calculate real byte length}
  l := 0;
  for i := 1 to ps^.info.getlen do
    begin
      uc := ucfs_getc(ps,i);
      ASSERT(uc >= 0);
      Inc(l,ucfs_u32c_utf8len(uc));
    end;

  {de/encode}
  SetLength(Result,l);
  l := 1;
  for i := 1 to ps^.info.getlen do
    begin
      uc := ucfs_getc(ps,i);
      ucfs_u32c_to_utf8c(uc,@utf8c);
      ASSERT(utf8c.len >= 1);
      ASSERT(utf8c.len <= 6);
      if utf8c.len <= 1 then
        Result[l] := Chr(utf8c.cbytes[0])
      else
        Move( utf8c.cbytes[0], Result[l], utf8c.len );
      Inc(l,utf8c.len);
    end;
end;

function ucfs_cpy( ps: PUCFS32String; start, len: VMInt; b: VMInt = 0 ): PUCFS32String;
var i: VMInt;
begin
  ASSERT(b in [0,1,2,4]);
  if not Assigned(ps) then
    Exit(nil);
  ASSERT(start > 0);
  {check explicit shrinking: it could destroy characters}
  if (b > 0) and
     (b < ps^.info.getcharsize) then
    b := ps^.info.getcharsize;
  {check start above len -> 0 len copy}
  if start > ps^.info.getlen then
    Exit(nil);
  {check len, it must be correct for move/copy so cut it to the maximum len}
  if ((start+len)-1) > ps^.info.getlen then
    len := (ps^.info.getlen+1)-start;
  if len > 0 then
    begin
      if b <= 0 then
        begin
          {check if result string can be downsized}
          b := 1;
          for i := start to (start+len)-1 do
            begin
              if (b=1) and
                 (ucfs_getc( ps, i ) > $FF) then
                b := 2;
              if (b=2) and
                 (ucfs_getc( ps, i ) > $FFFF) then
                begin
                  b := 4;
                  break;
                end;
            end;
        end;
      {alloc and copy (either move for same size, or use expanding/shrinking characterwise conversion)}
      Result := ucfs_alloc( len, b );
      if b = ps^.info.getcharsize then
        Move( ps^.bchars[(start-1)*b], Result^.bchars[0], len*b )
      else
        begin
          for i := start to (start+len)-1 do
            ucfs_setc( Result, (i-start)+1, ucfs_getc( ps, i ) );
        end;
    end
  else
    Result := nil;
end;

function ucfs_cpy(ps: PUCFS32String; b: VMInt): PUCFS32String;
begin
  Result := ucfs_cpy(ps,1,ucfs_length(ps),b);
end;

function ucfs_alloc( charlen: VMInt; blen: VMInt = 1 ): PUCFS32String;
begin
  ASSERT( charlen <= CL_SO_MaxShortBuffer );
  ASSERT( blen in [1,2,4] );
  if charlen <= 0 then
    Exit(nil);
  Result := GetMem( SizeOf(TUCFS32StringInfo) + (blen*charlen) );
  Result^.info.init(blen,charlen);
end;

procedure ucfs_release( ps: PUCFS32String );
begin
  if Assigned(ps) then
    begin
      ASSERT(ps^.info.refcnt > 0);
      Dec(ps^.info.refcnt,1);
      if ps^.info.refcnt <= 0 then
        Freemem( ps, ps^.info.getbufferlen );
    end;
end;

function ucfs_cbw_check(var ps: PUCFS32String; b: VMInt): Boolean;
begin
  if Assigned(ps) and
     (ps^.info.refcnt > 1) then
    begin
      ucfs_release(ps);
      ps := ucfs_cpy(ps,b);
      Result := true;
    end
  else
    Result := false;
end;

function ucfs_incref(ps: PUCFS32String): PUCFS32String;
begin
  if Assigned(ps) then
    Inc(ps^.info.refcnt);
  Result := ps;
end;

function ucfs_length( ps: PUCFS32String ): VMInt;
begin
  if Assigned(ps) then
    Result := ps^.info.getlen
  else
    Result := 0;
end;

function ucfs_charsize(ps: PUCFS32String): VMInt;
begin
  if Assigned(ps) then
    Result := ps^.info.getcharsize
  else
    Result := 1;
end;

function ucfs_setc_check( ps: PUCFS32String; p: VMInt; uc: TUCFS32Char ): Boolean;
begin
  ASSERT( Assigned(ps) );
  ASSERT( uc >= 0 );
  ASSERT( p > 0 );
  ASSERT( p <= ps^.info.getlen );
  Result := true;
  if ps^.info.getcharsize = 1 then
    begin
      if uc <= $FF then
        ps^.bchars[ p-1 ] := uc
      else
        Result := false;
    end
  else if ps^.info.getcharsize = 2 then
    begin
      if uc <= $FFFF then
        ps^.wchars[ p-1 ] := uc
      else
        Result := false;
    end
  else
    ps^.lchars[ p-1 ] := uc
end;

procedure ucfs_setc( var ps: PUCFS32String; p: VMInt; uc: TUCFS32Char );
var tmp: PUCFS32String;
begin
  ucfs_cbw_check(ps);
  if not ucfs_setc_check( ps, p, uc ) then
    begin
      if uc <= $FFFF then
        tmp := ucfs_cpy(ps,2)
      else
        tmp := ucfs_cpy(ps,4);
      ucfs_release(ps);
      ps := tmp;
      if not ucfs_setc_check(ps,p,uc) then
        put_internalerror(12012802);
    end;
end;

function ucfs_getc( ps: PUCFS32String; p: VMInt ): TUCFS32Char;
begin
  ASSERT( Assigned(ps) );
  ASSERT( p > 0 );
  ASSERT( p <= ps^.info.getlen );
  ASSERT( ps^.info.getcharsize in [1,2,4] );
  if ps^.info.getcharsize = 1 then
    Result := ps^.bchars[ p-1 ]
  else if ps^.info.getcharsize = 2 then
    Result := ps^.wchars[ p-1 ]
  else
    Result := ps^.lchars[ p-1 ];
end;

function ucfs_utf8us(const s: String; lows: VMInt): PUCFS32String;
begin
  if ucfs_from_utf8string( s, Result, lows ) >= 0 then
    put_internalerror(12012803);
end;

function ucfs_from_utf8string( const s: String; out ps: PUCFS32String; lows: VMInt ): VMInt;
var i,j,len,cl,ml: VMInt;
    uc: TUCFS32Char;
    utf8c: TFSUtf8Char;
begin
  {check length 0 string}
  ps := nil;
  if Length(s) <= 0 then
    Exit(-1);

  {calculate real length in utf8string}
  ASSERT( lows in [1,2,4] );
  len := 0;
  i := 1;
  ml := lows;
  {skip over utf8 first bytes and calculate the real length and
   maximum byte length}
  while i <= Length(s) do
    begin
      cl := ucfs_FirstByte_To_CharLength( Ord(s[i]) );
      if cl <= 0 then 
        Exit(i);  // first byte decoding error
      ASSERT( cl <= 6 );
      if cl > ml then
        ml := cl;
      if (cl+i) > (Length(s)+1) then
        Exit(i);  // missing bytes
      Inc( i, cl );
      Inc(len,1);
    end;

  {check maxlength}
  if len > CL_SO_MaxShortBuffer then
    Exit(CL_SO_MaxShortBuffer);

  {set the actual byte length,
   -> a 1 byte utf8 char is representable using 1 byte (max 7 bit)
   -> a 3 byte utf8 char is representable with 2 bytes (7/11/16 bit for 1/2/3 byte utf8)
   -> a 6 byte utf8 char is representable with 4 bytes (7/11/16/21/26/31 bit for 1/2/3/4/5/6 byte utf8)}
  if ml <= 1 then
    ml := 1
  else if ml <= 3 then
    ml := 2
  else
    ml := 4;

  {allocate, move/expand}
  ps := ucfs_alloc( len, ml );
  j := 1;
  for i := 1 to len do
    begin
      {decode length, load utf8 sequence and test/convert it into u32c}
      cl := ucfs_FirstByte_To_CharLength( Ord(s[j]) );
      utf8c.len := cl;
      if cl <= 1 then
        utf8c.cbytes[0] := Ord(s[j])
      else
        Move(s[j],utf8c.cbytes[0],cl);
      if not ucfs_valid_utf8c( @utf8c ) then
        begin
          Freemem(ps);
          ps := nil;
          Exit(j);
        end;
      uc := ucfs_utf8c_To_u32c( @utf8c );
      {aaand set it in the u32c utf8 string}
      Inc(j, cl);
      if not ucfs_setc_check( ps, i, uc ) then
        begin
          Freemem(ps);
          ps := nil;
          ASSERT(true); // misscalculated?
        end;
    end;
  Result := -1;
end;

(*******************************************************************************
 * UTF8 Character Handling/Conversion
 ******************************************************************************)

function ucfs_valid_utf8c( pc: PFSUtf8Char ): Boolean;
var tcu: TUCFS32Char;
    utf8c: TFSUtf8Char;
begin
  Result := (pc^.len >= 1) and
            (pc^.len <= MaxUtf8Len);
  if Result then
    tcu := ucfs_utf8c_To_u32c( pc );
  Result := Result and (tcu >= 0);
  if Result then
    begin
      ucfs_u32c_To_utf8c( tcu, @utf8c );
      Result := (pc^.len = utf8c.len) and
                (CompareByte(pc^.cbytes[0], utf8c.cbytes[0], utf8c.len ) = 0);
    end;
end;

function ucfs_u32c_utf8len( uc: TUCFS32Char ): VMInt;
begin
  ASSERT(uc >= 0);
  Result := 0;
  while uc > Utf8ByteFirstMask[Result] do
    begin
      uc := uc shr 6;
      Inc(Result,1);
    end;
  Inc(Result,1);
end;

function ucfs_u32c_bytelen(uc: TUCFS32Char): VMInt;
begin
  if uc <= $FF then
    Result := 1
  else if uc <= $FFFF then
    Result := 2
  else
    Result := 4;
end;

function ucfs_u32c_upcase(uc: TUCFS32Char): TUCFS32Char;
const
  ucdiff = Ord('A')-Ord('a');
begin
  if (uc >= Ord('a')) and
     (uc <= Ord('z')) then
    Result := uc + ucdiff
  else
    Result := uc;
end;

function ucfs_u32c_lowercase(uc: TUCFS32Char): TUCFS32Char;
const
  ucdiff = Ord('a')-Ord('A');
begin
  if (uc >= Ord('A')) and
     (uc <= Ord('Z')) then
    Result := uc + ucdiff
  else
    Result := uc;
end;

procedure ucfs_u32c_To_utf8c( uc: TUCFS32Char; pc: PFSUtf8Char );
var l: VMInt;
begin
  if uc < 0 then
    uc := 0; // cut.. 32 bit = invalid utf8_6 only codes 31 bits
  {simple case, lower 7bit}
  if uc <= Utf8ByteFirstMask[0] then
    begin
      pc^.len := 1;
      pc^.cbytes[0] := uc;
    end;
  {calc length by checking nr of 6bit bytes to be used}
  l := ucfs_u32c_utf8len(uc);
  pc^.len := l;
  {write bytes 6..2}
  for l := l-1 downto 1 do
    begin
      pc^.cbytes[l] := (Utf8ByteSequenceMarkerMask and uc) or Utf8ByteSequenceMarker;
      uc := uc shr 6;
    end;
  {write first byte}
  pc^.cbytes[0] := (uc and Utf8ByteFirstMask[pc^.len-1]) or (Utf8ByteFirstMarker[pc^.len-1]);
end;

function ucfs_utf8c_To_u32c( pc: PFSUtf8Char ): TUCFS32Char;
var i: VMInt;
begin
  ASSERT(pc^.len > 0);
  ASSERT(pc^.len <= MaxUtf8Len);
  Result := pc^.cbytes[0] and Utf8ByteFirstMask[pc^.len-1];
  for i := 1 to pc^.len-1 do
    Result := (Result shl 6) or (Utf8ByteSequenceMarkerMask and pc^.cbytes[i]);
end;

function ucfs_FirstByte_To_CharLength( fsb: Byte ): Byte;
begin
  Result := 0;
  while (Result < MaxUtf8Len) and
        ((fsb and (not Utf8ByteFirstMask[Result])) <>
         (fsb and Utf8ByteFirstMarker[Result])) do
    Inc(Result,1);
  Inc(Result,1);
  if Result > MaxUtf8Len then
    Result := 0;
end;

function ucfs_valid_follow(fsb: Byte): Boolean;
begin
  Result := (fsb and (Utf8ByteSequenceMarkerMask or Utf8ByteSequenceMarker)) = fsb;
end;

{ TUCFS32StringInfo }

procedure TUCFS32StringInfo.init(cc: Byte; len: VMWord; initr: VMWord);
begin
  ASSERT(cc in [1,2,4]);
  ASSERT(len <= CL_SO_MaxShortBuffer);
  charlen := len shl 3;
  charlen := charlen or cc;
  refcnt := initr;
end;

function TUCFS32StringInfo.getrefs: VMWord;
begin
  Result := refcnt;
end;

function TUCFS32StringInfo.getlen: VMWord;
begin
  Result := charlen shr 3;
end;

function TUCFS32StringInfo.getcharsize: VMWord;
begin
  Result := charlen and %111;
end;

function TUCFS32StringInfo.getbufferlen: VMWord;
begin
  Result := (getlen*getcharsize) + SizeOf(TUCFS32StringInfo);
end;

end.
