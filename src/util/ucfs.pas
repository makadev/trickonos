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
  TFSULongs = array[ 0 .. CL_SO_MaxShortBuffer-1 ] of LongWord;
  
  PUCFS32String = ^TUCFS32String;
  TUCFS32String = packed record
    charlen: VMInt;   // length in Chars
    bytelen: VMInt;   // length per Byte -> bytelen*charlen -> real length
    chars: TFSUBytes; // <- dont access directly when bytelen > 0
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

{DOC>> set character uc, at position p in ps, returns false if string
       must be regrown
       -> ps must be <> nil (nonempty)}
function ucfs_setc_check( ps: PUCFS32String; p: VMInt; uc: TUCFS32Char ): Boolean;
{DOC>> ucfs_setc_check wrapper (triggers IE on unhandled regrow) }
procedure ucfs_setc( ps: PUCFS32String; p: VMInt; uc: TUCFS32Char ); inline;
{DOC>> get character at position p in ps
       -> ps must be <> nil (nonempty)}
function ucfs_getc( ps: PUCFS32String; p: VMInt ): TUCFS32Char;
{DOC>> get length of strings in characters}
function ucfs_length( ps: PUCFS32String ): VMInt;
{DOC>> get byte length (length for 1 character in bytes)}
function ucfs_charsize( ps: PUCFS32String ): VMInt; inline;
{DOC>> convert ansi utf8 string into utf8string. @returns position of
       wrong utf8 code in s or -1 if conversion was successfull
       lows may be 1,2 or 4 - the lowest character size for conversion}
function ucfs_from_utf8string( const s: String; out ps: PUCFS32String; lows: VMInt = 1 ): VMInt;
{DOC>> ucfs_from_utf8string, triggers IE on fault}
function ucfs_utf8us( const s: String; lows: VMInt = 1 ): PUCFS32String;
{DOC>> convert ansi ascii 7bit string into utf8 string. @returns position of wrong
       character or -1 if conversion was successfull.}
function ucfs_from_string( const s: String; out ps: PUCFS32String; lows: VMInt = 1 ): VMInt;
{DOC>> ucfs_from_string, triggers IE on fault}
function ucfs_a7us( const s: String; lows: VMInt = 1 ): PUCFS32String;
{DOC>> convert ucfstring into utf8 encoded string}
function ucfs_to_utf8string( ps: PUCFS32String ): String;
{DOC>> convert ucfstring into ansi ascii 7 bit string, if replace_faults=true, non a7
       characters are replaced with C_DEFAULT_FAULT_REPLACE, if
       replace_faults = false, the result will be empty on faults (check length)}
function ucfs_to_string( ps: PUCFS32String; replace_faults: Boolean = false ): String;
{DOC>> alloc string with len charlen, and bytesize blen (must be 1,2 or 4)}
function ucfs_alloc( charlen: VMInt; blen: VMInt = 1 ): PUCFS32String; inline;
{DOC>> free string}
procedure ucfs_release( ps: PUCFS32String ); inline;
{DOC>> copy (part of) string, expand to bytesize b or compress (b=0)}
function ucfs_copy( ps: PUCFS32String; start, len: VMInt; b: VMInt = 0 ): PUCFS32String;
{DOC>> copy (whole) string, expand to bytesize b or compress (b=0)}
function ucfs_copy( ps: PUCFS32String; b: VMInt = 0 ): PUCFS32String; inline;

(*******************************************************************************
 * UCF String OPS
 ******************************************************************************)

{DOC>> concat 2 strings, result is a new string}
function ucfs_concat( psl, psr: PUCFS32String ): PUCFS32String;
{DOC>> qlex compare}
function ucfs_compare( psl, psr: PUCFS32String ): VMInt;
{DOC>> check if substring pss matches ps at pos p..p+len-1}
function ucfs_submatch( ps, pss: PUCFS32String; p, len: VMInt ): Boolean;
{DOC>> move src/sub string pss into psdest at pos destp..destp+srclen-1
       returns false if psdestp must be regrown due to bytelen missmatch
       invalid srclen/destp -> assert}
function ucfs_submove_check( pss, psdest: PUCFS32String; destp, srclen: VMInt ): Boolean;
{DOC>> submove, triggers IE on undhandled regrow}
procedure ucfs_submove(pss, psdest: PUCFS32String; destp, srclen: VMInt); inline;

implementation

uses eomsg;

(*******************************************************************************
 * UCF String OPS
 ******************************************************************************)

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
  if (srclen <= 0) then
    Exit(true);
  ASSERT(destp >= 0);
  ASSERT(((destp+srclen)-1) <= ucfs_length(psdest));
  if pss^.bytelen = psdest^.bytelen then
    Move( pss^.chars[0], psdest^.chars[(destp-1) * psdest^.bytelen], srclen*psdest^.bytelen)
  else
    begin
      if pss^.bytelen > psdest^.bytelen then
        begin
          for i := 1 to pss^.charlen do
            if ucfs_u32c_bytelen(ucfs_getc(pss,i)) > psdest^.bytelen then
              Exit(false);
        end;
      for i := 1 to pss^.charlen do
        ucfs_setc(psdest,(i+destp)-1,ucfs_getc(pss,i));
    end;
  Result := true;
end;

procedure ucfs_submove(pss, psdest: PUCFS32String; destp, srclen: VMInt);
begin
  if not ucfs_submove_check(pss,psdest,destp,srclen) then
    put_internalerror(12012801);
end;

function ucfs_compare( psl, psr: PUCFS32String ): VMInt;
var l, i: VMInt;
begin
  l := ucfs_length(psl);
  Result := l - ucfs_length(psr);
  if (Result = 0) and
     (l > 0) then
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

function ucfs_concat( psl, psr: PUCFS32String ): PUCFS32String;
var i: VMInt;
begin
  Result := nil;
  if Assigned(psl) and
     Assigned(psr) then
    begin
      ASSERT( (psl^.charlen+psr^.charlen) <= CL_SO_MaxShortBuffer );
      {alloc}
      if psl^.bytelen > psr^.bytelen then
        Result := ucfs_alloc( psl^.charlen + psr^.charlen, psl^.bytelen )
      else
        Result := ucfs_alloc( psl^.charlen + psr^.charlen, psr^.bytelen );
      {move into result}
      ucfs_submove(psl,Result,1,psl^.charlen);
      ucfs_submove(psr,Result,psl^.charlen+1,psr^.charlen);
    end
  else
    begin
      {either left/right/nil - use downgrading}
      if Assigned(psl) then
        Result := ucfs_copy( psl, 1, psl^.charlen );
      if Assigned(psr) then
        Result := ucfs_copy( psr, 1, psr^.charlen );
    end;
end;

(*******************************************************************************
 * UCF Strings
 ******************************************************************************)

type
  {for casting}
  PUCFS32String2 = ^TUCFS32String2;
  TUCFS32String2 = packed record
    charlen: VMInt;
    bytelen: VMInt;
    chars: TFSUWords;
  end;

  {for casting}
  PUCFS32String4 = ^TUCFS32String4;
  TUCFS32String4 = packed record
    charlen: VMInt;
    bytelen: VMInt;
    chars: TFSULongs;
  end;

function ucfs_a7us(const s: String; lows: VMInt): PUCFS32String;
begin
  if ucfs_from_string( s, Result, lows ) >= 0 then
    put_internalerror(12012804);
end;

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
  for i := 1 to ps^.charlen do
    begin
      uc := ucfs_getc(ps,i);
      ASSERT(uc >= 0);
      Inc(l,ucfs_u32c_utf8len(uc));
    end;

  {de/encode}
  SetLength(Result,l);
  l := 1;
  for i := 1 to ps^.charlen do
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

function ucfs_to_string( ps: PUCFS32String; replace_faults: Boolean = false ): String;
{decode to a7 string}
var i: VMInt;
    uc: TUCFS32Char;
begin
  if ucfs_length(ps) <= 0 then
    Exit('');

  SetLength(Result,ps^.charlen);
  for i := 1 to ps^.charlen do
    begin
      uc := ucfs_getc(ps,i);
      ASSERT(uc >= 0);
      if uc <= Utf8ByteFirstMask[0] then
        Result[i] := Chr(uc)
      else
        begin
          if replace_faults then
            Result[i] := C_DEFAULT_FAULT_REPLACE
          else
            Exit('');
        end;
    end;
end;

function ucfs_copy( ps: PUCFS32String; start, len: VMInt; b: VMInt = 0 ): PUCFS32String;
var i: VMInt;
begin
  ASSERT(b in [0,1,2,4]);
  if not Assigned(ps) then
    Exit(nil);
  ASSERT(start > 0);
  {check explicit shrinking: it could destroy characters}
  if (b > 0) and
     (b < ps^.bytelen) then
    b := ps^.bytelen;
  {check start above len -> 0 len copy}
  if start > ps^.charlen then
    Exit(nil);
  {check len, it must be correct for move/copy so cut it to the maximum len}
  if ((start+len)-1) > ps^.charlen then
    len := (ps^.charlen+1)-start;
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
      if b = ps^.bytelen then
        Move( ps^.chars[(start-1)*b], Result^.chars[0], len*b )
      else
        begin
          for i := start to (start+len)-1 do
            ucfs_setc( Result, (i-start)+1, ucfs_getc( ps, i ) );
        end;
    end
  else
    Result := nil;
end;

function ucfs_copy(ps: PUCFS32String; b: VMInt): PUCFS32String;
begin
  Result := ucfs_copy(ps,1,ucfs_length(ps),b);
end;

function ucfs_alloc( charlen: VMInt; blen: VMInt = 1 ): PUCFS32String;
begin
  ASSERT( blen in [1,2,4] );
  ASSERT( charlen <= CL_SO_MaxShortBuffer );
  if charlen <= 0 then
    Exit(nil);
  Result := GetMem( (SizeOf(TUCFS32String) - SizeOf(TFSUBytes)) + (blen*charlen) );
  Result^.charlen := charlen;
  Result^.bytelen := blen;
end;

procedure ucfs_release( ps: PUCFS32String );
begin
  if Assigned(ps) then
    Freemem( ps, (SizeOf(TUCFS32String) - SizeOf(TFSUBytes)) + (ps^.bytelen*ps^.charlen) );
end;

function ucfs_length( ps: PUCFS32String ): VMInt;
begin
  if Assigned(ps) then
    begin
      ASSERT(ps^.charlen > 0);
      Result := ps^.charlen
    end
  else
    Result := 0;
end;

function ucfs_setc_check( ps: PUCFS32String; p: VMInt; uc: TUCFS32Char ): Boolean;
begin
  ASSERT( Assigned(ps) );
  ASSERT( uc >= 0 );
  ASSERT( p > 0 );
  ASSERT( p <= ps^.charlen );
  ASSERT( ps^.bytelen in [1,2,4] );
  Result := true;
  if ps^.bytelen = 1 then
    begin
      if uc <= $FF then
        ps^.chars[ p-1 ] := uc
      else
        Result := false;
    end
  else if ps^.bytelen = 2 then
    begin
      if uc <= $FFFF then
        PUCFS32String2( ps )^.chars[ p-1 ] := uc
      else
        Result := false;
    end
  else
    PUCFS32String4( ps )^.chars[ p-1 ] := uc
end;

procedure ucfs_setc( ps: PUCFS32String; p: VMInt; uc: TUCFS32Char );
begin
  if not ucfs_setc_check( ps, p, uc ) then
    put_internalerror(12012802);
end;

function ucfs_getc( ps: PUCFS32String; p: VMInt ): TUCFS32Char;
begin
  ASSERT( Assigned(ps) );
  ASSERT( p > 0 );
  ASSERT( p <= ps^.charlen );
  ASSERT( ps^.bytelen in [1,2,4] );
  if ps^.bytelen = 1 then
    Result := ps^.chars[ p-1 ]
  else if ps^.bytelen = 2 then
    Result := PUCFS32String2( ps )^.chars[ p-1 ]
  else
    Result := PUCFS32String4( ps )^.chars[ p-1 ];
end;

function ucfs_utf8us(const s: String; lows: VMInt): PUCFS32String;
begin
  if ucfs_from_utf8string( s, Result, lows ) >= 0 then
    put_internalerror(12012803);
end;

function ucfs_from_string( const s: String; out ps: PUCFS32String; lows: VMInt = 1 ): VMInt;
var i: VMInt;
begin
  {check length 0 string}
  ps := nil;
  if Length(s) <= 0 then
    Exit(-1);

  {check maxlength}
  if Length(s) > CL_SO_MaxShortBuffer then
    Exit(CL_SO_MaxShortBuffer);

  {check all characters are 7bit}
  ASSERT( lows in [1,2,4] );
  for i := 1 to Length(s) do
    begin
      if Ord(s[i]) > Utf8ByteFirstMask[0] then
        Exit(i);
    end;

  {alloc and copy/expand}
  ps := ucfs_alloc( Length(s), lows );
  if lows = 1 then
    Move( s[1], ps^.chars[0], Length(s) )
  else
    begin
      for Result := 1 to Length(s) do
        ucfs_setc( ps, Result, Ord(s[i]) );
    end;
  Result := -1;
end;

function ucfs_charsize(ps: PUCFS32String): VMInt;
begin
  if Assigned(ps) then
    Result := ps^.bytelen
  else
    Result := 1;
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
    tcu: TUCFS32Char;
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

end.
