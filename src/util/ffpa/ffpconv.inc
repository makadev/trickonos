{   Fixed Fix Point Arithmetic conversion

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

function tfm_nrinfostr( ptfmint: PTFM_Integer ): String;
var f: TTFMIFlags;
begin
  Result := '';
  for f := Low(TTFMIFlags) to High(TTFMIFlags) do
    if f in ptfmint^.props.flags then
      begin
        if Length(Result) > 0 then
          Result := Result + ',';
        Result := Result + C_Flags_Short[f];
      end; 
  Result := '<m:' + IntToStr(ptfmint^.props.mbits) + 
            ',f:['+ Result + ']>';
end;

function make_sep_string( const s: String; nrsep: VMInt ): String;
var i,j,d: VMInt;
begin
  if (nrsep > 0) and
     (Length(s) > nrsep) and
     ((Length(s) div nrsep) > 0) then
    begin
      Result := '';
      {create result, since we have nr digits, (nr-1) div sep additional
       separators are introduced -> direct allocate}
      SetLength(Result,Length(s)+((Length(s)-1) div nrsep));
      d := Length(s);
      j := Length(Result);
      while d >= 1 do
         begin
           i := nrsep;
           {copy <sep> digits from result}
           while (d >= 1) and
                 (i >= 1) do
             begin
               Result[j] := s[d];
               Dec(d,1);
               Dec(i,1);
               Dec(j,1);
             end;
           {put separator}
           if d >= 1 then
             begin
               Result[j] := '_';
               Dec(j,1);
             end;
         end;
    end
  else
    Result := s;
end;

type
  TNumberChars = set of char;

function nr_contains_striprs( const s: String; nrset: TNumberChars;
                              allowesign, allowsep, stripz: Boolean ): String;
{check number format: [-|+]? [:nrset:,_]+ for allowesign and
  [:nrset:]+ for no signed, no separated.
 rewrite number into format: [:nrset:]+ [+|-] without separator
 optinally stripping leading zeros with stripz
 returns empty string if invalid transformation}
var pos, i, d: VMInt;
    signed, lzero: Boolean;
begin
  ASSERT( not ('_' in nrset) );
  ASSERT( not ('-' in nrset) );
  ASSERT( not ('+' in nrset) );

  if Length(s) <= 0 then
    Exit('');
  Result := '';

  pos := 1;
  if allowesign and
     (s[1] in [ '+', '-' ]) then
    begin
      pos := 2;
      signed := s[1] = '-';
    end;

  // check and count digits
  d := 0;
  lzero := false;
  for i := pos to Length(s) do
    begin
      // check and skip separator
      if allowsep and
         (s[i] = '_') then
        continue;
      if stripz then
        begin
          // check and skip leading zero, set lzero for special case of
          // zero number (all leading zeros)
          if s[i] = '0' then
            begin
              lzero := true;
              continue;
            end
          else
            stripz := false;
        end;
      if s[i] in nrset then
        begin
          {inc digit counter}
          Inc(d,1);
        end
      else
        Exit(''); // invalid digit
    end;
  if (d <= 0) and
     lzero then
    Inc(d,1);

  // copy stuff: we have d digits without leading zero, + 1 sign
  SetLength( Result, d+1 );
  // rewrite sign
  if signed then
    Result[d+1] := '-'
  else
    Result[d+1] := '+';

  // copy digits backward, ignoring separators
  for i := Length(s) downto pos do
    begin
      if s[i] in nrset then
        begin
          Result[d] := s[i];
          Dec(d,1);
          if d <= 0 then
            break;
        end;
    end;
end;

type
  TTFM_Integer_Small = object
    props: TTFM_Integer_Properties;
    intp: TFFPA_Word;
  end;

function tfm_to_string( nr: PTFM_Integer; sep: VMInt = 0 ): String;
{ create decimal string for nr, add separator '_' every 10^sep steps }
var radixnum: TTFM_Integer_Small;
    sepstep: VMInt;
    separate: Boolean;
begin
  Result := '';
  separate := (sep > 0);
  sepstep := sep;
  // prepare radix number r, use 5 bits (%01010 for 10)
  radixnum.props.Init(5);
  radixnum.intp := 10;
  // prepare division nr / r
  tfm_tmpload_division( nr, PTFM_Integer(@radixnum) );
  repeat
    // remainder = tmpB ~> needed for conversion (table lookup), clear after division
    // quotient = tmpA ~> successive division, since tmpA is also the place where to put the dividend, nothing changes
    // abs(divisor) = tmpC
    // -abs(divisor) = tmpD

    // div current num by radix - fix remainder/quotient since they are needed
    tfm_div_temps( true );
    // remainder is < wordsize, first word is located at 0
    // also, always positive, so resetting needs only to whipe this word
    if separate then
      begin
        if sepstep <= 0 then
          begin
            Result := '_' + Result;
            sepstep := sep;
          end;
        Dec(sepstep,1);
      end;
    ASSERT( (tmpB.intp[0] >= 0) and (tmpB.intp[0] < 10) );
    Result := Chr(tmpB.intp[0]+Ord('0')) + Result;
    tmpB.intp[0] := 0;
    tmpB.props.reset_of_flags;
    // check if quotient is 0 -> done conversion
  until tmpA.is_zero;
  if nr^.is_signed then
    Result := '-' + Result;
end;

function tfm_to_hex( nr: PTFM_Integer; sep: VMInt = 0; digits: VMInt = 0 ): String;
{ create hexadecimal string, using at most digits digits or
  all when digits <= 0 (strips leading zeros) }
const
  hextab = '0123456789abcdef';
var
  w: TFFPA_Word;
  spos, nw, i: VMInt;
  cutdig: Boolean;
begin
  cutdig := false;
  if digits <= 0 then
    begin
      digits := ((nr^.props.mbits-1) div 4)+1;
      cutdig := true;
    end;
  SetLength( Result, digits );
  spos := ((nr^.props.mbits-1) div 4)+1;
  nr^.cut;
  nw := 0;
  repeat
    w := nr^.intp[nw];
    for i := 1 to C_TTFM_WORD_BYTES do
      begin
        Result[digits] := hextab[ (w and $F)+1 ];
        w := w shr TFFPA_Word(4);
        Dec(spos,1);
        Dec(digits,1);
        if (spos <= 0) or 
           (digits <= 0) then
          break;
        Result[digits] := hextab[ (w and $F)+1 ];
        w := w shr TFFPA_Word(4);
        Dec(spos,1);
        Dec(digits,1);
        if (spos <= 0) or 
           (digits <= 0) then
          break;
      end;
    Inc(nw,1);
  until (spos <= 0) or
        (digits <= 0);
  
  while digits > 0 do
    begin
      Result[digits] := '0';
      Dec(digits,1);
    end;
 
  if cutdig then
    begin 
      spos := 1;
      while (spos < Length(Result)) and
            (Result[spos] = '0') do
         Inc(spos,1);
      Result := Copy(Result,spos,Length(Result));
    end;

  if sep > 0 then
    Result := make_sep_string(Result, sep);
end;


function tfm_from_string( num: String; bits: VMInt ): PTFM_Integer;
{ load Integer from decimal string: Format [+|-]? [0-9_]+ }
var radixnum: TTFM_Integer_Small;
    i, j: VMInt;
begin
  // check&rewrite number
  num := nr_contains_striprs( num, ['0'..'9'], true, true, true );

  if (Length(num) <= 0) then
    Exit(nil);

  // prepare radix number r, use 5 bits (%01010 for 10)
  radixnum.props.Init(5);
  radixnum.intp := 10;

  // prepare result (for size and as argument for multiply preparation)
  Result := tfm_alloc( bits, true );

  i := 1;
  j := Length(num)-1;
  if i < j then
    begin
      // prepare multiplication by r, use r as A since it will speed up the
      // multiplication, Result is currently 0
      tfm_tmpload_multiplication( PTFM_Integer(@radixnum), Result );
      repeat
        // reset A to 10
        tmpA.intp[0] := 10;
        tmpA.props.reset_of_flags;
        // add number to B
        ASSERT(((Ord(num[i]) - Ord('0')) >= 0) and
               ((Ord(num[i]) - Ord('0')) < 10));
        radixnum.intp := Ord(num[i]) - Ord('0');
        tmpB.add_direct( PTFM_Integer(@radixnum) );
        // move B into C
        tmpC.copy_num( @tmpB );
       // wipe B
        tmpB.reinit;
        Inc( i, 1 );
        // multiplyadd C*10 into B;
        tfm_mul_temps;
      until i >= j;
      // Result is in tmpB, copy back
      Result^.copy_num( @tmpB );
    end;

  // add last number
  ASSERT(((Ord(num[i]) - Ord('0')) >= 0) and
         ((Ord(num[i]) - Ord('0')) < 10));
  radixnum.intp := Ord(num[i]) - Ord('0');
  Result^.add_direct( PTFM_Integer(@radixnum) );

  // check and set sign
  if num[j] = '-' then
    Result^.twoscomplement;
end;

function tfm_from_hex( hexstring: String; bits: VMInt ): PTFM_Integer;
{DOC>> load Integer from hex string [+|-]? [0-9a-fA-F_]+}
var d, i, j: VMInt;
begin
  // check&rewrite number
  hexstring := nr_contains_striprs( UpCase(hexstring), ['0'..'9','A'..'F'], true, true, true );

  if (Length(hexstring) <= 0) then
    Exit(nil);

  // alloc number of given bitness
  Result := tfm_alloc( bits, true );

  // directly write wordwise
  d := Length(hexstring)-1;
  j := 0;
  while (d > 0) and
        (bits > 0) do
    begin
      i := C_TTFM_WORD_BITS div 4;
      repeat
        if hexstring[d] in ['0'..'9'] then
          Result^.intp[j] := (Result^.intp[j] shl TFFPA_Word(4))
                             or TFFPA_Word( Ord(hexstring[d]) - Ord('0') )
        else
          Result^.intp[j] := (Result^.intp[j] shl TFFPA_Word(4))
                             or TFFPA_Word( Ord(hexstring[d]) - Ord('A') );
        Dec(i,1);
        Dec(d,1);
        Dec(bits,4);
      until (d <= 0) or
            (bits <= 0) or
            (i < 0);
      Inc(j,1);
    end;

  // since we wrote directly into this number, reset its oper flags which may
  // be wrong now
  Result^.props.reset_of_flags;

  // sign
  if hexstring[Length(hexstring)] = '-' then
    Result^.twoscomplement;
end;

function tfm_load_null( m: VMInt = 32 ): PTFM_Integer;
begin
  Result := tfm_alloc(m);
end;

function tfm_load_int( val: VMInt; m: VMInt = 32 ): PTFM_Integer;
begin
  Result := tfm_alloc(m);
  Result^.intp[0] := LongWord(val);
  Result^.props.reset_of_flags;
end;
