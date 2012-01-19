{   Fixed Fix Point Arithmetic multipliy related code

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


procedure tfm_tmpload_division( ptfm_A, ptfm_B: PTFM_Integer );
{ load tmpA/B/CD for division (successive substraction) ptfm_A / ptfm_B:
   A is loaded with abs(dividend), will by quotient
   B zereoed, will be remainder
   C is loaded with abs(divisor)
   D is loaded with -abs(divisor) }
var m, newm: VMInt;
begin
  // expand m
  max_upgrade( ptfm_A, ptfm_B, newm );

  // expand to word boundary
  if (newm mod C_TTFM_WORD_BITS) <> 0 then
    m := (newm - (newm mod C_TTFM_WORD_BITS)) + C_TTFM_WORD_BITS
  else
    m := newm;

  tmpA.props.Init(m);
  Move( tmpA.props, tmpB.props, SizeOf(TTFM_Integer_Properties) );
  Move( tmpA.props, tmpC.props, SizeOf(TTFM_Integer_Properties) );
  Move( tmpA.props, tmpD.props, SizeOf(TTFM_Integer_Properties) );

  // set and load A with ptfm_A
  tmpA.copy_num( ptfm_A );
  if ptfm_A^.is_signed then
    tmpA.twoscomplement;

  // wipe B
  FillByte( tmpB.intp[0], tmpB.props.sizebytes, 0 );

  // set and load C with ptfm_B
  tmpC.copy_num( ptfm_B );
  if ptfm_B^.is_signed then
    tmpC.twoscomplement;

  // set and load D with ptfm_B
  tmpD.copy_num( ptfm_B );
  if not ptfm_B^.is_signed then
    tmpD.twoscomplement;
end;

procedure tfm_tmpload_multiplication( ptfm_A, ptfm_B: PTFM_Integer );
{ load tmpA/B/C for multiplication (successive adding) ptfm_A * ptfm_B:
   A is loaded with the first factor (multiplier, it instructs the number of adds)
   B zereoed, will be (partial) result
   C is loaded with the second factor (this one is added) }
var m, newm: VMInt;
begin
  // expand m,n
  max_upgrade( ptfm_A, ptfm_B, newm );

  // expand to word boundary
  if (newm mod C_TTFM_WORD_BITS) <> 0 then
    m := (newm - (newm mod C_TTFM_WORD_BITS)) + C_TTFM_WORD_BITS
  else
    m := newm;

  tmpA.props.Init(m);
  Move( tmpA.props, tmpB.props, SizeOf(TTFM_Integer_Properties) );
  Move( tmpA.props, tmpC.props, SizeOf(TTFM_Integer_Properties) );

  // set and load A with ptfm_A
  tmpA.copy_num( ptfm_A );
  if ptfm_A^.is_signed then
    tmpA.twoscomplement;


  // A is not added, only bit tested and shifted, so check if its safe to
  // reduce the amount of tests (the bitsize)
  while ((m-C_TTFM_WORD_BITS) > 0) and
        (tmpA.intp[m div C_TTFM_WORD_BITS] = 0) do
    m := m - C_TTFM_WORD_BITS;

  // wipe B
  FillByte( tmpB.intp[0], tmpB.props.sizebytes, 0 );

  // set and load C with ptfm_B
  tmpC.copy_num( ptfm_B );
  if ptfm_B^.is_signed then
    tmpC.twoscomplement;
end;

procedure tfm_temporal_add_B_C;
{add tmpC to tmpB}
var w: VMInt;
    carry: Boolean;
begin
  carry := false;
  for w := 0 to tmpC.props.sizewords-1 do
    begin
      if carry then
{$PUSH}
{$R-}
{$Q-}
        begin
          tmpB.intp[w] := tmpB.intp[w] + tmpC.intp[w] + 1;
          carry := tmpB.intp[w] = 0;
        end
      else
        begin
          tmpB.intp[w] := tmpB.intp[w] + tmpC.intp[w];
          carry := tmpB.intp[w] < tmpC.intp[w];
        end;
{$POP}
    end;
  tmpB.props.reset_of_flags;
end;

procedure tfm_temporal_add_B_D;
{add tmpD to tmpB}
var w: VMInt;
    carry: Boolean;
begin
  carry := false;
  for w := 0 to tmpD.props.sizewords-1 do
    begin
      if carry then
{$PUSH}
{$R-}
{$Q-}
        begin
          tmpB.intp[w] := tmpB.intp[w] + tmpD.intp[w] + 1;
          carry := tmpB.intp[w] = 0;
        end
      else
        begin
          tmpB.intp[w] := tmpB.intp[w] + tmpD.intp[w];
          carry := tmpB.intp[w] < tmpD.intp[w];
        end;
{$POP}
    end;
  tmpB.props.reset_of_flags;
end;

function tfm_div_temps( remcorrect: Boolean ): Boolean;
{ divide package tmpB.tmpA by tmpC/tmpD
   returns false if tmpC is zero (division by zero)

  Basic algorithm

  calc l div r (non-restoring division)

    * R = abs(l) // always divide unsigned
    * D = abs(r)
    * R shl 1 // left shift remainder instead of right shift divisor
    *
    * qbit := 1 // initial - try subtract
    *
    * for bits do
    *   // subtract or add (correct) depending on previous operation
    *   if qbit = 1 then
    *     R := R - (D shl bits)
    *   else
    *     R := R + (D shl bits)
    *   // check which resulting quotient bit and type of operation for next step
    *   if R >= 0 then
    *     R shl 1
    *     R[0] := qbit := 1 // divisor fits -> normal subtract
    *   else
    *     R shl 1
    *     R[0] := qbit := 0 // divisor didn't fit -> correct in next step
    * correct some stuff
}

var nrbits: VMInt;
    csigned, remsign: Boolean;
begin
  Result := true;

  {check for division by zero}
  if tmpC.is_zero then
    Exit(false);

  {calculate the effictive number of needed division steps
   shifting left tmpB.tmpA until first bit is in remainder (or dividend is 0)}

  nrbits := tmpC.props.mbits;

  // find the first set bit in tmpA and shift it up
  // -> fast initial step since sub would create
  // 0 quotient bits anyways
  while (nrbits > 0) and
        (not tmpA.bit_test( nrbits-1 )) do
    Dec( nrbits, 1 );

  // nrbits > 0 <=> A is non zero
  if nrbits > 0 then
    begin
      // skip leading zeros, since A <> 0 -> also shift in the first bit
      if tmpC.props.mbits-nrbits > 0 then
        tmpA.shl_direct( (tmpC.props.mbits-nrbits)+1 );
      tmpB.intp[0] := tmpB.intp[0] + 1;
      tmpB.props.reset_of_flags;
    end;

  remsign := false;
  csigned := true;

  while nrbits > 0 do
    begin
      if csigned then
        // subtract divisor from remainder
        tfm_temporal_add_B_D
      else
        // add back divisor to remainder
        tfm_temporal_add_B_C;

      // check remainder < 0
      remsign := tmpB.is_signed;

      // shift remainder left (not when nrbits=1, it would be corrected with shr)
      if nrbits > 1 then
        begin
          tmpB.shl_direct( 1 );
          if tmpA.is_signed then
            tmpB.intp[0] := tmpB.intp[0] + 1;
            tmpB.props.reset_of_flags;
        end;

      // shift quotient left and add the new quotient bit
      tmpA.shl_direct( 1 );
      if remsign then
        begin
          // remainder was signed -> add back
          csigned := false;
        end
      else
        begin
          tmpA.intp[0] := tmpA.intp[0] + 1;
          tmpA.props.reset_of_flags;
          // remainder was unsiged -> try another substraction
           csigned := true;
        end;
      Dec( nrbits, 1 );
    end;

  if remsign and remcorrect then
    begin
      // correction: remainder is negative due to previous substraction
      // -> since we stopped, it is not corrected in next phase, so simply correct it now
      // and add back the divisor
      tfm_temporal_add_B_C;
    end;
end;

procedure tfm_mul_temps;
{ multiply tmpC by tmpA

  Basic algorithm

    * F = abs(l) // tmpA
    * M = abs(r) // tmpC
    *
    * for bits do
    *   B sh1 1
    *   if F.msb then
    *     B := B + M;
    *   F shl 1
}
var nrbits, shiftn: VMInt;
    initshift: Boolean;
begin
  initshift := true;
  nrbits := tmpC.props.mbits;
  while not tmpA.is_zero do
    begin
      shiftn := 1;
      // find the next set bit
      while not tmpA.bit_test( tmpA.props.mbits-shiftn ) do
        Inc(shiftn,1);
      Dec(nrbits,shiftn);
      // ignore first shift
      if not initshift then
        begin
          // multiply, for each skipped bit additional
          tmpB.shl_direct( shiftn );
        end
      else
        initshift := false;
      // leading bit is set -> add
      tfm_temporal_add_B_C;
      // shift out leading bit
      tmpA.shl_direct( shiftn );
    end;

  {when stopped because A is zero, we need to do the last missing shifts for
   the "missing" 0 bits -> ignore when initshift, then A=0 and B=A*C}
  if (not initshift) and
     (nrbits > 0) then
    tmpB.shl_direct( nrbits );
end;
