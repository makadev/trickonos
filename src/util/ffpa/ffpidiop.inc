{   Fixed Fix Point Arithmetic direct integer operations

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

function TTFM_Integer.compare( opr: PTFM_Integer ): VMInt;
{ compare against number, returns = 0 if both are equal,
  < 0 if this is less, > 0 if this is greater }
var lw,rw: TFFPA_Word;
    i: VMInt;
begin
  if opr^.props.sizewords > props.sizewords then
    i := opr^.props.sizewords
  else
    i := props.sizewords;
  {sign test}
  if is_signed xor opr^.is_signed then
    begin
      if is_signed then
        Exit(-i)
      else
        Exit(i);
    end;
  {same sign test, componentwise
   since we have 2s complement numbers, simply compare the words}
  expand;
  opr^.expand;
  for i := i downto 1 do
    begin
      lw := logic_expanded_word(i-1);
      rw := opr^.logic_expanded_word(i-1);
      if lw <> rw then
        begin
          if lw < rw then
            Exit(-i)
          else
            Exit(i);
        end;
    end;
  // no missmatch
  Result := 0;
end;

procedure TTFM_Integer.copy_num( opr: PTFM_Integer );
var i: VMInt;
begin
  opr^.expand; // needed since range "logically" expanded
  for i := 0 to props.sizewords-1 do
    intp[i] := opr^.logic_expanded_word( i );
  props.reset_of_flags;
end;

procedure TTFM_Integer.not_direct;
{ calculate (first) complement }
var i: VMInt;
begin
  for i := 0 to props.sizewords-1 do
    intp[i] := not intp[i];
  props.reset_of_flags;
end;

function TTFM_Integer.bit_test( bit: VMInt ): Boolean;
{ check if bit is set where 2^bit is the bitn }
var mask: TFFPA_Word;
    wpos: VMInt;
begin
  ASSERT((bit >= 0) and
         (bit < props.mbits));
  wpos := (VMWord(bit) div VMWord(C_TTFM_WORD_BITS));
  mask := TFFPA_Word(1) shl TFFPA_Word(TFFPA_Word(bit) mod TFFPA_Word(C_TTFM_WORD_BITS));
  Result := (intp[wpos] and mask) > 0;
end;

procedure TTFM_Integer.twoscomplement;
{ calculate 2s complement }
var i: VMInt;
begin
  not_direct;
  for i := 0 to props.sizewords-1 do
    begin
{$PUSH}
{$R-}
{$Q-}
      Inc(intp[i],1);
{$POP}
      if intp[i] <> 0 then
        Break; // number wont propagate
    end;
  props.reset_of_flags;
end;

procedure TTFM_Integer.and_direct( opr: PTFM_Integer );
{ direct and into this, oper is (obviously) not reranged }
var w: VMInt;
begin
  opr^.expand; // needed since range "logically" expanded
  for w := 0 to props.sizewords-1 do
    intp[w] := intp[w] and opr^.logic_expanded_word(w);
  props.reset_of_flags;
end;

procedure TTFM_Integer.or_direct( opr: PTFM_Integer );
{ direct or into this, oper is (obviously) not reranged }
var w: VMInt;
begin
  opr^.expand; // needed since range "logically" expanded
  for w := 0 to props.sizewords-1 do
    intp[w] := intp[w] or opr^.logic_expanded_word(w);
  props.reset_of_flags;
end;

procedure TTFM_Integer.xor_direct( opr: PTFM_Integer );
{ direct xor into this, oper is (obviously) not reranged }
var w: VMInt;
begin
  opr^.expand; // needed since range "logically" expanded
  for w := 0 to props.sizewords-1 do
    intp[w] := intp[w] xor opr^.logic_expanded_word(w);
  props.reset_of_flags;
end;

procedure TTFM_Integer.add_direct( opr: PTFM_Integer );
{direct add oper into this}
var nw: TFFPA_Word;
    w: VMInt;
    carry: Boolean;
begin
  opr^.expand;
  carry := false;
  for w := 0 to props.sizewords-1 do
    begin
      nw := opr^.logic_expanded_word(w);
      if carry then
{$PUSH}
{$R-}
{$Q-}
        begin
          intp[w] := intp[w] + nw + 1;
          carry := nw = 0;
        end
      else
        begin
          intp[w] := intp[w] + nw;
          carry := intp[w] < nw;
        end;
{$POP}
    end;
  props.reset_of_flags;
end;

procedure TTFM_Integer.shl_directwordsize( nrwshift: VMInt );
{shift left, wordsize}
var nw, w: VMInt;
begin
  if nrwshift > 0 then
    begin
      nw := props.sizewords-1;
      if nrwshift <= nw then
        begin
          {shift words from w at w+nrwshift}
          for w := nw-nrwshift downto 0 do
            intp[ w + nrwshift ] := intp[ w ];
          {clear 0..nrwshift-1 words}
          FillByte( intp[0], nrwshift*C_TTFM_WORD_BYTES, 0 );
        end
      else
        FillByte( intp[0], props.sizebytes, 0 );
      props.reset_of_flags;
    end;
end;

procedure TTFM_Integer.shr_directwordsize( nrwshift: VMInt; arith: Boolean );
{shift right, wordsize}
var w,nw: VMInt;
    s: Boolean;
begin
  if nrwshift > 0 then
    begin
      expand;
      s := arith and is_signed;
      nw := props.sizewords-1;
      if nrwshift <= nw then
        begin
          {shift words from nrwshift+w to w}
          for w := nrwshift to nw do
            intp[ w - nrwshift ] := intp[ w ];
          {sign extend wordwise}
          w := (nw+1)-nrwshift;
          if s then
            FillByte( intp[w], nrwshift*C_TTFM_WORD_BYTES, $FF )
          else
            FillByte( intp[w], nrwshift*C_TTFM_WORD_BYTES, 0 );
        end
      else
        begin
          {sign "extend", everything shifted out so wipe with sign}
          if s then
            FillByte( intp[0], props.sizebytes, $FF )
          else
            FillByte( intp[0], props.sizebytes, 0 );
        end;
      props.reset_of_flags;
    end;
end;

procedure TTFM_Integer.shl_direct( shiftm: VMInt );
{ shl by shiftm bits }
var ws, wm, a: TFFPA_Word;
    w, nw: VMInt;
begin
  ws := shiftm div TFFPA_Word(C_TTFM_WORD_BITS);
  a := shiftm - (ws*TFFPA_Word(C_TTFM_WORD_BITS));

  { big shift }
  shl_directwordsize( ws );

  { small shifts by amount a
    * check for a
    * check if big shift cleared everything }
  nw := props.sizewords-1;
  if (a > 0) and
     (ws <= nw) then
    begin
      // load
      ws := intp[0];
      // wm -> previous part that needs shift in
      intp[0] := ( ws shl a );
      if nw > 0 then
        begin
          {optimize by ws, not always needed}
          for w := 1 to nw do
            begin
              // wm -> current part that is shifted out
              wm := ws shr TFFPA_Word(C_TTFM_WORD_BITS-a);
              // load
              ws := intp[w];
              // wm -> previous part that needs shift in
              intp[w] := ( ws shl a ) or wm;
            end;
        end;
      props.reset_of_flags;
    end;
end;


procedure TTFM_Integer.shr_direct( shiftm: VMInt; arith: Boolean );
{ shr by shiftm bits, do sar (sign extend) with arith }
var ws, wm, a: TFFPA_Word;
    w, nw: VMInt;
    s: Boolean;
begin
  s := arith and is_signed;
  ws := shiftm div TFFPA_Word(C_TTFM_WORD_BITS);
  a := shiftm - (ws*TFFPA_Word(C_TTFM_WORD_BITS));

  { big shift }
  shr_directwordsize( ws, arith );

  { small shifts by amount a }
  nw := props.sizewords-1;
  if (a > 0) and
     (ws <= nw) then
    begin
      // expand/cut, otherwise may shift in garbage
      if s then
        begin
          // sar on signed -> shift in 1
          expand;
          // load
          ws := intp[nw];
          // wm -> previous part that needs shift in
          intp[nw] := ( ws shr a ) or (TFFPA_Word(0) shl TFFPA_Word(C_TTFM_WORD_BITS-a));
        end
      else
        begin
          // sar on positive or shr -> shift in sign, extend with 0
          cut;
          // load
          ws := intp[nw];
          // wm -> previous part that needs shift in
          intp[nw] := ( ws shr a );
        end;
      if nw > 0 then
        begin
          {optimize by ws, not always needed}
          for w := nw-1 downto 0 do
            begin
              // wm -> current part that is shifted out
              wm := ws shl TFFPA_Word(C_TTFM_WORD_BITS-a);
              // load
              ws := intp[w];
              // wm -> previous part that needs shift in
              intp[w] := ( ws shr a ) or wm;
            end;
        end;
      props.reset_of_flags;
    end;
end;
