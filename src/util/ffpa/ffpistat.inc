{   Fixed Fix Point Arithmetic integer state and checks

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

procedure TTFM_Integer.reinit;
{ wipe flags and integer buffer }
begin
  props.init(props.mbits);
  FillByte( intp[0], props.sizebytes, 0 );
end;

function TTFM_Integer.is_wordsize: Boolean;
{ check or recalculate fit flags
    * depends on expand
    * depends on sign test }
var i: VMInt;
begin
  if not (of_fwtested in props.flags) then
    begin
      props.set_unset_flags([of_fwtested,if_fitsword],[]);
      if props.mbits > C_TTFM_WORD_BITS then
        begin
          expand;
          if is_signed then
            begin
              {check for "sign bit" in first word}
              if bit_test(C_TTFM_WORD_BITS-1) then
                begin
                  {check the other words, must be "sign extensions"}
                  for i := props.sizewords-1 downto 1 do
                    begin
                      if intp[i] <> TFFPA_Word(not(TFFPA_Word(0))) then
                        begin
                          props.set_unset_flags([of_fwtested],[if_fitsword]);
                          break;
                        end;
                    end;
                end
              else
                props.set_unset_flags([of_fwtested],[if_fitsword]);
            end
          else
            begin
              {check for "sign bit" in first word}
              if not bit_test(C_TTFM_WORD_BITS-1) then
                begin
                  {check the other words, must be "sign extensions"}
                  for i := props.sizewords-1 downto 1 do
                    begin
                      if intp[i] <> 0 then
                        begin
                          props.set_unset_flags([of_fwtested],[if_fitsword]);
                          break;
                        end;
                    end;
                end
              else
                props.set_unset_flags([of_fwtested],[if_fitsword]);
            end;
        end;
    end;
  Result := if_fitsword in props.flags;
end;

function TTFM_Integer.is_zero: Boolean;
{ check or recalculate zero flags
  * depends on expand }
var i: VMInt;
begin
  if not (of_zerotested in props.flags) then
    begin
      expand;
      if not (of_zerotested in props.flags) then // may be set by expands signtest
        begin
          for i := 0 to props.sizewords-1 do
            begin
              if intp[i] <> 0 then
                begin
                  props.set_unset_flags( [of_zerotested], [if_zero] );
                  Exit(false);
                end;
            end;
          props.set_unset_flags( [of_zerotested,if_zero], [] );
        end;
    end;
  Result := if_zero in props.flags;
end;

function TTFM_Integer.is_signed: Boolean;
{ check or recalculate sign flags
  * depends on nothing
  * set zero flag if signed -> signed numbers are never zero }
begin
  if not (of_signtested in props.flags) then
    begin
      if bit_test( props.mbits - 1 ) then
        props.set_unset_flags( [of_signtested,if_signed,of_zerotested], [if_zero] )
      else
        props.set_unset_flags( [of_signtested], [if_signed] );
    end;
  Result := if_signed in props.flags;
end;

procedure TTFM_Integer.expand;
{ sign extend higher part to word boundary
  * depends on cf_hiwb
  * depends on sign test
  * resets [of_uppertested,if_expanded] }
var mom: VMWord;
    mw: VMInt;
begin
  if not ( cf_hiwb in props.flags ) then
    begin
      if (props.flags * [of_uppertested,if_expanded]) <> [of_uppertested,if_expanded] then
        begin
          mom := props.mw_cutoff;
          ASSERT(mom > 0); // since not word boundary
          {expand bits above m}
          mw := props.sizewords-1;
          if is_signed then
            intp[mw] := TFFPA_Word(TFFPA_Word(not TFFPA_Word(0)) shl TFFPA_Word(mom)) or intp[mw]
          else
            intp[mw] := TFFPA_Word(not (TFFPA_Word(not TFFPA_Word(0)) shl TFFPA_Word(mom))) and intp[mw];
        end;
    end;
  props.set_unset_flags( [of_uppertested,if_expanded], [] );
end;

procedure TTFM_Integer.cut;
{ cut bits above sign bit to word boundary
  * depends on cf_hiwb
  * depends on sign test
  * resets [of_uppertested,if_expanded] }
var mom: VMWord;
    mw: VMInt;
begin
  if cf_hiwb in props.flags then
    begin
      props.set_unset_flags( [of_uppertested,if_expanded], [] );
      Exit;
    end;

  // if signed -> since not word bound, cut removes the sign extension
  // if unsigned -> also sign (zero) extended
  if is_signed then
    props.set_unset_flags( [of_uppertested], [if_expanded] )
  else
    props.set_unset_flags( [of_uppertested,if_expanded], [] );

  mom := props.mw_cutoff;
  ASSERT(mom > 0); // since not word boundary
  {cut bits above m}
  mw := props.sizewords-1;
  intp[mw] := TFFPA_Word(not (TFFPA_Word(not TFFPA_Word(0)) shl TFFPA_Word(mom))) and intp[mw];
end;

procedure TTFM_Integer.set_signbit( signed: Boolean );
{ directly set/reset sign bit, this is NOT abs/neg, its used for
  arithmetic reranging operations like f.e.
  arithmetic shift left/right which preserves the sign bit by setting it
  back after non-arithmetic operation

  * depends on sign test
  * resets [of_signtested,if_signed] (due to is_signed check or direct setting)}
var mom: VMWord;
    mw: VMInt;
begin
  if is_signed <> signed then
    begin
      // simply expand inclusive sign bit
      props.reset_of_flags;
      mom := (props.mbits-1) and (C_TTFM_WORD_BITS-1);
      mw := props.sizewords-1;
      if signed then
        begin
          intp[mw] := TFFPA_Word(TFFPA_Word(not TFFPA_Word(0)) shl TFFPA_Word(mom)) or intp[mw];
          props.set_unset_flags( [of_signtested,if_signed,of_uppertested,if_expanded], [] );
        end
      else
        begin
          intp[mw] := TFFPA_Word(not (TFFPA_Word(not TFFPA_Word(0)) shl TFFPA_Word(mom))) and intp[mw];
          props.set_unset_flags( [of_signtested,of_uppertested,if_expanded], [] );
        end;
    end;
end;

function TTFM_Integer.logic_expanded_word( w: VMInt ): TFFPA_Word;
{ logically expand the number and return word w
    -> create sign extended upper words
  does not expand the actual number, this must be done by caller}
begin
  {convert to position for this number}
  if (w < props.sizewords) then
    begin
      {word position is in range, return the word}
      Result := intp[w];
    end
  else
    begin
      {word position is above -> sign extend}
      if is_signed then
        Result := TFFPA_Word(not TFFPA_Word(0))
      else
        Result := 0;
    end;
end;
