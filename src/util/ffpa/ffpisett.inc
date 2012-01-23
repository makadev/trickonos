{   Fixed Fix Point Arithmetic integer properties

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

procedure TTFM_Integer_Properties.init( bits: VMInt );
begin
  ASSERT(bits >= 1);
  mbits := bits;
  mwords := VMWord(VMWord(mbits-1) div VMWord(C_TTFM_WORD_BITS)) + 1;
  flags := [];
  reset_cf;
  reset_of_flags;
end; 

function TTFM_Integer_Properties.sizewords: VMInt;
begin
  Result := mwords;
end;

function TTFM_Integer_Properties.sizebytes: VMInt;
begin
  Result := mwords * C_TTFM_WORD_BYTES;
end;

function TTFM_Integer_Properties.mw_cutoff: VMInt;
begin
  Result := VMWord(VMWord(mbits) mod VMWord(C_TTFM_WORD_BITS));
end;

procedure TTFM_Integer_Properties.reset_cf;
{ reset cf flags }
begin
  if mw_cutoff <= 0 then
    flags := flags + [cf_hiwb]
  else
    flags := flags - [cf_hiwb];
end;

procedure TTFM_Integer_Properties.reset_of_flags( exceptf: TTFMIFlagSet = [] );
{ reset operation flags, except exceptf }
begin
  flags := flags - (C_OperationFlags - exceptf);
end;

procedure TTFM_Integer_Properties.set_unset_flags( setf, unsetf: TTFMIFlagSet );
{ flagging (flags - unsetf) + setf }
begin
  flags := (flags - unsetf) + setf;
end;
    
