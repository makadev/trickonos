{   Fixed Fix Point operation wrappers

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

function tfm_not( ptfmop: PTFM_Integer ): PTFM_Integer;
begin
  Result := tfm_alloc( ptfmop^.props.mbits, false );
  Result^.copy_num( ptfmop );
  Result^.not_direct;
end;

function tfm_abs( ptfmop: PTFM_Integer ): PTFM_Integer;
begin
  Result := tfm_alloc( ptfmop^.props.mbits, false );
  Result^.copy_num( ptfmop );
  if ptfmop^.is_signed then
    Result^.twoscomplement;
end;

function tfm_neg( ptfmop: PTFM_Integer ): PTFM_Integer;
begin
  Result := tfm_alloc( ptfmop^.props.mbits, false );
  Result^.copy_num( ptfmop );
  if not ptfmop^.is_signed then
    Result^.twoscomplement;
end;

function tfm_and( ptfmintl, ptfmintr: PTFM_Integer ): PTFM_Integer;
begin
  Result := tfm_allocextop( ptfmintl, ptfmintr, false );
  Result^.copy_num( ptfmintl ); // copy load
  Result^.and_direct( ptfmintr ); // and
end;

function tfm_or( ptfmintl, ptfmintr: PTFM_Integer ): PTFM_Integer;
begin
  Result := tfm_allocextop( ptfmintl, ptfmintr, false );
  Result^.copy_num( ptfmintl ); // copy load
  Result^.or_direct( ptfmintr ); // or
end;

function tfm_xor( ptfmintl, ptfmintr: PTFM_Integer ): PTFM_Integer;
begin
  Result := tfm_allocextop( ptfmintl, ptfmintr, false );
  Result^.copy_num( ptfmintl ); // copy load
  Result^.xor_direct( ptfmintr ); // xor
end;

function tfm_shl( ptfmop: PTFM_Integer; shiftm: VMInt ): PTFM_Integer;
begin
  ASSERT(shiftm >= 0);
  Result := tfm_alloc( ptfmop^.props.mbits, false );
  Result^.copy_num( ptfmop ); // copy load
  Result^.shl_direct( shiftm ); // shift
end;

function tfm_shr( ptfmop: PTFM_Integer; shiftm: VMInt ): PTFM_Integer;
begin
  ASSERT(shiftm >= 0);
  Result := tfm_alloc( ptfmop^.props.mbits, false );
  Result^.copy_num( ptfmop ); // copy load
  Result^.shr_direct( shiftm, false ); // shift
end;

function tfm_sar( ptfmop: PTFM_Integer; shiftm: VMInt ): PTFM_Integer;
begin
  ASSERT(shiftm >= 0);
  Result := tfm_alloc( ptfmop^.props.mbits, false );
  Result^.copy_num( ptfmop ); // copy load
  Result^.shr_direct( shiftm, true ); // shift
end;

function tfm_rol( ptfmop: PTFM_Integer; shiftm: VMInt ): PTFM_Integer;
begin
  ASSERT(shiftm >= 0);
  Result := tfm_alloc( ptfmop^.props.mbits, false );
  Result^.copy_num( ptfmop ); // copy load
  shiftm := shiftm mod ptfmop^.props.mbits;
  if shiftm <= 0 then
    Exit(Result);
  tmpD.props.Init( ptfmop^.props.mbits );
  tmpD.copy_num( ptfmop ); // copy load
  Result^.shl_direct( shiftm ); // shift left first part
  tmpD.shr_direct( (ptfmop^.props.mbits-shiftm), false ); // shift right second part
  Result^.or_direct( @tmpD ); // or together
end;

function tfm_ror( ptfmop: PTFM_Integer; shiftm: VMInt ): PTFM_Integer;
begin
  ASSERT(shiftm >= 0);
  Result := tfm_alloc( ptfmop^.props.mbits, false );
  Result^.copy_num( ptfmop ); // copy load
  shiftm := shiftm mod ptfmop^.props.mbits;
  if shiftm <= 0 then
    Exit(Result);
  tmpD.props.Init( ptfmop^.props.mbits );
  tmpD.copy_num( ptfmop ); // copy load
  Result^.shr_direct( shiftm, false ); // shift right first part
  tmpD.shl_direct( (ptfmop^.props.mbits-shiftm) ); // shift left second part
  Result^.or_direct( @tmpD ); // or together
end;

function tfm_add( ptfmintl, ptfmintr: PTFM_Integer ): PTFM_Integer;
begin
  Result := tfm_allocextop( ptfmintl, ptfmintr, false );
  Result^.copy_num( ptfmintl );
  Result^.add_direct( ptfmintr );
end;

function tfm_sub( ptfmintl, ptfmintr: PTFM_Integer ): PTFM_Integer;
begin
  Result := tfm_allocextop( ptfmintl, ptfmintr, false );
  Result^.copy_num( ptfmintr );
  Result^.twoscomplement;
  Result^.add_direct( ptfmintl );
end;

function tfm_div( ptfmintl, ptfmintr: PTFM_Integer ): PTFM_Integer;
begin
  tfm_tmpload_division( ptfmintl, ptfmintr );
  if tfm_div_temps( false ) then
    begin
      Result := tfm_allocextop( ptfmintl, ptfmintr, false );
      Result^.copy_num( @tmpA );
      { division will be performed on positive numbers, explicit sign quotient if
        needed }
      if ptfmintl^.is_signed xor ptfmintr^.is_signed then
        Result^.twoscomplement;
    end
  else
    Result := nil; // div by zero
end;

function tfm_mod( ptfmintl, ptfmintr: PTFM_Integer ): PTFM_Integer;
begin
  tfm_tmpload_division( ptfmintl, ptfmintr );
  if tfm_div_temps( true ) then
    begin
      Result := tfm_allocextop( ptfmintl, ptfmintr, false );
      Result^.copy_num( @tmpB );
      { sign reminder with divisor sign }
      if ptfmintr^.is_signed then
        Result^.twoscomplement;
    end
  else
    Result := nil; // div by zero
end;

function tfm_mul( ptfmintl, ptfmintr: PTFM_Integer ): PTFM_Integer;
begin
  tfm_tmpload_multiplication( ptfmintl, ptfmintr );
  tfm_mul_temps;
  Result := tfm_allocextop( ptfmintl, ptfmintr, false );
  Result^.copy_num( @tmpB );
  { multiplication will be performed on positive numbers,
    explicit sign product if needed }
  if ptfmintl^.is_signed xor ptfmintr^.is_signed then
    Result^.twoscomplement;
end;
