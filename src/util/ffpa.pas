{   Unit for Fixed Fix Point Arithmetic (fix point numbers with fixed length)

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

unit ffpa;

{$MODE OBJFPC}{$H+}

interface

uses SysUtils, commontl;

type
  TFFPA_Word = MachineWord;

{ max sizes }
const
  C_TTFM_WORD_BITS = SizeOf(TFFPA_Word)*8;
  C_TTFM_WORD_BYTES = SizeOf(TFFPA_Word);

  C_TTFM_MAX = 65536;
  C_TTFM_MAX_BYTES = C_TTFM_MAX div 8;
  C_TTFM_MAX_WORDS = C_TTFM_MAX_BYTES div C_TTFM_WORD_BYTES;


type
  { (max) word buffer }
  TTFMWords = packed array[0..C_TTFM_MAX_WORDS-1] of TFFPA_Word;
  PTFMWords = ^TTFMWords;

  { often checked states/flags calculated/set while operating }
  TTFMIFlags = (
    { word boundary correction }

    // higher part (before comma) is word bound - must always be correct
    cf_hiwb,

    // cf_hiwb or operand was expanded since last operation
    of_uppertested,
    // operands hi part is expanded (sign extended to word boundary)
    if_expanded,

    { word ranging }

    // operand was tested for word size
    of_fwtested,
    // number fits into Q.wordsize.0 without loss of precision
    if_fitsword,

    { sign test }

    // number was tested for sign after last operation
    of_signtested,
    // number is signed
    if_signed,

    { zero test }

    // number was compared against 0 after last operation
    of_zerotested,
    // number is zero
    if_zero
  );

  TTFMIFlagSet = set of TTFMIFlags;

const
  { operation flags, i.g. destroyed on operation }
  C_OperationFlags = [
    of_uppertested,
    of_fwtested,
    of_signtested,
    of_zerotested
  ];

  { constructive flags, i.g. destroyed/set on copy/construction }
  C_ConstructionFlags = [
    cf_hiwb
  ];

  { short flags, mostly for debug output }
  C_Flags_Short: array[TTFMIFlags] of String = (
    'W_' {cf_hiwb},
    '=H' {of_uppertested},
    'E_' {if_expanded},
    '=F' {of_fwtested},
    '_F' {if_fitsword},
    '=S' {of_signtested},
    'S_' {if_signed},
    '=Z' {of_zerotested},
    '_Z' {if_zero}
  );

type
  TTFM_Integer_Properties = packed object
    private
      mbits: VMInt;
      mwords: VMInt;
      flags: TTFMIFlagSet;
    public
      property NrBits: VMInt read mbits;

      procedure init( bits: VMInt );
      { number of words used to contain mbits bits }
      function sizewords: VMInt;
      { number of bytes used to contain mbits bits }
      function sizebytes: VMInt;
      { number of bits in the last word, which are not part of the number }
      function mw_cutoff: VMInt;
      { set/unset creation flags }
      procedure reset_cf;
      { reset operation flags, except exceptf }
      procedure reset_of_flags( exceptf: TTFMIFlagSet = [] );
      { flagging (flags - unsetf) + setf }
      procedure set_unset_flags( setf, unsetf: TTFMIFlagSet ); // <- very carefull with that
  end;

  PTFM_Integer = ^TTFM_Integer;
  TTFM_Integer = object
    private
      props: TTFM_Integer_Properties;
      intp: TTFMWords;
    public
      property Properties: TTFM_Integer_Properties read props;

      { zero init number, wipe flags (props.mbits must be correct) }
      procedure reinit;

      { check or recalculate zero flags }
      function is_zero: Boolean;

      { check or recalculate sign flags }
      function is_signed: Boolean;

      { check or recalculate fit flags }
      function is_wordsize: Boolean;

      { sign extend higher part to word boundary }
      procedure expand;

      { cut bits above sign bit to word boundary }
      procedure cut;

      { directly set/reset sign bit, this is NOT abs/neg, its used for
        arithmetic reranging operations like f.e.
        arithmetic shift left/right which preserves the sign bit by setting it
        back after non-arithmetic operation }
      procedure set_signbit( signed: Boolean );

      { logically expand the number and return word w
          -> create sign extended upper words }
      function logic_expanded_word( w: VMInt ): TFFPA_Word;
      
      (* direct operations *)

      { compare against number, returns = 0 if both are equal,
        < 0 if this is less, > 0 if this is greater
        abs(result) is the topmost differing word}
      function compare( opr: PTFM_Integer ): VMInt;

      { check if bit is set where 2^bit is the bitn
          -> m part tested with positive <bit> }
      function bit_test( bit: VMInt ): Boolean;
      
      { copy number part (logically expanded) }
      procedure copy_num( opr: PTFM_Integer );

      { calculate 2's complement }
      procedure twoscomplement;

      { complement }
      procedure not_direct;

      { direct and into this }
      procedure and_direct( opr: PTFM_Integer );
      { direct or into this }
      procedure or_direct( opr: PTFM_Integer );
      { direct xor into this }
      procedure xor_direct( opr: PTFM_Integer );
      { direct add into this }
      procedure add_direct( opr: PTFM_Integer );

      { shl by words }
      procedure shl_directwordsize( nrwshift: VMInt );
      { shr by words, arithmetic extend (expand sign) if arith }
      procedure shr_directwordsize( nrwshift: VMInt; arith: Boolean );
      
      { shl by shiftm bits }
      procedure shl_direct( shiftm: VMInt );
      { shr by shiftm bits, do sar (sign extend) with arith }
      procedure shr_direct( shiftm: VMInt; arith: Boolean );
  end;

(* Helpers *)

//DOC>> calculate result upgrade, returns true if resulting size is in different format as op1m and op2m
function max_upgrade( op1m, op2m: VMInt; out resupm: VMInt ): Boolean;
//DOC>> calculate result upgrade, returns true if resulting size is in different format as op1 and op2
function max_upgrade( op1, op2: PTFM_Integer; out resupm: VMInt ): Boolean;

(* Conversion *)

//DOC>> load Integer 0 with precision m
function tfm_load_null( m: VMInt = 32 ): PTFM_Integer;
//DOC>> convert Integer val into Integer with m bits precision
function tfm_load_int( val: VMInt; m: VMInt = 32 ): PTFM_Integer;

//DOC>> create decimal string for nr, add separator '_' every 10^sep steps
function tfm_to_string( nr: PTFM_Integer; sep: VMInt = 0 ): String;
//DOC>> load Integer from decimal string: Format [+|-]? [0-9_]+
function tfm_from_string( num: String; bits: VMInt ): PTFM_Integer;

//DOC>> create hexadecimal string, using at most digits digits or all when digits <= 0 (strips leading zeros)
function tfm_to_hex( nr: PTFM_Integer; sep: VMInt = 0; digits: VMInt = 0 ): String;
//DOC>> load Integer from hex string [+|-]? [0-9a-fA-F_]+
function tfm_from_hex( hexstring: String; bits: VMInt ): PTFM_Integer;

//DOC>> create number info string, containing flags and bitness
function tfm_nrinfostr( ptfmint: PTFM_Integer ): String;

(* Alloc / Copy / Release *)

// alloc and wipe a new number
function tfm_alloc( m: VMInt; wipe: Boolean = true ): PTFM_Integer;

// alloc and wipe a new number with upgraded size depending on op1/op2
function tfm_allocextop( op1, op2: PTFM_Integer; wipe: Boolean = true ): PTFM_Integer;

// release mem
procedure tfm_release( ptfmint: PTFM_Integer );

(* Logic *)

function tfm_not( ptfmop: PTFM_Integer ): PTFM_Integer;
function tfm_and( ptfmintl, ptfmintr: PTFM_Integer ): PTFM_Integer;
function tfm_or( ptfmintl, ptfmintr: PTFM_Integer ): PTFM_Integer;
function tfm_xor( ptfmintl, ptfmintr: PTFM_Integer ): PTFM_Integer;
function tfm_shl( ptfmop: PTFM_Integer; shiftm: VMInt ): PTFM_Integer;
function tfm_shr( ptfmop: PTFM_Integer; shiftm: VMInt ): PTFM_Integer;
function tfm_sar( ptfmop: PTFM_Integer; shiftm: VMInt ): PTFM_Integer;

function tfm_rol( ptfmop: PTFM_Integer; shiftm: VMInt ): PTFM_Integer;
function tfm_ror( ptfmop: PTFM_Integer; shiftm: VMInt ): PTFM_Integer;

(* Arithmetic *)

function tfm_abs( ptfmop: PTFM_Integer ): PTFM_Integer;
function tfm_neg( ptfmop: PTFM_Integer ): PTFM_Integer;
function tfm_add( ptfmintl, ptfmintr: PTFM_Integer ): PTFM_Integer;
function tfm_sub( ptfmintl, ptfmintr: PTFM_Integer ): PTFM_Integer;
function tfm_div( ptfmintl, ptfmintr: PTFM_Integer ): PTFM_Integer;
function tfm_mod( ptfmintl, ptfmintr: PTFM_Integer ): PTFM_Integer;
function tfm_mul( ptfmintl, ptfmintr: PTFM_Integer ): PTFM_Integer;

implementation

(* Temporary Variables (mul/div/mod/rol/ror/..) *)

var
  tmpA: TTFM_Integer; // div quotient/dividend // mul multiplier (first operand, bits direct adding)
  tmpB: TTFM_Integer; // div remainder         // mul sum of multipliend (result)
  tmpC: TTFM_Integer; // div abs(divisor)      // mul multipliend (second operand, added multiple times)
  tmpD: TTFM_Integer; // div -abs(divisor)     // --

function max_upgrade( op1m, op2m: VMInt; out resupm: VMInt ): Boolean;
{calculate result upgrade}
begin
  if op1m >= op2m then
    begin
      Result := op1m <> op2m;
      resupm := op1m;
    end
  else
    begin
      Result := true;
      resupm := op2m;
    end;
end;

function max_upgrade( op1, op2: PTFM_Integer; out resupm: VMInt ): Boolean;
begin
  Result := max_upgrade( op1^.props.mbits, op2^.props.mbits, resupm );
end;

function tfm_alloc( m: VMInt; wipe: Boolean ): PTFM_Integer;
{ allocate number,
    * if wipe, flags and number buffer is zeroed and creation flags set
    * if no wipe, buffer is not zeroed }
begin
  Result := nil;
  if m <= 1 then
    m := 2;
  if m > C_TTFM_MAX then
    m := C_TTFM_MAX;
  Result := GetMem(((((m-1) div C_TTFM_WORD_BITS) + 1)*C_TTFM_WORD_BYTES + SizeOf(TTFM_Integer)) - SizeOf(TTFMWords));
  Result^.props.init(m);
  //if wipe then
    Result^.reinit;
end;

function tfm_allocextop( op1, op2: PTFM_Integer; wipe: Boolean ): PTFM_Integer;
{ calc upgrade for op1/op2 and alloc }
var newm: Integer;
begin
  max_upgrade(op1,op2,newm);
  Result := tfm_alloc(newm,wipe);
end;

procedure tfm_release( ptfmint: PTFM_Integer );
begin
  Freemem(ptfmint);
end;

{$I ffpa/ffpisett.inc}
{$I ffpa/ffpistat.inc}
{$I ffpa/ffpidiop.inc}
{$I ffpa/ffpamulo.inc}
{$I ffpa/ffpconv.inc}
{$I ffpa/ffpawrap.inc}

end.
