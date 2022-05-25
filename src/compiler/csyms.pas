{   Unit for scanner symbols, sets and base declarations

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

unit csyms;

{$mode objfpc}{$H+}
{$CODEPAGE UTF8}

interface

uses
  SysUtils, commontl, eomsg, ucfs, coreobj;

type
  TSMESymbol = (
    (* Scanned *)

    {Error}
    SMES_ERROR=0,
    {End Of Stream}
    SMES_EOS,
    {CODE Marker (IntroMarker)}
    SMES_CODE_INTRO,
    {CODE Marker (OutroMarker)}
    SMES_CODE_OUTRO,
    {Put Line}
    SMES_PUT,

    S__START_SPECIAL,
    SMES_ID,       // id (not reserved)
    SMES_String,   // '<chars>' or "<chars>"
    SMES_Int,      // number, decimal
    SMES_IntHex,   // number, hexadecimal
    SMES_IntOct,   // number, octal
    SMES_IntBin,   // number, binary
    S__END_SPECIAL,

    S__START_KEY,
    SMES_NIL,       // nil
    SMES_FALSE,     // false
    SMES_TRUE,      // true

    SMES_AND,       // and
    SMES_OR,        // or
    SMES_XOR,       // xor
    SMES_NOT,       // not
    SMES_IS,        // is
    SMES_DIV,       // div
    SMES_MOD,       // mod
    SMES_SHLK,      // shl
    SMES_SHRK,      // shr
    SMES_ROLK,      // rol
    SMES_RORK,      // ror

    SMES_CLASS,     // class
    SMES_FUNCTION,  // function
    SMES_BEGIN,     // begin
    SMES_VAR,       // var
    SMES_IF,        // if
    SMES_ELSEIF,    // elseif
    SMES_ELSE,      // else
    SMES_END,       // end
    SMES_THEN,      // then
    SMES_DO,        // do
    SMES_WHILE,     // while
    SMES_REPEAT,    // repeat
    SMES_UNTIL,     // until
    SMES_FOREACH,   // foreach
    SMES_IN,        // in
    S__END_KEY,

    S__START_OPER,
    SMES_ORound,    // (
    SMES_CRound,    // )
    SMES_Comma,     // ,
    SMES_Colon,     // :
    SMES_SColon,    // ;
    SMES_OSquare,   // [
    SMES_CSquare,   // ]
    SMES_OCurly,    // {
    SMES_CCurly,    // }
    SMES_Dot,       // .
    SMES_Plus,      // +
    SMES_Minus,     // -
    SMES_Star,      // *
    {SMES_Slash,     // /}
    SMES_OAngel,    // <
    SMES_CAngel,    // >
    SMES_Assign,    // :=
    SMES_LE,        // <=
    SMES_GE,        // >=
    SMES_EQ,        // =
    SMES_NEQ,       // <>
    SMES_SHL,       // <<
    SMES_SHR,       // >>
    SMES_ROL,       // <<<
    SMES_ROR,       // >>>
    S__END_OPER,

    S__TERMINATOR
  );

const
  C_Reserved_Symbol: array[Ord(S__START_KEY)+1..Ord(S__END_KEY)-1] of String =
    (
      'NIL',
      'FALSE',
      'TRUE',
      'AND',
      'OR',
      'XOR',
      'NOT',
      'IS',
      'DIV',
      'MOD',
      'SHL',
      'SHR',
      'ROL',
      'ROR',
      'CLASS',
      'FUNCTION',
      'BEGIN',
      'VAR',
      'IF',
      'ELSEIF',
      'ELSE',
      'END',
      'THEN',
      'DO',
      'WHILE',
      'REPEAT',
      'UNTIL',
      'FOREACH',
      'IN'
    );

  C_Char_Capital_a = Ord('A');
  C_Char_Capital_z = Ord('Z');
  C_Char_Small_a = Ord('a');
  C_Char_Small_z = Ord('z');

  C_Char_Small_q = Ord('q');

  C_Char_Small_f = Ord('f');
  C_Char_Capital_f = Ord('F');

  C_Char_Underscore = Ord('_');

  C_Char_Digit_Zero = Ord('0');
  C_Char_Digit_One = Ord('1');
  C_Char_Digit_Seven = Ord('7');
  C_Char_Digit_Nine = Ord('9');

  C_Char_Dollar = Ord('$');
  C_Char_Percent = Ord('%');
  C_Char_Ampersand = Ord('&');

  C_Char_SQuote = Ord('''');
  C_Char_DQuote = Ord('"');

  C_Char_BackSlash = Ord('\');
  C_Char_Small_t = Ord('t');
  C_Char_Small_n = Ord('n');
  C_Char_Small_l = Ord('l');
  C_Char_Small_u = Ord('u');

  C_Char_Plus = Ord('+');
  C_Char_Minus = Ord('-');
  C_Char_Star = Ord('*');
  C_Char_Dot = Ord('.');
  C_Char_Comma = Ord(',');
  C_Char_Colon = Ord(':');
  C_Char_SColon = Ord(';');
  C_Char_Equ = Ord('=');
  C_Char_LRPar = Ord('(');
  C_Char_RRPar = Ord(')');
  C_Char_LSPar = Ord('[');
  C_Char_RSPar = Ord(']');
  C_Char_LCPar = Ord('{');
  C_Char_RCPar = Ord('}');
  C_Char_LAngel = Ord('<');
  C_Char_RAngel = Ord('>');


  C_Set_ID_Follow = [C_Char_Small_a..C_Char_Small_z,
                     C_Char_Capital_a..C_Char_Capital_z,
                     C_Char_Digit_Zero..C_Char_Digit_Nine,
                     C_Char_Underscore];

  C_Set_Num_Follow = [C_Char_Digit_Zero..C_Char_Digit_Nine];
  C_Set_HexNum_Follow = [C_Char_Digit_Zero..C_Char_Digit_Nine,
                         C_Char_Capital_a..C_Char_Capital_f,
                         C_Char_Small_a..C_Char_Small_f];
  C_Set_OctNum_Follow = [C_Char_Digit_Zero..C_Char_Digit_Seven];
  C_Set_BinNum_Follow = [C_Char_Digit_Zero..C_Char_Digit_One];

type
  TSMESSymSet = set of TSMESymbol;

function IdToSym( us: PUCFS32String ): TSMESymbol;

implementation

var
  i,maxreslen: MachineInt;
  ResTrie: THashTrie;

function IdToSym(us: PUCFS32String): TSMESymbol;
var ptrpack: Pointer;
    ptri: MachineInt;
    s: String;
begin
  Result := SMES_ID;
  s := ucfs_to_utf8string(us);
  if Length(s) <= maxreslen then
    begin
      ptrpack := ResTrie.Lookup(upcase(s));
      if ptrpack <> nil then
        begin
          ptri := PtrInt(ptrpack-nil);
          if (ptri > Ord(S__START_KEY)) and
             (ptri < Ord(S__END_KEY)) then
            Result := TSMESymbol(ptri)
          else
            put_internalerror(2011112900);
        end;
    end;
end;

initialization
  ResTrie.Init(16);
  maxreslen := 1;
  for i := Low(C_Reserved_Symbol) to High(C_Reserved_Symbol) do
    begin
      if Length(C_Reserved_Symbol[i]) > maxreslen then
        maxreslen := Length(C_Reserved_Symbol[i]);
      ResTrie.Add(Upcase(C_Reserved_Symbol[i]),Pointer(PtrInt(i)));
    end;

finalization
  ResTrie.Clear;

end.

