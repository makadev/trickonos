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

interface

uses
  SysUtils, commontl, eomsg, coreobj;

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
    SMES_NOT,       // not
    SMES_IS,        // is
    SMES_DIV,       // div
    SMES_SHLK,      // shl
    SMES_SHRK,      // shr
    SMES_ROLK,      // rol
    SMES_RORK,      // ror

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
  TResSymbol: array[Ord(S__START_KEY)+1..Ord(S__END_KEY)-1] of String =
    (
      'NIL',
      'FALSE',
      'TRUE',
      'AND',
      'OR',
      'NOT',
      'IS',
      'DIV',
      'SHL',
      'SHR',
      'ROL',
      'ROR',
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

type
  TSMESSymSet = set of TSMESymbol;

function IdToSym( const s: String ): TSMESymbol;

implementation

var
  i,maxreslen: MachineInt;
  ResTrie: THashTrie;

function IdToSym(const s: String): TSMESymbol;
var ptrpack: Pointer;
    ptri: MachineInt;
begin
  Result := SMES_ID;
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
  for i := Low(TResSymbol) to High(TResSymbol) do
    begin
      if Length(TResSymbol[i]) > maxreslen then
        maxreslen := Length(TResSymbol[i]);
      ResTrie.Add(Upcase(TResSymbol[i]),Pointer(PtrInt(i)+nil));
    end;

finalization
  ResTrie.Clear;

end.

