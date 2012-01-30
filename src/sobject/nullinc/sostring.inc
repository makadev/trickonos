{   Include for sonull's String Type

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

(******************************************************************************
 STRING
 ******************************************************************************)

var
  TMethodTrie_String: THashTrie;

procedure so_string_addmethod( const mname: String; methh: TSOMethodHandler );
begin
  if TMethodTrie_String.Add(UpCase(mname),methh) <> nil then
    put_internalerror(12012202);
end;

type
  {string instance}
  PSO_String = ^TSO_String;
  TSO_String = object(TSOInstance)
    pstr: PUCFS32String;
  end;

function socls_String_TypeQuery( soself: PSOInstance ): String;
begin
  {$IFDEF SELFCHECK}SelfCheck(soself,so_string_class);{$ENDIF}
  Result := C_SOTYPE_STRING_NAME;
end;

function socls_String_Compare(soself, rightop: PSOInstance): TSOCompareResult;
var res: VMInt;
begin
  {$IFDEF SELFCHECK}SelfCheck(soself,so_string_class);{$ENDIF}
  if rightop^.IsType(so_string_class) then
    begin
      res := ucfs_compare(PSO_String(soself)^.pstr,PSO_String(rightop)^.pstr);
      if res = 0 then
        Result := socmp_isEqual
      else if res > 0 then
        Result := socmp_isGreater
      else
        Result := socmp_isLess;
    end
  else
    Result := DefaultCompare(soself, rightop);
end;

function socls_String_MethodCall( callinfo: PMethodCallInfo ): PSOInstance;
{generic, lookup method and call}
var m: TSOMethodHandler;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_string_class);{$ENDIF}
      m := TSOMethodHandler( TMethodTrie_String.LookupByHash(hashk,name) );
      if Assigned(m) then
        Result := m( callinfo )
      else
        Result := nil;
    end;
end;

procedure socls_String_PreDestructor(instance: PSOInstance);
begin
{$IFDEF DEBUG}
  if Assigned(PSO_String(instance)^.pstr) and
     (PSO_String(instance)^.pstr^.info.getrefs > 1) then
    ucfs_release( PSO_String(instance)^.pstr )
  else
    begin
      ucfs_release( PSO_String(instance)^.pstr );
      PSO_String(instance)^.pstr := nil;
    end;
{$ELSE}
  ucfs_release( PSO_String(instance)^.pstr );
{$ENDIF}
end;

procedure socls_String_PostConstructor(instance: PSOInstance);
begin
  PSO_String(instance)^.pstr := nil;
end;

(*******************************************************************************
  STRING Methods/Attr/Helpers
 ******************************************************************************)

{String::Add(string) -> string}
function _String_Add_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_string_class);{$ENDIF}
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_string_class) then
            begin
              check_so_shortbuf(so_string_length(soargs^[0]) + so_string_length(soself));
              Result := so_string_init_ucfs(ucfs_concat(PSO_String(soself)^.pstr,
                                                        PSO_String(soargs^[0])^.pstr),false);
            end
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{String::GetIndex(integer) -> string}
function _String_GetIndex_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_string_class);{$ENDIF}
      if argnum = 1 then
        begin
          {check for integer arg}
          if soargs^[0]^.IsType(so_integer_class) then
            begin
              {check integer arg is in range}
              if so_integer_fits(soargs^[0]) and
                 ((so_integer_get(soargs^[0],true) > 0) or
                  (so_integer_get(soargs^[0],true) <= so_string_length(soself))) then
                begin
                  {and access, return character as new string}
                  Result := so_string_init_ucfs( ucfs_cpy(PSO_String(soself)^.pstr,
                                                 so_integer_get(soargs^[0],true),1), false);
                end
              else
                Result := init_range_error(soself,soargs^[0],1,name);
            end
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

function so_string_init_utf8( const s: String ): PSOInstance;
begin
  Result := InitInstance(so_string_class);
  check_so_shortbuf(Length(s));
  if ucfs_from_utf8string(s,PSO_String(Result)^.pstr) >= 0 then
    begin
      put_debug('Internal String (utf8) Decode Error');
      put_internalerror(12012601); // decode error
    end;
end;

function so_string_init_char(uc: TUCFS32Char): PSOInstance;
begin
  ASSERT(uc>=0);
  Result := InitInstance(so_string_class);
  PSO_String(Result)^.pstr := ucfs_alloc(1,ucfs_u32c_bytelen(uc));
  ucfs_setc(PSO_String(Result)^.pstr,1,uc);
end;

function so_string_init_ucfs( ps: PUCFS32String; incref: Boolean ): PSOInstance;
begin
  Result := InitInstance(so_string_class);
  PSO_String(Result)^.pstr := ps;
  if incref then
    ucfs_incref(ps);
end;

function so_string_init_empty: PSOInstance;
begin
  Result := InitInstance(so_string_class);
  {nothing more, pstr is initialized with nil and empty=nil}
end;

function so_string_length( sostring: PSOInstance ): VMInt;
begin
  {$IFDEF SELFCHECK}SelfCheck(sostring,so_string_class);{$ENDIF}
  Result := ucfs_length(PSO_String(sostring)^.pstr);
end;

function so_string_get_ucfs(sostring: PSOInstance; increfs: Boolean
  ): PUCFS32String;
begin
  {$IFDEF SELFCHECK}SelfCheck(sostring,so_string_class);{$ENDIF}
  Result := PSO_String(sostring)^.pstr;
  if increfs then
    ucfs_incref(Result);
end;

function so_string_get_utf8( sostring: PSOInstance ): String;
begin
  {$IFDEF SELFCHECK}SelfCheck(sostring,so_string_class);{$ENDIF}
  Result := ucfs_to_utf8string(PSO_String(sostring)^.pstr);
end;

{String::Length() -> integer}
function _String_Length_( callinfo: PMethodCallInfo ): PSOInstance;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_string_class);{$ENDIF}
      if argnum = 0 then
        Result := so_integer_init(ucfs_length(PSO_String(soself)^.pstr))
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

