{   Unit for type extension (methods/attr handler) for string

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

unit exstring;

{$mode objfpc}{$H+}

interface

uses SysUtils, commontl, socore, solnull, ucfs;

{DOC>> String::Trim() -> (string)}
function _String_Trim_(callinfo:PMethodCallInfo): PSOInstance;
{DOC>> String::TrimLeft() -> (string)}
function _String_TrimLeft_(callinfo:PMethodCallInfo): PSOInstance;
{DOC>> String::TrimRight() -> (string)}
function _String_TrimRight_(callinfo:PMethodCallInfo): PSOInstance;
{DOC>> String::Split(<delimiter>) -> List of Strings}
function _String_Split_(callinfo:PMethodCallInfo): PSOInstance;
{DOC>> String::Join(<list of strings> or <strings>) (inverse to Split) -> (string)}
function _String_Join_(callinfo:PMethodCallInfo): PSOInstance;
{DOC>> String::Upcase() -> (string)}
function _String_Upcase_(callinfo:PMethodCallInfo): PSOInstance;
{DOC>> String::Lowercase() -> (string)}
function _String_Lowercase_(callinfo:PMethodCallInfo): PSOInstance;

procedure InitAndRegister;

implementation

function _String_Trim_(callinfo:PMethodCallInfo): PSOInstance;
{String::Trim() -> (string)
  -- trim spaces}
var us: PUCFS32String;
    s,l: VMInt;
begin
  with callinfo^ do
    begin
      if argnum = 0 then
        begin
          us := so_string_get_ucfs(soself,false);
          s := 1;
          l := ucfs_length(us);
          {strip right}
          while (l > 0) and
                (ucfs_getc(us,l) = 32) do
            Dec(l,1);
          {strip left}
          while (l > 0) and
                (ucfs_getc(us,s) = 32) do
            begin
              Inc(s,1);
              Dec(l,1);
            end;
          if (s = 1) and
             (l = ucfs_length(us)) then
            begin
              Result := soself;
              Result^.IncRef;
            end
          else
            Result := so_string_init_ucfs(ucfs_cpy(us,s,l),false);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

function _String_TrimLeft_(callinfo:PMethodCallInfo): PSOInstance;
{String::TrimLeft() -> (string)
  -- trim spaces (left side)}
var us: PUCFS32String;
    s,l: VMInt;
begin
  with callinfo^ do
    begin
      if argnum = 0 then
        begin
          us := so_string_get_ucfs(soself,false);
          s := 1;
          l := ucfs_length(us);
          {strip left}
          while (l > 0) and
                (ucfs_getc(us,s) = 32) do
            begin
              Inc(s,1);
              Dec(l,1);
            end;
          if (s = 1) and
             (l = ucfs_length(us)) then
            begin
              Result := soself;
              Result^.IncRef;
            end
          else
            Result := so_string_init_ucfs(ucfs_cpy(us,s,l),false);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

function _String_TrimRight_(callinfo:PMethodCallInfo): PSOInstance;
{String::TrimRight() -> (string)
 -- trim spaces (right side)}
var us: PUCFS32String;
    l: VMInt;
begin
  with callinfo^ do
    begin
      if argnum = 0 then
        begin
          us := so_string_get_ucfs(soself,false);
          l := ucfs_length(us);
          {strip right}
          while (l > 0) and
                (ucfs_getc(us,l) = 32) do
            Dec(l,1);
          if l = ucfs_length(us) then
            begin
              Result := soself;
              Result^.IncRef;
            end
          else
            Result := so_string_init_ucfs(ucfs_cpy(us,1,l),false);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

function _String_Split_(callinfo:PMethodCallInfo): PSOInstance;
{String::Split(<delimiter>) -> List of Strings}
var h,sh: MachineInt;
    i,j,l: VMInt;
    tmps, tmpsplits: PUCFS32String;
    pins: PSOInstance;
begin
  with callinfo^ do
    begin
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_string_class) then
            begin
              tmps := so_string_get_ucfs(soself,false);
              tmpsplits := so_string_get_ucfs(soargs^[0],false);
              Result := so_list_init;
              if (ucfs_length(tmpsplits) < ucfs_length(tmps)) and
                 (ucfs_length(tmpsplits) > 0) then
                begin
                  if (ucfs_length(tmpsplits) > 2) and
                     (ucfs_length(tmpsplits) <= High(Word)) and
                     (ucfs_length(tmps) > 100) then
                    begin
                      {simple rolling hash scan with theoretic O(n)}
                      h := 0;
                      for i := 1 to ucfs_length(tmpsplits) do
{$PUSH}
{$R-}{$Q-}
                        h := h + ucfs_getc(tmpsplits, i);
{$POP}
                      sh := 0;
                      for i := 1 to ucfs_length(tmpsplits)-1 do
{$PUSH}
{$R-}{$Q-}
                        sh := sh + ucfs_getc(tmps, i);
{$POP}
                      i := ucfs_length(tmpsplits);
                      j := 1;
                      l := 0;
                      repeat
                        {update rolling hash}
{$PUSH}
{$R-}{$Q-}
                        sh := sh + ucfs_getc(tmps, i);
{$POP}
                        {compare hashes}
                        if sh = h then
                          begin
                            {match, compare strings}
                            if ucfs_submatch(tmps,tmpsplits,j,ucfs_length(tmpsplits)) then
                              begin
                                {real match, copy (l+1)..(j-1), add to list}
                                pins := so_string_init_ucfs(ucfs_cpy(tmps,l+1,j-(l+1)),false);
                                so_list_append_one(Result,pins,false);
                                {update last match/new end}
                                l := i;
                                i := i + ucfs_length(tmpsplits);
                                {check space left}
                                if i > ucfs_length(tmps) then
                                  break;
                                {prehash 1..Length-1}
                                sh := 0;
                                for j := l+1 to l+ucfs_length(tmpsplits)-1 do
{$PUSH}
{$R-}{$Q-}
                                  sh := sh + ucfs_getc(tmps, j);
{$POP}
                                {update new start}
                                j := l+1;
                                {next cycle}
                              end;
                          end
                        else
                          begin
                            {update next end}
                            Inc(i,1);
                            {update rolling hash (sub last)}
{$PUSH}
{$R-}{$Q-}
                            sh := sh - ucfs_getc(tmps, j);
{$POP}
                            {update next start}
                            Inc(j,1);
                          end;
                      until i > ucfs_length(tmps);
                    end
                  else
                    begin
                      {simple scan with theoretic  O(n*m)}
                      l := 0;
                      i := 1;
                      while i <= (ucfs_length(tmps)-(ucfs_length(tmpsplits)-1)) do
                        begin
                          if ucfs_getc(tmps,i) <> ucfs_getc(tmpsplits,1) then
                            Inc(i,1)
                          else
                            begin
                              if ucfs_submatch(tmps,tmpsplits,i,ucfs_length(tmpsplits)) then
                                begin
                                  pins := so_string_init_ucfs(ucfs_cpy(tmps,l+1,i-(l+1)),false);
                                  so_list_append_one(Result,pins,false);
                                  i := i+ucfs_length(tmpsplits);
                                  l := i-1;
                                end
                              else
                                Inc(i,1);
                            end;
                        end;
                    end;
                {cp leftof (l+1)..Length}
                if (ucfs_length(tmps)-l) > 0 then
                  begin
                    pins := so_string_init_ucfs(ucfs_cpy(tmps,l+1,ucfs_length(tmps)-l),false);
                    so_list_append_one(Result,pins,false);
                  end
                else
                  begin
                    {nothing left -> match at end, put ''}
                    pins := so_string_init_empty;
                    so_list_append_one(Result,pins,false);
                  end;
                end
              else if (ucfs_length(tmpsplits) = ucfs_length(tmps)) and
                      (ucfs_compare(tmpsplits,tmps) = 0) and
                      (ucfs_length(tmpsplits) > 0) then
                begin
                  {full match split - splitstr = string -> ['','']}
                  pins := so_string_init_empty;
                  so_list_append_one(Result,pins,true);
                  so_list_append_one(Result,pins,false);
                end
              else
                begin
                  {splitstr < str or splitstr=''
                   -> return string since they cant match}
                  pins := soself;
                  so_list_append_one(Result,pins,true);
                end;
            end
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

function _String_Join_(callinfo:PMethodCallInfo): PSOInstance;
{String::Join(<list of strings> or <strings>) (inverse to Split) -> (string)}
var i,j,blen,clen,rlen: VMInt;
    p: Pointer;
    tmps,ctmp: PUCFS32String;
begin
  with callinfo^ do
    begin
      Result := nil;
      blen := ucfs_charsize(so_string_get_ucfs(soself,false));
      clen := ucfs_length(so_string_get_ucfs(soself,false));
      rlen := 0;
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_list_class) then
            begin
              if so_list_count(soargs^[0]) > 0 then
                begin
                  {check - list must contain only strings
                    also get max bytelen and charlen for merge}
                  p := nil;
                  if so_list_iter_next(soargs^[0],p) then
                    begin
                      repeat
                         if so_list_iter_getval(p)^.IsType(so_string_class) then
                           begin
                             tmps := so_string_get_ucfs(so_list_iter_getval(p),false);
                             if blen < ucfs_charsize(tmps) then
                               blen := ucfs_charsize(tmps);
                             rlen := rlen+ucfs_length(tmps);
                             check_so_shortbuf(rlen);
                             rlen := rlen + clen;
                           end
                         else
                           begin
                             Result := init_invargvalue_error(soself,soargs^[0],1,name);
                             Exit(Result);
                           end;
                      until not so_list_iter_next(soargs^[0],p);
                    end;
                  {sub rlen by 1 string since join only inserts between}
                  rlen := rlen - clen;
                  {everything is string, join}
                  tmps := ucfs_alloc(rlen,blen);
                  p := nil;
                  if so_list_iter_next(soargs^[0],p) then
                    begin
                      ctmp := so_string_get_ucfs(so_list_iter_getval(p),false);
                      ucfs_submove(ctmp,tmps,1,ucfs_length(ctmp));
                      j := ucfs_length(ctmp)+1;
                      while so_list_iter_next(soargs^[0],p) do
                        begin
                          ucfs_submove(so_string_get_ucfs(soself,false),tmps,j,clen);
                          Inc(j,clen);
                          ctmp := so_string_get_ucfs(so_list_iter_getval(p),false);
                          ucfs_submove(ctmp,tmps,j,ucfs_length(ctmp));
                          Inc(j,ucfs_length(ctmp));
                        end;
                    end;
                  Result := so_string_init_ucfs(tmps,false);
                end
              else
                {joined with nothing}
                Result := so_string_init_empty;
            end
          else if soargs^[0]^.IsType(so_string_class) then
            begin
              {only one string, nothing to join
                -> pass back this one}
              Result := soargs^[0];
              Result^.IncRef;
            end;
        end
      else if argnum > 1 then
        begin
          {check - all args strings}
          for i := 0 to argnum-1 do
            begin
              if soargs^[i]^.IsType(so_string_class) then
                begin
                  tmps := so_string_get_ucfs(soargs^[i],false);
                  if blen < ucfs_charsize(tmps) then
                    blen := ucfs_charsize(tmps);
                  rlen := rlen+ucfs_length(tmps);
                  check_so_shortbuf(rlen);
                  rlen := rlen + clen;
                end
              else
                begin
                  Result := init_invargvalue_error(soself,soargs^[i],i,name);
                  Exit(Result);
                end;
            end;
          {sub rlen by 1 string since join only inserts between}
          rlen := rlen - clen;
          {now join}
          tmps := ucfs_alloc(rlen,blen);
          j := 1;
          for i := 0 to argnum-1 do
            begin
              ucfs_submove(so_string_get_ucfs(soargs^[i],false),tmps,j,
                ucfs_length(so_string_get_ucfs(soargs^[i],false)));
              Inc(j,ucfs_length(so_string_get_ucfs(soargs^[i],false)));
              if i < (argnum-1) then
                begin
                  ucfs_submove(so_string_get_ucfs(soself,false),tmps,j,clen);
                  Inc(j,clen);
                end;
            end;
          Result := so_string_init_ucfs(tmps,false);
        end
      else
        begin
          {argnum = 1, joined with nothing}
          Result := so_string_init_empty;
        end;
    end;
end;

function _String_Upcase_(callinfo:PMethodCallInfo): PSOInstance;
{String::Upcase() -> (string)}
var us: PUCFS32String;
    uc: TUCFS32Char;
    i: VMInt;
begin
  with callinfo^ do
    begin
      if argnum = 0 then
        begin
          {$WARNING optimize, check f copy is needed (s.Upcase() = s)}
          us := so_string_get_ucfs(soself,false);
          us := ucfs_cpy(us,1,ucfs_length(us));
          Result := so_string_init_ucfs(us,false);
          for i := 1 to ucfs_length(us) do
            begin
              uc := ucfs_getc(us,i);
              if (uc >= Ord('a')) and
                 (uc <= Ord('z')) then
                begin
                  uc := uc + (Ord('A') - Ord('a'));
                  ucfs_setc(us,i,uc);
                end;
            end;
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

function _String_Lowercase_(callinfo:PMethodCallInfo): PSOInstance;
{String::Lowercase() -> (string)}
var us: PUCFS32String;
    uc: TUCFS32Char;
    i: VMInt;
begin
  with callinfo^do
    begin
      if argnum = 0 then
        begin
          {$WARNING optimize, check if copy is needed (s.Lowercase() = s)}
          us := so_string_get_ucfs(soself,false);
          us := ucfs_cpy(us,1,ucfs_length(us));
          Result := so_string_init_ucfs(us,false);
          for i := 1 to ucfs_length(us) do
            begin
              uc := ucfs_getc(us,i);
              if (uc >= Ord('A')) and
                 (uc <= Ord('Z')) then
                begin
                  uc := uc + (Ord('a') - Ord('A'));
                  ucfs_setc(us,i,uc);
                end;
            end;
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

procedure InitAndRegister;
begin
  so_string_addmethod('TRIM',@_String_Trim_);
  so_string_addmethod('TRIMLEFT',@_String_TrimLeft_);
  so_string_addmethod('TRIMRIGHT',@_String_TrimRight_);
  so_string_addmethod('SPLIT',@_String_Split_);
  so_string_addmethod('JOIN',@_String_Join_);
  so_string_addmethod('UPCASE',@_String_Upcase_);
  so_string_addmethod('LOWERCASE',@_String_Lowercase_);
end;

end.

