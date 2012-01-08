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

type
  {string instance}
  PSO_String = ^TSO_String;
  TSO_String = object(TSOInstance)
    str: String;
  end;

  {Class for <String> Type}
  TSOTypeString = class(TSOTypeBase)
    protected
      class function InstanceSize: MachineInt; override;
      class procedure PostConstructor(instance: PSOInstance); override;
      class procedure PreDestructor(instance: PSOInstance); override;
    public
      class function BaseTypeName: String; override;
      class function BinOpAdd(soself, rightop: PSOInstance): PSOInstance; override;
      class function Compare(soself, rightop: PSOInstance): TSOCompareResult; override;
      class function GetIndex(soself, idx: PSOInstance): PSOInstance; override;
      class function SetIndex(soself, idx, value: PSOInstance): PSOInstance; override;
  end;


{ TSOTypeString }

class function TSOTypeString.BaseTypeName: String;
begin
  Result := C_SOTYPE_STRING_NAME;
end;

class function TSOTypeString.InstanceSize: MachineInt;
begin
  Result := SizeOf(TSO_String);
end;

class function TSOTypeString.BinOpAdd(soself, rightop: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_string_class);
  if rightop^.IsType(so_string_class) then
    begin
      Result := so_string_init( PSO_String(soself)^.str + PSO_String(rightop)^.str );
      check_so_shortbuf(Length(PSO_String(Result)^.str));
    end
  else
    Result := init_invargtype_error(soself,rightop,1,DEFAULT_METHOD_BinOpAdd);
end;

class function TSOTypeString.Compare(soself, rightop: PSOInstance): TSOCompareResult;
var res: Integer;
begin
  SelfCheck(soself,so_string_class);
  if rightop^.IsType(so_string_class) then
    begin
      res := CompareStr(PSO_String(soself)^.str,PSO_String(soself)^.str);
      if res = 0 then
        Result := socmp_isEqual
      else if res > 0 then
        Result := socmp_isGreater
      else
        Result := socmp_isLess;
    end
  else
    Result := inherited Compare(soself, rightop);
end;

class function TSOTypeString.GetIndex(soself, idx: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_string_class);
  if idx^.IsType(so_integer_class) then
    begin
      if (so_integer_get(idx) > 0) or
         (so_integer_get(idx) <= Length(PSO_String(soself)^.str)) then
        begin
          Result := so_string_init(PSO_String(soself)^.str[so_integer_get(idx)]);
        end
      else
        Result := init_range_error(soself,idx,1,DEFAULT_METHOD_GetIndex);
    end
  else
    Result := init_invargtype_error(soself,idx,1,DEFAULT_METHOD_GetIndex);
end;

class function TSOTypeString.SetIndex(soself, idx, value: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_string_class);
  if idx^.IsType(so_integer_class) then
    begin
      if value^.IsType(so_string_class) then
        begin
          if (so_integer_get(idx) > 0) or
             (so_integer_get(idx) <= Length(PSO_String(soself)^.str)) then
            begin
              if Length(PSO_String(value)^.str) = 1 then
                begin
                  if not soself^.GetLocked then
                    begin
                      PSO_String(soself)^.str[so_integer_get(idx)] := PSO_String(value)^.str[1];
                      Result := value;
                      Result^.IncRef;
                    end
                  else
                    Result := init_lockmod(soself,DEFAULT_METHOD_SetIndex);
                end
              else
                Result := init_invargvalue_error(soself,value,2,DEFAULT_METHOD_SetIndex);
            end
          else
            Result := init_range_error(soself,idx,1,DEFAULT_METHOD_SetIndex);
        end
      else
        Result := init_invargtype_error(soself,value,2,DEFAULT_METHOD_SetIndex);
    end
  else
    Result := init_invargtype_error(soself,idx,1,DEFAULT_METHOD_SetIndex);
end;

class procedure TSOTypeString.PostConstructor(instance: PSOInstance);
begin
  SetLength(PSO_String(instance)^.str,0);
end;

class procedure TSOTypeString.PreDestructor(instance: PSOInstance);
begin
  SetLength(PSO_String(instance)^.str,0);
end;

(*******************************************************************************
  STRING Methods/Attr/Helpers
 ******************************************************************************)

function so_string_init( const s: string ): PSOInstance;
begin
  Result := InitInstance(TSOTypeString);
  PSO_String(Result)^.str := s;
end;

function so_string_get(sostring: PSOInstance): String;
begin
  SelfCheck(sostring,TSOTypeString);
  Result := PSO_String(sostring)^.str;
end;

{String::Length() -> integer}
function _String_Length_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if argnum = 0 then
    begin
      Result := so_integer_init(Length(PSO_String(soself)^.str));
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;

{String::Trim() -> (self)}
function _String_Trim_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if argnum = 0 then
    begin
      if not soself^.GetLocked then
        begin
          PSO_String(soself)^.str := Trim(PSO_String(soself)^.str);
          Result := soself;
          Result^.IncRef;
        end
      else
        Result := init_lockmod(soself,mname);
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;

{String::TrimLeft() -> (self)}
function _String_TrimLeft_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if argnum = 0 then
    begin
      if not soself^.GetLocked then
        begin
          PSO_String(soself)^.str := TrimLeft(PSO_String(soself)^.str);
          Result := soself;
          Result^.IncRef;
        end
      else
        Result := init_lockmod(soself,mname);
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;

{String::TrimRight() -> (self)}
function _String_TrimRight_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if argnum = 0 then
    begin
      if not soself^.GetLocked then
        begin
          PSO_String(soself)^.str := TrimRight(PSO_String(soself)^.str);
          Result := soself;
          Result^.IncRef;
        end
      else
        Result := init_lockmod(soself,mname);
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;

{String::Split(<delimiter>) -> List of Strings}
function _String_Split_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
var i,j,l,h,sh: MachineInt;
    tmps, tmpsplits: String;
    pins: PSOInstance;
begin
  if argnum = 1 then
    begin
      if soargs^[0]^.IsType(so_string_class) then
        begin
          tmps := PSO_String(soself)^.str;
          tmpsplits := PSO_String(soargs^[0])^.str;
          Result := so_list_init;
          if Length(tmpsplits) < Length(tmps) then
            begin
              if (Length(tmpsplits) > 2) and
                 (Length(tmpsplits) <= High(Word)) and
                 (Length(tmps) > 200) then
                begin
                  {simple rolling hash scan with theoretic O(n)}
                  h := 0;
                  for i := 1 to Length(tmpsplits) do
                    h := h + Ord(tmpsplits[i]);
                  sh := 0;
                  for i := 1 to Length(tmpsplits)-1 do
                    sh := sh + Ord(tmps[i]);
                  i := Length(tmpsplits);
                  j := 1;
                  l := 0;
                  repeat
                    {update rolling hash}
                    sh := sh+Ord(tmps[i]);
                    {compare hashes}
                    if sh = h then
                      begin
                        {match, compare strings}
                        if CompareMem(@tmps[j],@tmpsplits[1],Length(tmpsplits)) then
                          begin
                            {real match, copy (l+1)..(j-1), add to list}
                            pins := so_string_init(Copy(tmps,l+1,j-(l+1)));
                            Result := so_list_append(Result,@pins,1);
                            Result^.DecRef;
                            {update last match/new end}
                            l := i;
                            i := i+Length(tmpsplits);
                            {check space left}
                            if i > Length(tmps) then
                              break;
                            {prehash 1..Length-1}
                            sh := 0;
                            for j := l+1 to l+Length(tmpsplits)-1 do
                              sh := sh + Ord(tmps[j]);
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
                        sh := sh-Ord(tmps[j]);
                        {update next start}
                        Inc(j,1);
                      end;
                  until i > Length(tmps);
                end
              else
                begin
                  {simple scan with theoretic  O(n*m)}
                  l := 0;
                  i := 1;
                  while i <= (Length(tmps)-(Length(tmpsplits)-1)) do
                    begin
                      if tmps[i] <> tmpsplits[1] then
                        Inc(i,1)
                      else
                        begin
                          if CompareMem(@tmps[i],@tmpsplits[1],Length(tmpsplits)) then
                            begin
                              pins := so_string_init(Copy(tmps,l+1,i-(l+1)));
                              Result := so_list_append(Result,@pins,1);
                              Result^.DecRef;
                              i := i+Length(tmpsplits);
                              l := i-1;
                            end
                          else
                            Inc(i,1);
                        end;
                    end;
                end;
            {cp leftof (l+1)..Length}
            if (Length(tmps)-l) > 0 then
              begin
                pins := so_string_init(Copy(tmps,l+1,Length(tmps)-l));
                Result := so_list_append(Result,@pins,1);
                Result^.DecRef;
              end
            else
              begin
                {nothing left -> match at end, put ''}
                pins := so_string_init('');
                Result := so_list_append(Result,@pins,1);
                Result^.DecRef;
              end;
            end
          else if (Length(tmpsplits) = Length(tmps)) and
                  (tmpsplits = tmps) then
            begin
              {splitstr = string -> ['','']}
              pins := so_string_init('');
              Result := so_list_append(Result,@pins,1);
              // fix refcount since append increfs again
              Result^.DecRef;
              pins := so_string_init('');
              Result := so_list_append(Result,@pins,1);
              // fix refcount since append increfs again
              Result^.DecRef;
            end
          else
            begin
              {splitstr < str -> return string since they cant match}
              pins := so_string_init(tmps);
              Result := so_list_append(Result,@pins,1);
              // fix refcount since append increfs again
              Result^.DecRef;
            end;
        end
      else
        Result := init_invargtype_error(soself,soargs^[0],1,mname);
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;

{String::Join(<list of strings> or <strings>) (inverse to Split) -> (self)}
function _String_Join_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
var i: Integer;
    p: Pointer;
    tmps: String;
begin
  Result := nil;
  if argnum = 1 then
    begin
      if soargs^[0]^.IsType(so_list_class) then
        begin
          if so_list_count(soargs^[0]) > 0 then
            begin
              {check - list must contain only strings}
              p := nil;
              if so_list_iter_next(soargs^[0],p) then
                begin
                  repeat
                     if not so_list_iter_getval(p)^.IsType(so_string_class) then
                       begin
                         Result := init_invargvalue_error(soself,soargs^[0],1,mname);
                         Exit(Result);
                       end;
                  until not so_list_iter_next(soargs^[0],p);
                end;
              {everything is string, join}
              tmps := '';
              p := nil;
              if so_list_iter_next(soargs^[0],p) then
                begin
                  tmps := PSO_String(so_list_iter_getval(p))^.str;
                  while so_list_iter_next(soargs^[0],p) do
                    begin
                      tmps := tmps + PSO_String(soself)^.str + PSO_String(so_list_iter_getval(p))^.str;
                      check_so_shortbuf(Length(tmps));
                    end;
                end;
              PSO_String(soself)^.str := tmps;
            end
          else
            {joined with nothing}
            PSO_String(soself)^.str := '';
        end
      else if soargs^[0]^.IsType(so_string_class) then
        begin
          {only one string, nothing to join}
          PSO_String(soself)^.str := PSO_String(soargs^[0])^.str;
        end;
    end
  else if argnum > 1 then
    begin
      {check - all args strings}
      for i := 0 to argnum-1 do
        begin
          if not soargs^[i]^.IsType(so_string_class) then
            begin
              Result := init_invargvalue_error(soself,soargs^[i],i,mname);
              Exit(Result);
            end;
        end;
      {now join}
      tmps := '';
      for i := 0 to argnum-1 do
        begin
          tmps := tmps + PSO_String(soargs^[i])^.str;
          if i < (argnum-1) then
            begin
              tmps := tmps + PSO_String(soself)^.str;
              check_so_shortbuf(Length(tmps));
            end;
        end;
      PSO_String(soself)^.str := tmps;
    end
  else
    begin
      {argnum = 1, joined with nothing}
      PSO_String(soself)^.str := '';
    end;
  if not Assigned(Result) then
    begin
      Result := soself;
      Result^.IncRef;
    end;
end;
