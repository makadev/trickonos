{   Include for sonull's Dictionary Type

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

(*******************************************************************************
  DICT Methods/Attr/Helpers
 ******************************************************************************)

var
  TMethodTrie_Dict: THashTrie;

procedure so_dict_addmethod( const mname: String; methh: TSOMethodHandler );
begin
  if TMethodTrie_Dict.Add(UpCase(mname),methh) <> nil then
    put_internalerror(12012205);
end;

type
  {dict instance}
  PSO_Dict = ^TSO_Dict;
  TSO_Dict = object(TSOInstance)
    dict: THashTrie;
  end;

function socls_Dict_TypeQuery( soself: PSOInstance ): String;
begin
  {$IFDEF SELFCHECK}SelfCheck(soself,so_dict_class);{$ENDIF}
  Result := C_SOTYPE_DICTIONARY_NAME;
end;

function DictComparator( s: PUCFS32String; data, xdata: Pointer ) : Boolean;
var rval: PSOInstance;
begin
  rval := PSOInstance(PSO_Dict(xdata)^.dict.Lookup(s));
  if Assigned(rval) then
    Result := PSOInstance(data)^.GetTypeCls^.Compare(PSOInstance(data),PSOInstance(rval)) = socmp_isEqual
  else
    Result := false;
end;

function socls_Dict_Compare(soself, rightop: PSOInstance): TSOCompareResult;
begin
  {$IFDEF SELFCHECK}SelfCheck(soself,so_dict_class);{$ENDIF}
  if rightop^.IsType(so_dict_class) then
    begin
      if soself = rightop then
        Result := socmp_isEqual
      else if PSO_Dict(soself)^.dict.GetCount <> PSO_Dict(rightop)^.dict.GetCount then
        Result := socmp_NotComparable
      else
        begin
          if PSO_Dict(soself)^.dict.ForEach(@DictComparator,rightop) then
            Result := socmp_isEqual
          else
            Result := socmp_NotComparable;
        end;
    end
  else
    Result := socmp_NotComparable;
end;

procedure socls_Dict_GCEnumerator(instance: PSOInstance; tracer: TGarbageCollectorTracer);
begin
  PSO_Dict(instance)^.dict.ForEach(@hashtrie_gcenum,tracer);
end;

procedure socls_Dict_PostConstructor(instance: PSOInstance);
begin
  PSO_Dict(instance)^.dict.Init(0,4);
end;

procedure socls_Dict_PreDestructor(instance: PSOInstance);
begin
  PSO_Dict(instance)^.dict.Done;
end;

function socls_Dict_MethodCall( callinfo: PMethodCallInfo ): PSOInstance;
{generic, lookup method and call}
var m: TSOMethodHandler;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_dict_class);{$ENDIF}
      m := TSOMethodHandler( TMethodTrie_Dict.LookupByHash(hashk,name) );
      if Assigned(m) then
        Result := m( callinfo )
      else
        Result := nil;
    end;
end;

{Dict::SetMember(string,value) / Dict::SetIndex(string,value) -> value}
function _Dict_SetMember_(callinfo: PMethodCallInfo): PSOInstance;
{lookupadd, incref, return}
var oldval: PSOInstance;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_dict_class);{$ENDIF}
      if argnum = 2 then
        begin
          if soargs^[0]^.IsType(so_string_class) then
            begin
              if not soself^.GetLocked then
                begin
                  check_so_maxcollection(PSO_Dict(soself)^.dict.GetCount+1);
                  soargs^[1]^.IncRef; // << entry in table
                  soargs^[1]^.IncRef; // << incref again for result
                  oldval := PSOInstance(PSO_Dict(soself)^.dict.Add(so_string_get_ucfs(soargs^[0],false),soargs^[1]));
                  if Assigned(oldval) then
                    oldval^.DecRef;
                  Result := soargs^[1];
                end
              else
                Result := init_lockmod(soself,name);
            end
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{Dict::GetMember(string) / Dict::GetIndex(string) -> value}
function _Dict_GetMember_(callinfo: PMethodCallInfo): PSOInstance;
{lookup, incref, return}
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_dict_class);{$ENDIF}
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_string_class) then
            begin
              Result := PSO_Dict(soself)^.dict.Lookup(so_string_get_ucfs(soargs^[0],false));
              if Assigned(Result) then
                Result^.IncRef
              else
                Result := so_nil;
            end
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

(*******************************************************************************
  Dict Interator
 ******************************************************************************)

type
  TSODict_IODictIterator = class(TInternalObject)
    private
       FHTIter: THashTrieIterator;
    public
      function Next( callinfo: PMethodCallInfo ): PSOInstance;
      function CurrentKey( attrinfo: PAttributeInfo ): PSOInstance;
      function CurrentValue( attrinfo: PAttributeInfo ): PSOInstance;
      constructor Create; override;
      destructor Destroy; override;
  end;

{ TSOList_IOListIterator }

function TSODict_IODictIterator.Next( callinfo: PMethodCallInfo ): PSOInstance;
{next, initialize to first element and lock dict, otherwise clear iterator and unlock}
begin
  ASSERT(Length(FReferings) = 1);
  if not hashtrie_iterator_done(FHTIter) then
    begin
      {check for next, if no next -> unlock}
      if hashtrie_iterator_next(@(PSO_Dict(FReferings[0])^.dict),FHTIter) then
        Result := so_true
      else
        begin
          Result := so_false;
          PSO_Dict(FReferings[0])^.DecLocks;
        end;
    end
  else
    begin
      {check for first, if first exists -> lock}
      if hashtrie_iterator_next(@(PSO_Dict(FReferings[0])^.dict),FHTIter) then
        begin
          Result := so_true;
          PSO_Dict(FReferings[0])^.IncLocks;
        end
      else
        Result := so_false;
    end;
end;

function TSODict_IODictIterator.CurrentKey(attrinfo: PAttributeInfo): PSOInstance;
{currentkey, check locks/iterator, return key as sostring
 no setting allowed!}
begin
  if not hashtrie_iterator_done(FHTIter) then
    begin
      if not Assigned(attrinfo^.setter) then
        begin
          ASSERT(PSO_Dict(FReferings[0])^.GetLocked);
          Result := so_string_init_ucfs(FHTIter.currcoll^.colstring,true);
        end
      else
        Result := nil;
    end
  else
    Result := nil;
end;

function TSODict_IODictIterator.CurrentValue(attrinfo: PAttributeInfo): PSOInstance;
{currentValue, check locks/iterator, return value and incref
  may also allow setter}
begin
  Result := nil;
  if not hashtrie_iterator_done(FHTIter) then
    begin
      ASSERT(PSO_Dict(FReferings[0])^.GetLocked);
      if not Assigned(attrinfo^.setter) then
        begin
          {get value, incref}
          Result := PSOInstance(FHTIter.currcoll^.data);
          Result^.IncRef;
        end
      else
        begin
          {set value, decref replaced val, double incref setter
           for replace and return}
          Result := PSOInstance(FHTIter.currcoll^.data);
          Result^.DecRef;
          FHTIter.currcoll^.data := attrinfo^.setter;
          Result := attrinfo^.setter;
          Result^.IncRef;
          Result^.IncRef;
        end;
    end;
end;

constructor TSODict_IODictIterator.Create;
begin
  inherited Create;
  Self.RegisterMethod(C_STDCALLM_ITER__NEXT,@Next);
  Self.RegisterAttribute(C_STDMEMBR_DICTITER__CURRENTKEY,@CurrentKey);
  Self.RegisterAttribute(C_STDMEMBR_DICTITER__CURRENTVALUE,@CurrentValue);
  hashtrie_iterator_reset(FHTIter);
end;

destructor TSODict_IODictIterator.Destroy;
{iterator is collected, check if its still in iteration and those holds a lock
 on the dict -> remove it}
begin
  if not hashtrie_iterator_done(FHTIter) then
    PSO_Dict(FReferings[0])^.DecLocks;
  inherited Destroy;
end;

(*******************************************************************************
  DICT Methods/Attr/Helpers
 ******************************************************************************)

function so_dict_init: PSOInstance;
begin
  Result := InitInstance(so_dict_class);
end;

function so_dict_get_member(d: PSOInstance; const s: String; noincref: Boolean): PSOInstance;
begin
  {$IFDEF SELFCHECK}SelfCheck(d,so_dict_class);{$ENDIF}
  Result := PSO_Dict(d)^.dict.Lookup(s);
  if Assigned(Result) then
    begin
      if not noincref then
        Result^.IncRef;
    end
  else
    Result := so_nil;
end;

procedure so_dict_set_member(d, value: PSOInstance; const s: String; noincref: Boolean);
var tmp: PSOInstance;
begin
  {$IFDEF SELFCHECK}SelfCheck(d,so_dict_class);{$ENDIF}
  tmp := PSO_Dict(d)^.dict.Add(s,value);
  if Assigned(tmp) then
    tmp^.DecRef;
  if not noincref then
    tmp^.IncRef;
end;

{DICT::Count()}
function _Dict_Count_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_dict_class);{$ENDIF}
      if argnum = 0 then
        begin
          Result := so_integer_init(PSO_Dict(soself)^.dict.GetCount);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{DICT::Delete(<string>)}
function _Dict_Delete_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_dict_class);{$ENDIF}
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_string_class) then
            begin
              if not soself^.GetLocked then
                begin
                  Result := PSO_Dict(soself)^.dict.Delete(so_string_get_ucfs(soargs^[0],false));
                  if not Assigned(Result) then
                    Result := so_nil;
                end
              else
                Result := init_lockmod(soself,name);
            end
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{DICT::Iterator() -> iterator object}
function _Dict_Iterator_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_dict_class);{$ENDIF}
      if argnum = 0 then
        begin
          Result := so_create_internalobject_bound(C_SOTYPE_IO_DICTITER_NAME,TSODict_IODictIterator,soself);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;
