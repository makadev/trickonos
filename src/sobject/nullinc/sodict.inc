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

type
  {dict instance}
  PSO_Dict = ^TSO_Dict;
  TSO_Dict = object(TSOInstance)
    dict: THashTrie;
  end;

  {Class for <Dict> Type}
  TSOTypeDict = class(TSOTypeBase)
    protected
      class function InstanceSize: MachineInt; override;
      class procedure GCEnumerator(instance: PSOInstance; tracer: TGarbageCollectorTracer); override;
      class procedure PostConstructor(instance: PSOInstance); override;
      class procedure PreDestructor(instance: PSOInstance); override;
    public
      class function BaseTypeName: String; override;
      class function Compare(soself, rightop: PSOInstance): TSOCompareResult; override;
      class function SetMember(soself, value: PSOInstance; const name: String ): PSOInstance; override;
      class function GetMember(soself: PSOInstance; const name: String): PSOInstance; override;
      class function SetIndex(soself, idx, value: PSOInstance): PSOInstance; override;
      class function GetIndex(soself, idx: PSOInstance): PSOInstance; override;
  end;

{ TSOTypeDict }

class function TSOTypeDict.BaseTypeName: String;
begin
  Result := C_SOTYPE_DICTIONARY_NAME;
end;

function DictComparator( const s: String; data, xdata: Pointer ) : Boolean;
var rval: PSOInstance;
begin
  rval := PSOInstance(PSO_Dict(xdata)^.dict.Lookup(s));
  if Assigned(rval) then
    Result := PSOInstance(data)^.GetTypeCls.Compare(PSOInstance(data),PSOInstance(rval)) = socmp_isEqual
  else
    Result := false;
end;

class function TSOTypeDict.Compare(soself, rightop: PSOInstance
  ): TSOCompareResult;
begin
  SelfCheck(soself,self);
  if rightop^.IsType(self) then
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

class function TSOTypeDict.InstanceSize: MachineInt;
begin
  Result := SizeOf(TSO_Dict);
end;

function dict_collenum( const unused: String; data, xdata: Pointer ): Boolean;
begin
  TGarbageCollectorTracer(xdata)(PSOInstance(data));
  Result := true;
end;

class procedure TSOTypeDict.GCEnumerator(instance: PSOInstance;
  tracer: TGarbageCollectorTracer);
begin
  PSO_Dict(instance)^.dict.ForEach(@dict_collenum,tracer);
end;

class procedure TSOTypeDict.PostConstructor(instance: PSOInstance);
begin
  //FillByte(PSO_Dict(soinstance)^.dict,SizeOf(THashTrie),0); done by Instancer
  PSO_Dict(instance)^.dict.Init(0,4);
end;

class procedure TSOTypeDict.PreDestructor(instance: PSOInstance);
begin
  PSO_Dict(instance)^.dict.Done;
end;

class function TSOTypeDict.SetMember(soself, value: PSOInstance; const name: String
  ): PSOInstance;
{lookupadd, incref, return}
var oldval: PSOInstance;
begin
  SelfCheck(soself,self);
  if not soself^.GetLocked then
    begin
      check_so_maxcollection(PSO_Dict(soself)^.dict.GetCount+1);
      value^.IncRef; // << entry in table
      value^.IncRef; // << incref again for result
      oldval := PSOInstance(PSO_Dict(soself)^.dict.Add(name,value));
      if Assigned(oldval) then
        oldval^.DecRef;
      Result := value;
    end
  else
    Result := init_lockmod(soself,DEFAULT_METHOD_SetMember);
end;

class function TSOTypeDict.GetMember(soself: PSOInstance; const name: String
  ): PSOInstance;
{lookup, incref, return}
begin
  SelfCheck(soself,self);
  Result := PSO_Dict(soself)^.dict.Lookup(name);
  if Assigned(Result) then
    Result^.IncRef
  else
    Result := so_nil;
end;

class function TSOTypeDict.SetIndex(soself, idx, value: PSOInstance): PSOInstance;
{same as setmember, with additional stringcheck}
var oldval: PSOInstance;
begin
  SelfCheck(soself,self);
  if idx^.IsType(so_string_class) then
    begin
      if not soself^.GetLocked then
        begin
          check_so_maxcollection(PSO_Dict(soself)^.dict.GetCount+1);
          value^.IncRef; // << entry in table
          value^.IncRef; // << incref again for result
          oldval := PSOInstance(PSO_Dict(soself)^.dict.Add(so_string_get(idx),value));
          if Assigned(oldval) then
            oldval^.DecRef;
          Result := value;
        end
      else
        Result := init_lockmod(soself,DEFAULT_METHOD_SetIndex);
    end
  else
    Result := init_invargtype_error(soself,idx,1,DEFAULT_METHOD_SetIndex);
end;

class function TSOTypeDict.GetIndex(soself, idx: PSOInstance): PSOInstance;
{same as getmember, with additional stringcheck}
begin
  SelfCheck(soself,self);
  if idx^.IsType(so_string_class) then
    begin
      Result := PSO_Dict(soself)^.dict.Lookup(so_string_get(idx));
      if Assigned(Result) then
        Result^.IncRef
      else
        Result := so_nil;
    end
  else
    Result := init_invargtype_error(soself,idx,1,DEFAULT_METHOD_GetIndex);
end;

(*******************************************************************************
  Dict Interator
 ******************************************************************************)

type
  TSODict_IODictIterator = class(TInternalObject)
    private
       FHTIter: THashTrieIterator;
    public
      function Next( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
      function CurrentKey( const aname: String; soself: PSOInstance; setter: PSOInstance ): PSOInstance;
      function CurrentValue( const aname: String; soself: PSOInstance; setter: PSOInstance ): PSOInstance;
      constructor Create; override;
      procedure PreCollect; override;
  end;

{ TSOList_IOListIterator }

function TSODict_IODictIterator.Next(const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
{next, initialize to first element and lock dict, otherwise clear iterator and unlock}
begin
  ASSERT(mname=Upcase(C_STDCALLM_ITER__NEXT));
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

function TSODict_IODictIterator.CurrentKey(const aname: String; soself: PSOInstance; setter: PSOInstance): PSOInstance;
{currentkey, check locks/iterator, return key as sostring
 no setting allowed!}
begin
  ASSERT(aname=Upcase(C_STDMEMBR_DICTITER__CURRENTKEY));
  if not hashtrie_iterator_done(FHTIter) then
    begin
      if not Assigned(setter) then
        begin
          ASSERT(PSO_Dict(FReferings[0])^.GetLocked);
          Result := so_string_init(FHTIter.currcoll^.colstring);
        end
      else
        Result := nil;
    end
  else
    Result := nil;
end;

function TSODict_IODictIterator.CurrentValue(const aname: String; soself: PSOInstance; setter: PSOInstance): PSOInstance;
{currentValue, check locks/iterator, return value and incref
  may also allow setter}
begin
  ASSERT(aname=Upcase(C_STDMEMBR_DICTITER__CURRENTVALUE));
  Result := nil;
  if not hashtrie_iterator_done(FHTIter) then
    begin
      ASSERT(PSO_Dict(FReferings[0])^.GetLocked);
      if not Assigned(setter) then
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
          FHTIter.currcoll^.data := setter;
          Result := setter;
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

procedure TSODict_IODictIterator.PreCollect;
{iterator is collected, check if its still in iteration and those holds a lock
 on the dict -> remove it}
begin
  if not hashtrie_iterator_done(FHTIter) then
    PSO_Dict(FReferings[0])^.DecLocks;
  inherited PreCollect;
end;

(*******************************************************************************
  DICT Methods/Attr/Helpers
 ******************************************************************************)

function so_dict_init: PSOInstance;
begin
  Result := InitInstance(TSOTypeDict);
end;

function so_dict_get_member(d: PSOInstance; const s: String; noincref: Boolean
  ): PSOInstance;
begin
  Result := TSOTypeDict.GetMember(d,s);
  if noincref then
    Result^.DecRef;
end;

procedure so_dict_set_member(d, value: PSOInstance; const s: String;
  noincref: Boolean);
var tmp: PSOInstance;
begin
  tmp := TSOTypeDict.SetMember(d,value,s);
  if noincref then
    tmp^.DecRef;
end;

{DICT::Count()}
function _Dict_Count_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if argnum = 0 then
    begin
      Result := so_integer_init(PSO_Dict(soself)^.dict.GetCount);
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;

{DICT::Delete(<string>)}
function _Dict_Delete_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if argnum = 1 then
    begin
      if soargs^[0]^.IsType(so_string_class) then
        begin
          if not soself^.GetLocked then
            begin
              Result := PSO_Dict(soself)^.dict.Delete(so_string_get(soargs^[0]));
              if not Assigned(Result) then
                Result := so_nil;
            end
          else
            Result := init_lockmod(soself,mname);
        end
      else
        Result := init_invargtype_error(soself,soargs^[0],1,mname);
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;

{DICT::Iterator() -> iterator object}
function _Dict_Iterator_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if argnum = 0 then
    begin
      Result := so_create_internalobject_bound(C_SOTYPE_IO_DICTITER_NAME,TSODict_IODictIterator,soself);
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;
