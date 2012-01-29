{   Include for sonull's List Type

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
  LIST
 ******************************************************************************)

var
  TMethodTrie_List: THashTrie;

procedure so_list_addmethod( const mname: String; methh: TSOMethodHandler );
begin
  if TMethodTrie_List.Add(UpCase(mname),methh) <> nil then
    put_internalerror(12012204);
end;

type
  {doubly linked list instance}
  PSOListElement = ^TSOListElement;
  TSOListElement = record
    next: PSOListElement;
    prev: PSOListElement;
    elem: PSOInstance;
  end;

  PSO_List = ^TSO_List;
  TSO_List = object(TSOInstance)
    first: PSOListElement;
    last: PSOListElement;
    count: VMInt;
  end;

function socls_List_TypeQuery( soself: PSOInstance ): String;
begin
  SelfCheck(soself,so_list_class);
  Result := C_SOTYPE_LIST_NAME;
end;

procedure socls_List_GCEnumerator(instance: PSOInstance; tracer: TGarbageCollectorTracer);
var d: PSOListElement;
begin
  d := PSO_List(instance)^.first;
  while Assigned(d) do
    begin
      tracer(d^.elem);
      d := d^.next;
    end;
end;

procedure socls_List_PostConstructor(instance: PSOInstance);
begin
  PSO_List(instance)^.first := nil;
  PSO_List(instance)^.last := nil;
  PSO_List(instance)^.count := 0;
end;

procedure socls_List_PreDestructor(instance: PSOInstance);
var d,n: PSOListElement;
begin
  d := PSO_List(instance)^.first;
  while Assigned(d) do
    begin
      n := d^.next;
      Dispose(d);
      d := n;
    end;
  PSO_List(instance)^.first := nil;
  PSO_List(instance)^.last := nil;
  PSO_List(instance)^.count := 0;
end;

function socls_List_Compare(soself, rightop: PSOInstance): TSOCompareResult;
var ld,rd: PSOListElement;
begin
  SelfCheck(soself,so_list_class);
  if rightop^.IsType(so_list_class) then
    begin
      if soself = rightop then
        Result := socmp_isEqual
      else if PSO_List(soself)^.count < PSO_List(rightop)^.count then
        Result := socmp_isLess
      else if PSO_List(soself)^.count > PSO_List(rightop)^.count then
        Result := socmp_isGreater
      else
        begin
          Result := socmp_isEqual;
          ld := PSO_List(soself)^.first;
          rd := PSO_List(rightop)^.first;
          while (Result = socmp_isEqual) and
                Assigned(ld) do
            begin
              Result := ld^.elem^.GetTypeCls^.Compare(ld^.elem,rd^.elem);
              ld := ld^.next;
              rd := rd^.next;
            end;
        end;
    end
  else
    Result := socmp_NotComparable;
end;

function socls_List_MethodCall( callinfo: PMethodCallInfo ): PSOInstance;
{generic, lookup method and call}
var m: TSOMethodHandler;
begin
  with callinfo^ do
    begin
      m := TSOMethodHandler( TMethodTrie_List.LookupByHash(hashk,name) );
      if Assigned(m) then
        Result := m( callinfo )
      else
        Result := nil;
    end;
end;

(*******************************************************************************
  LIST Interator
 ******************************************************************************)

type
  TSOList_IOListIterator = class(TInternalObject)
    private
      FLstElem: PSOListElement;
    public
      function Next( callinfo: PMethodCallInfo ): PSOInstance;
      function Current( attrinfo: PAttributeInfo ): PSOInstance;
      constructor Create; override;
      destructor Destroy; override;
  end;

{ TSOList_IOListIterator }

function TSOList_IOListIterator.Next( callinfo: PMethodCallInfo ): PSOInstance;
begin
  ASSERT(Length(FReferings) = 1);
  if Assigned(FLstElem) then
    begin
      ASSERT(PSO_List(FReferings[0])^.GetLocked);
      FLstElem := FLstElem^.next;
    end
  else
    begin
      // inc lock on first place touch
      PSO_List(FReferings[0])^.IncLocks;
      FLstElem := PSO_List(FReferings[0])^.first;
    end;
  if Assigned(FLstElem) then
    Result := so_true
  else
    begin
      Result := so_false;
      // dec lock on end
      PSO_List(FReferings[0])^.DecLocks;
    end;
end;

function TSOList_IOListIterator.Current( attrinfo: PAttributeInfo ): PSOInstance;
{current value, check iterator, check lock on list, return list value
  also allows setting}
begin
  Result := nil;
  if Assigned(FLstElem) then
    begin
      ASSERT(PSO_List(FReferings[0])^.GetLocked);
      if not Assigned(attrinfo^.setter) then
        begin
          {getter, return value and incref}
          Result := FLstElem^.elem;
          Result^.IncRef;
        end
      else
        begin
          {setter, decref/remove val, set setter, incref twice for
           return value and for list entry}
          FLstElem^.elem^.DecRef;
          FLstElem^.elem := attrinfo^.setter;
          Result := attrinfo^.setter;
          Result^.IncRef;
          Result^.IncRef;
        end;
    end;
end;

constructor TSOList_IOListIterator.Create;
begin
  inherited Create;
  FLstElem := nil;
  Self.RegisterMethod(C_STDCALLM_ITER__NEXT,@Next);
  Self.RegisterAttribute(C_STDMEMBR_LISTITER__CURRENT,@Current);
end;

destructor TSOList_IOListIterator.Destroy;
{iterator is collected, check if its still in iteration and those holds a lock
 on the list -> remove it}
begin
  if Assigned(FLstElem) then
    PSO_List(FReferings[0])^.DecLocks; // didnt iter til end -> unlock
  inherited Destroy;
end;


(*******************************************************************************
  LIST Methods/Attr/Helpers
 ******************************************************************************)

function so_list_init: PSOInstance;
begin
  Result := InitInstance(so_list_class);
end;

function internal_list_append( soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
var elem: PSOListElement;
    i: Integer;
begin
  check_so_maxargs(argnum);
  for i := 0 to argnum-1 do
    begin
      elem := New(PSOListElement);
      elem^.next := nil;
      elem^.prev := PSO_List(soself)^.last;
      elem^.elem := soargs^[i];
      if Assigned(PSO_List(soself)^.last) then
        PSO_List(soself)^.last^.next := elem
      else
        PSO_List(soself)^.first := elem;
      PSO_List(soself)^.last := elem;
      Inc(PSO_List(soself)^.count,1);
      elem^.elem^.IncRef;
    end;
  soself^.IncRef;
  Result := soself;
end;

function so_list_append(soself: PSOInstance; soargs: PSOMethodVarArgs;
  argnum: Integer): PSOInstance;
begin
  check_so_maxcollection( PSO_List(soself)^.count + argnum );
  if not soself^.GetLocked then
    begin
      Result := internal_list_append(soself,soargs,argnum);
    end
  else
    Result := init_lockmod(soself,C_STDCALLM_LIST__APPEND);
end;

function so_list_count(plst: PSOInstance): VMInt;
begin
  ASSERT(plst^.IsType(so_list_class));
  Result := PSO_List(plst)^.count;
end;

function so_list_iter_next(plst: PSOInstance; var iter: Pointer): Boolean;
begin
  ASSERT(plst^.IsType(so_list_class));
  if Assigned(iter) then
    begin
      iter := PSOListElement(iter)^.next;
    end
  else
    begin
      iter := PSO_List(plst)^.first;
    end;
  Result := Assigned(iter);
end;

function so_list_iter_prev(plst: PSOInstance; var iter: Pointer): Boolean;
begin
  ASSERT(plst^.IsType(so_list_class));
  if Assigned(iter) then
    begin
      iter := PSOListElement(iter)^.prev;
    end
  else
    begin
      iter := PSO_List(plst)^.last;
    end;
  Result := Assigned(iter);
end;

function so_list_iter_getval(iter: Pointer): PSOInstance;
begin
  ASSERT(Assigned(iter));
  Result := PSOListElement(iter)^.elem;
end;

function so_list_iter_setval(iter: Pointer; val: PSOInstance): PSOInstance;
begin
  ASSERT(Assigned(iter));
  Result := PSOListElement(iter)^.elem;
  PSOListElement(iter)^.elem := val;
end;

{LIST::Length()}
function _List_Length_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      if argnum = 0 then
        begin
          Result := so_integer_init(PSO_List(soself)^.count);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{LIST::Append(<arg>+) -- modifying -> check im-locks}
function _List_Append_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      if argnum > 0 then
        begin
          if not soself^.GetLocked then
            begin
              check_so_maxcollection( PSO_List(soself)^.count + argnum );
              Result := internal_list_append(soself,soargs,argnum);
            end
          else
            Result := init_lockmod(soself,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{LIST::Iterator() -> iterator object}
function _List_Iterator_(callinfo: PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      if argnum = 0 then
        begin
          Result := so_create_internalobject_bound(C_SOTYPE_IO_LISTITER_NAME,TSOList_IOListIterator,soself);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;
