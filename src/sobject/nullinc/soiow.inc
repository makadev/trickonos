{   Include for sonull's Internal Object Wrapper Type

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
 Internal Object Wrapper
 ******************************************************************************)

type
  PSO_IO = ^TSO_IO;
  TSO_IO = object(TSOInstance)
    ioo: TInternalObject;
  end;

procedure socls_IO_PreDestructor(instance: PSOInstance);
begin
  PSO_IO(instance)^.ioo.Free;
  PSO_IO(instance)^.ioo := nil;
end;

procedure socls_IO_GCEnumerator(instance: PSOInstance;
  tracer: TGarbageCollectorTracer);
begin
  PSO_IO(instance)^.ioo.GCTrace(tracer);
end;

function socls_IO_MethodCall(hashk: MachineWord; const name: String; soself: PSOInstance; args: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
begin
  SelfCheck(soself,so_io_class);
  check_so_maxargs(argnum);
  Result := PSO_IO(soself)^.ioo.DispatchMethodCall(hashk,name,soself,args,argnum);
end;

function socls_IO_MethodCallOverride( hashk: MachineWord; const name: String; soself: PSOInstance ): PSOInstance;
begin
  SelfCheck(soself,so_io_class);
  Result := nil;
end;

function socls_IO_TypeQuery(soself: PSOInstance): String;
begin
  SelfCheck(soself,so_io_class);
  Result := C_SOTYPE_IO_BASENAME+PSO_IO(soself)^.ioo.TypeName;
end;

(******************************************************************************
 Internal Object
 ******************************************************************************)

{ TInternalObject }

function TInternalObject.DispatchMethodCall(hashk: MachineWord; const name: String; soself: PSOInstance; args: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
var methhe: PSSDMethEntry;
begin
  methhe := PSSDMethEntry(FDispTab.MethProc.LookupByHash(hashk,name));
  if Assigned(methhe) then
    begin
      Result := methhe^(name,soself,args,argnum);
      if not Assigned(Result) then
        Result := init_object_meth_error( soself, name );
    end
  else
    begin
      {directly check getmember/setmember and dispatch, this allows overriding the
       getmember/setmember with MethodCall}
      Result := nil;
      if (hashk = DEFAULT_METHOD_GetMember_Hash) and
         (name = DEFAULT_METHOD_GetMember) and
         (argnum = 1) then
        begin
          if args^[0]^.IsType(so_string_class) then
            Result := DispatchAttr(so_string_get(args^[0]),soself,nil);
        end
      else if (hashk = DEFAULT_METHOD_SetMember_Hash) and
              (name = DEFAULT_METHOD_SetMember) and
              (argnum = 2) then
        begin
          if args^[0]^.IsType(so_string_class) then
            Result := DispatchAttr(so_string_get(args^[0]),soself,args^[1]);
        end;
    end;
end;

function TInternalObject.DispatchAttr(const aname: String; soself: PSOInstance; setter: PSOInstance): PSOInstance;
{dispatch SetMember/GetMember call to attr handler method}
var attrhe: PSSDAttrEntry;
begin
  attrhe := PSSDAttrEntry(FDispTab.AttrProc.Lookup(aname));
  if Assigned(attrhe) then
    begin
      Result := attrhe^(aname,soself,setter);
      if not Assigned(Result) then
        Result := init_object_attr_error( soself, aname );
    end
  else
    Result := nil;
end;

procedure TInternalObject.GCTrace(tracer: TGarbageCollectorTracer);
{propagate references to tracer}
var i: VMInt;
begin
  if Length(FReferings) > 0 then
    begin
      for i := 0 to High(FReferings) do
        tracer(FReferings[i]);
    end;
end;

procedure TInternalObject.AddReference(refins: PSOInstance);
{add a traced reference}
begin
  if Assigned(refins) then
    begin
      SetLength(FReferings,Length(FReferings)+1);
      FReferings[High(FReferings)] := refins;
      refins^.IncRef;
    end;
end;

procedure TInternalObject.RegisterAttribute(const name: String;
  attrh: TSSDAttributHandlerM);
var pssdae: PSSDAttrEntry;
begin
  if not Assigned(attrh) then
     put_internalerror(2011122100);
  pssdae := New(PSSDAttrEntry);
  pssdae^ := attrh;
  pssdae := FDispTab.AttrProc.Add(Upcase(name),pssdae);
  if Assigned(pssdae) then
    put_internalerror(2011122101); // dup entry
end;

procedure TInternalObject.RegisterMethod(const name: String;
  methh: TSSDMethodHandlerM);
var pssdme: PSSDMethEntry;
begin
  if not Assigned(methh) then
     put_internalerror(2011122102);
  pssdme := New(PSSDMethEntry);
  pssdme^ := methh;
  pssdme := FDispTab.MethProc.Add(Upcase(name),pssdme);
  if Assigned(pssdme) then
    put_internalerror(2011122103); // dup entry
end;

function TInternalObject.TypeName: String;
begin
  Result := FTypeName;
end;

constructor TInternalObject.Create;
begin
  FDispTab.AttrProc.Init(0,1);
  FDispTab.MethProc.Init(0,1);
  SetLength(FReferings,0);
end;

destructor TInternalObject.Destroy;
begin
  SetLength(FReferings,0);
  FDispTab.AttrProc.ForEach(@pssdae_disposer,nil);
  FDispTab.AttrProc.Done;
  FDispTab.MethProc.ForEach(@pssdme_disposer,nil);
  FDispTab.MethProc.Done;
  inherited Destroy;
end;


procedure RegisterInternalObjectName(const ioname: String);
begin
{$IFDEF DEBUG}
  internal_register_type(C_SOTYPE_IO_BASENAME+ioname);
{$ENDIF}
end;

function so_create_internalobject(const ioname: String; iotcls: TIntObjectClass
  ): PSOInstance;
begin
  Result := socore.InitInstance(so_io_class);
  PSO_IO(Result)^.ioo := iotcls.Create;
  PSO_IO(Result)^.ioo.FTypeName := ioname;
end;

function so_create_internalobject_bound(const ioname: String;
  iotcls: TIntObjectClass; pref: PSOInstance): PSOInstance;
begin
  Result := so_create_internalobject(ioname,iotcls);
  PSO_IO(Result)^.ioo.AddReference(pref);
end;

function so_internalobject_get(soios: PSOInstance): TInternalObject;
begin
  SelfCheck(soios,so_io_class);
  Result := PSO_IO(soios)^.ioo;
end;



