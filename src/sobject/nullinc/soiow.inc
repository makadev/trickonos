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
  PSO_IOW = ^TSO_IOW;
  TSO_IOW = object(TSOInstance)
    ioo: TInternalObject;
  end;

  TSOTypeInternalObjectW = class(TSOTypeBase)
    protected
      class function InstanceSize: MachineInt; override;
      class procedure PreDestructor( instance: PSOInstance ); override;
      class procedure PreCollect( instance: PSOInstance ); override;
      class procedure GCEnumerator( instance: PSOInstance; tracer: TGarbageCollectorTracer ); override;
    public
      class function BaseTypeName: String; override;
      class function MethodCall( soself: PSOInstance; const name: String; args: PSOMethodVarArgs; argnum: VMInt ): PSOInstance; override;
      class function TypeQuery( soself: PSOInstance ): String; override;
      class function SetMember( soself, value: PSOInstance; const name: String ): PSOInstance; override;
      class function GetMember( soself: PSOInstance; const name: String ): PSOInstance; override;
      class function SetIndex( soself, idx, value: PSOInstance ): PSOInstance; override;
      class function GetIndex( soself, idx: PSOInstance): PSOInstance; override;
      class function BinOpAdd( soself, rightop: PSOInstance ): PSOInstance; override;
      class function BinOpSub( soself, rightop: PSOInstance ): PSOInstance; override;
      class function BinOpMul( soself, rightop: PSOInstance ): PSOInstance; override;
      class function BinOpDiv( soself, rightop: PSOInstance ): PSOInstance; override;
      class function BinOpMod( soself, rightop: PSOInstance ): PSOInstance; override;
      class function BinOpXor( soself, rightop: PSOInstance ): PSOInstance; override;
      class function BinOpShl( soself, rightop: PSOInstance ): PSOInstance; override;
      class function BinOpShr( soself, rightop: PSOInstance ): PSOInstance; override;
      class function BinOpRol( soself, rightop: PSOInstance ): PSOInstance; override;
      class function BinOpRor( soself, rightop: PSOInstance ): PSOInstance; override;
      class function BinOpAnd( soself, rightop: PSOInstance ): PSOInstance; override;
      class function BinOpOr( soself, rightop: PSOInstance ): PSOInstance; override;
      class function UnOpNeg( soself: PSOInstance ): PSOInstance; override;
      class function UnOpAbs( soself: PSOInstance ): PSOInstance; override;
      class function UnOpNot( soself: PSOInstance ): PSOInstance; override;
      class function DirectCall( soself: PSOInstance; args: PSOMethodVarArgs; argnum: VMInt ): PSOInstance; override;
  end;

{ TInternalObjectWrapper }

class function TSOTypeInternalObjectW.InstanceSize: MachineInt;
begin
  Result := SizeOf(TSO_IOW);
end;

class procedure TSOTypeInternalObjectW.PreDestructor(instance: PSOInstance);
begin
  SelfCheck(instance,self);
  PSO_IOW(instance)^.ioo.Free;
  PSO_IOW(instance)^.ioo := nil;
end;

class procedure TSOTypeInternalObjectW.PreCollect(instance: PSOInstance);
begin
  SelfCheck(instance,self);
  PSO_IOW(instance)^.ioo.PreCollect;
end;

class procedure TSOTypeInternalObjectW.GCEnumerator(instance: PSOInstance;
  tracer: TGarbageCollectorTracer);
begin
  SelfCheck(instance,self);
  PSO_IOW(instance)^.ioo.GCTrace(tracer);
end;

class function TSOTypeInternalObjectW.BaseTypeName: String;
begin
  Result := C_SOTYPE_IO_NAME;
end;

class function TSOTypeInternalObjectW.MethodCall(soself: PSOInstance;
  const name: String; args: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
begin
  SelfCheck(soself,self);
  check_so_maxargs(argnum);
  Result := PSO_IOW(soself)^.ioo.DispatchMethodCall(soself,name,args,argnum);
  if not Assigned(Result) then
    Result := inherited MethodCall(soself, name, args, argnum);
end;

class function TSOTypeInternalObjectW.TypeQuery(soself: PSOInstance): String;
begin
  SelfCheck(soself,self);
  Result := C_SOTYPE_IO_BASENAME+PSO_IOW(soself)^.ioo.TypeName;
end;

class function TSOTypeInternalObjectW.SetMember(soself, value: PSOInstance;
  const name: String): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.DispatchAttrSet(name,soself,value);
  if not Assigned(Result) then
    Result := inherited SetMember(soself, value, name);
end;

class function TSOTypeInternalObjectW.GetMember(soself: PSOInstance;
  const name: String): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.DispatchAttrGet(name,soself);
  if not Assigned(Result) then
    Result := inherited GetMember(soself, name);
end;

class function TSOTypeInternalObjectW.SetIndex(soself, idx, value: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.SetIndex(soself,idx,value);
  if not Assigned(Result) then
    Result := inherited SetIndex(soself, idx, value);
end;

class function TSOTypeInternalObjectW.GetIndex(soself, idx: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.GetIndex(soself,idx);
  if not Assigned(Result) then
    Result := inherited GetIndex(soself, idx);
end;

class function TSOTypeInternalObjectW.BinOpAdd(soself, rightop: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.BinOpAdd(soself,rightop);
  if not Assigned(Result) then
    Result := inherited BinOpAdd(soself, rightop);
end;

class function TSOTypeInternalObjectW.BinOpSub(soself, rightop: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.BinOpSub(soself,rightop);
  if not Assigned(Result) then
    Result := inherited BinOpSub(soself, rightop);
end;

class function TSOTypeInternalObjectW.BinOpMul(soself, rightop: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.BinOpMul(soself,rightop);
  if not Assigned(Result) then
    Result := inherited BinOpMul(soself, rightop);
end;

class function TSOTypeInternalObjectW.BinOpDiv(soself, rightop: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.BinOpDiv(soself,rightop);
  if not Assigned(Result) then
    Result := inherited BinOpDiv(soself, rightop);
end;

class function TSOTypeInternalObjectW.BinOpMod(soself, rightop: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.BinOpMod(soself,rightop);
  if not Assigned(Result) then
    Result := inherited BinOpMod(soself, rightop);
end;

class function TSOTypeInternalObjectW.BinOpXor(soself, rightop: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.BinOpXor(soself,rightop);
  if not Assigned(Result) then
    Result := inherited BinOpXor(soself, rightop);
end;

class function TSOTypeInternalObjectW.BinOpShl(soself, rightop: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.BinOpShl(soself,rightop);
  if not Assigned(Result) then
    Result := inherited BinOpShl(soself, rightop);
end;

class function TSOTypeInternalObjectW.BinOpShr(soself, rightop: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.BinOpShr(soself,rightop);
  if not Assigned(Result) then
    Result := inherited BinOpShr(soself, rightop);
end;

class function TSOTypeInternalObjectW.BinOpRol(soself, rightop: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.BinOpRol(soself,rightop);
  if not Assigned(Result) then
    Result := inherited BinOpRol(soself, rightop);
end;

class function TSOTypeInternalObjectW.BinOpRor(soself, rightop: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.BinOpRor(soself,rightop);
  if not Assigned(Result) then
    Result := inherited BinOpRor(soself, rightop);
end;

class function TSOTypeInternalObjectW.BinOpAnd(soself, rightop: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.BinOpAnd(soself,rightop);
  if not Assigned(Result) then
    Result := inherited BinOpAnd(soself, rightop);
end;

class function TSOTypeInternalObjectW.BinOpOr(soself, rightop: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.BinOpOr(soself,rightop);
  if not Assigned(Result) then
    Result := inherited BinOpOr(soself, rightop);
end;

class function TSOTypeInternalObjectW.UnOpNeg(soself: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.UnOpNeg(soself);
  if not Assigned(Result) then
    Result := inherited UnOpNeg(soself);
end;

class function TSOTypeInternalObjectW.UnOpAbs(soself: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.UnOpAbs(soself);
  if not Assigned(Result) then
    Result := inherited UnOpAbs(soself);
end;

class function TSOTypeInternalObjectW.UnOpNot(soself: PSOInstance
  ): PSOInstance;
begin
  SelfCheck(soself,self);
  Result := PSO_IOW(soself)^.ioo.UnOpNot(soself);
  if not Assigned(Result) then
    Result := inherited UnOpNot(soself);
end;

class function TSOTypeInternalObjectW.DirectCall(soself: PSOInstance;
  args: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
begin
  SelfCheck(soself,self);
  check_so_maxargs(argnum);
  Result := PSO_IOW(soself)^.ioo.DirectCall(soself,args,argnum);
  if not Assigned(Result) then
    Result := inherited DirectCall(soself, args, argnum);
end;

(******************************************************************************
 Internal Object
 ******************************************************************************)

{ TInternalObject }

function TInternalObject.DispatchMethodCall(soself: PSOInstance;
  const name: String; args: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
var methhe: PSSDMethEntry;
begin
  methhe := PSSDMethEntry(FDispTab.MethProc.Lookup(name));
  if Assigned(methhe) then
    begin
      Result := methhe^(name,soself,args,argnum);
      if not Assigned(Result) then
        Result := init_object_meth_error( soself, name );
    end
  else
    Result := nil;
end;

function TInternalObject.DispatchAttrGet(const aname: String;
  soself: PSOInstance): PSOInstance;
var attrhe: PSSDAttrEntry;
begin
  attrhe := PSSDAttrEntry(FDispTab.AttrProc.Lookup(aname));
  if Assigned(attrhe) then
    begin
      Result := attrhe^(aname,soself,nil);
      if not Assigned(Result) then
        Result := init_object_attr_error( soself, aname );
    end
  else
    Result := nil;
end;

function TInternalObject.DispatchAttrSet(const aname: String;
  soself: PSOInstance; setter: PSOInstance): PSOInstance;
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
var i: Integer;
begin
  if Length(FReferings) > 0 then
    begin
      for i := 0 to High(FReferings) do
        tracer(FReferings[i]);
    end;
end;

procedure TInternalObject.AddReference(refins: PSOInstance);
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

function TInternalObject.SetIndex(soself, idx, value: PSOInstance
  ): PSOInstance;
begin
  Result := nil;
end;

function TInternalObject.GetIndex(soself, idx: PSOInstance): PSOInstance;
begin
  Result := nil;
end;

function TInternalObject.BinOpAdd(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := nil;
end;

function TInternalObject.BinOpSub(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := nil;
end;

function TInternalObject.BinOpMul(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := nil;
end;

function TInternalObject.BinOpDiv(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := nil;
end;

function TInternalObject.BinOpMod(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := nil;
end;

function TInternalObject.BinOpShl(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := nil;
end;

function TInternalObject.BinOpShr(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := nil;
end;

function TInternalObject.BinOpRol(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := nil;
end;

function TInternalObject.BinOpRor(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := nil;
end;

function TInternalObject.BinOpAnd(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := nil;
end;

function TInternalObject.BinOpOr(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := nil;
end;

function TInternalObject.BinOpXor(soself, rightop: PSOInstance): PSOInstance;
begin
  Result := nil;
end;

function TInternalObject.UnOpNeg(soself: PSOInstance): PSOInstance;
begin
  Result := nil;
end;

function TInternalObject.UnOpAbs(soself: PSOInstance): PSOInstance;
begin
  Result := nil;
end;

function TInternalObject.UnOpNot(soself: PSOInstance): PSOInstance;
begin
  Result := nil;
end;

function TInternalObject.DirectCall(soself: PSOInstance;
  args: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
begin
  Result := nil;
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

procedure TInternalObject.PreCollect;
begin
  // nop
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
  Result := socore.InitInstance(TSOTypeInternalObjectW);
  PSO_IOW(Result)^.ioo := iotcls.Create;
  PSO_IOW(Result)^.ioo.FTypeName := ioname;
end;

function so_create_internalobject_bound(const ioname: String;
  iotcls: TIntObjectClass; pref: PSOInstance): PSOInstance;
begin
  Result := so_create_internalobject(ioname,iotcls);
  PSO_IOW(Result)^.ioo.AddReference(pref);
end;

function so_internalobject_get(soios: PSOInstance): TInternalObject;
begin
  if soios^.IsType(TSOTypeInternalObjectW) then
    Result := PSO_IOW(soios)^.ioo
  else
    put_internalerror(2012011200);
end;



