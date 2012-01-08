{   Include for sonull's System and Subsystem types

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
  SUBSYSTEM FunctionWrapper
 ******************************************************************************)

type
  PSO_SubsystemFW = ^TSO_SubsystemFW;
  TSO_SubsystemFW = object(TSOInstance)
     typename: String;
     data: Pointer;
     disptab: TDispTables;
     unload: TSystemUnloadCallBack
  end;

  TSOSubsystemFW = class(TSOTypeBase)
    protected
      class function InstanceSize: MachineInt; override;
      class procedure PreDestructor(instance: PSOInstance); override;
    public
      class function BaseTypeName: String; override;
      class function GetMember(soself: PSOInstance; const name: String): PSOInstance; override;
      class function SetMember(soself, value: PSOInstance; const name: String): PSOInstance; override;
      class function MethodCall(soself: PSOInstance; const name: String; args: PSOMethodVarArgs; argnum: VMInt): PSOInstance; override;
      class function TypeQuery(soself: PSOInstance): String; override;
  end;

{ TSOSubsystemFW }

class function TSOSubsystemFW.InstanceSize: MachineInt;
begin
  Result := SizeOf(TSO_SubsystemFW);
end;

class procedure TSOSubsystemFW.PreDestructor(instance: PSOInstance);
begin
  SetLength(PSO_SubsystemFW(instance)^.typename,0);
  if Assigned(PSO_SubsystemFW(instance)^.unload) then
    PSO_SubsystemFW(instance)^.unload(PSO_SubsystemFW(instance)^.data);
  PSO_SubsystemFW(instance)^.disptab.MethProc.Done;
  PSO_SubsystemFW(instance)^.disptab.AttrProc.Done;
end;

class function TSOSubsystemFW.BaseTypeName: String;
begin
  Result := C_SOTYPE_SUBSYSTEM_NAME;
end;

class function TSOSubsystemFW.GetMember(soself: PSOInstance; const name: String
  ): PSOInstance;
var attrh: TSSDAttributHandlerP;
begin
  SelfCheck(soself,self);
  attrh := TSSDAttributHandlerP(PSO_SubsystemFW(soself)^.disptab.AttrProc.Lookup(name));
  if Assigned(attrh) then
    begin
      Result := attrh(name,PSO_SubsystemFW(soself)^.data,soself,nil);
      if not Assigned(Result) then
        Result := init_subsys_attr_error( soself, name);
    end
  else
    Result := inherited GetMember(soself, name);
end;

class function TSOSubsystemFW.SetMember(soself, value: PSOInstance;
  const name: String): PSOInstance;
var attrh: TSSDAttributHandlerP;
begin
  SelfCheck(soself,self);
  attrh := TSSDAttributHandlerP(PSO_SubsystemFW(soself)^.disptab.AttrProc.Lookup(name));
  if Assigned(attrh) then
    begin
      Result := attrh(name,PSO_SubsystemFW(soself)^.data,soself,value);
      if not Assigned(Result) then
        Result := init_subsys_attr_error( soself, name );
    end
  else
    Result := inherited SetMember(soself, value, name);
end;

class function TSOSubsystemFW.MethodCall(soself: PSOInstance;
  const name: String; args: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
var methh: TSSDMethodHandlerP;
begin
  SelfCheck(soself,self);
  methh := TSSDMethodHandlerP(PSO_SubsystemFW(soself)^.disptab.MethProc.Lookup(name));
  if Assigned(methh) then
    begin
      check_so_maxargs(argnum);
      Result := methh(name,PSO_SubsystemFW(soself)^.data,soself,args,argnum);
      if not Assigned(Result) then
        Result := init_subsys_meth_error( soself, name );
    end
  else
    Result := inherited MethodCall(soself, name, args, argnum);
end;

class function TSOSubsystemFW.TypeQuery(soself: PSOInstance): String;
begin
  Result := C_SOTYPE_SUBSYS_BASENAME+PSO_SubsystemFW(soself)^.typename;
end;

(*******************************************************************************
  SUBSYSTEM Function Reg
 ******************************************************************************)

procedure RegisterSubSystemAttribute(var disptab: TDispTables;
  const name: String; attrh: TSSDAttributHandlerP);
begin
  if not Assigned(attrh) then
     put_internalerror(2011121987);
  if Assigned(disptab.AttrProc.Add(Upcase(name),attrh)) then
    put_internalerror(2011121988); // dup entry
end;

procedure RegisterSubSystemMethod(var disptab: TDispTables; const name: String;
  methh: TSSDMethodHandlerP);
begin
  if not Assigned(methh) then
     put_internalerror(2011121989);
  if Assigned(disptab.MethProc.Add(Upcase(name),methh)) then
    put_internalerror(2011121990); // dup entry
end;

(*******************************************************************************
  SUBSYSTEM ObjectWrapper
 ******************************************************************************)

type
  PSO_SubsystemOW = ^TSO_SubsystemOW;
  TSO_SubsystemOW = object(TSOInstance)
     typename: String;
     ssdisp: TSubSystemDispatcher;
  end;

  TSOSubsystemOW = class(TSOTypeBase)
    protected
      class function InstanceSize: MachineInt; override;
      class procedure PreDestructor(instance: PSOInstance); override;
    public
      class function BaseTypeName: String; override;
      class function GetMember(soself: PSOInstance; const name: String): PSOInstance; override;
      class function SetMember(soself, value: PSOInstance; const name: String): PSOInstance; override;
      class function MethodCall(soself: PSOInstance; const name: String; args: PSOMethodVarArgs; argnum: VMInt): PSOInstance; override;
      class function TypeQuery(soself: PSOInstance): String; override;
  end;

{ TSOSubsystemOW }

class function TSOSubsystemOW.InstanceSize: MachineInt;
begin
  Result := SizeOf(TSO_SubsystemOW);
end;

class procedure TSOSubsystemOW.PreDestructor(instance: PSOInstance);
begin
  SetLength(PSO_SubsystemOW(instance)^.typename,0);
  PSO_SubsystemOW(instance)^.ssdisp.OnSubSystemUnLoad;
  PSO_SubsystemOW(instance)^.ssdisp.Free;
end;

class function TSOSubsystemOW.BaseTypeName: String;
begin
  Result := C_SOTYPE_SUBSYSTEM_NAME;
end;

class function TSOSubsystemOW.GetMember(soself: PSOInstance; const name: String
  ): PSOInstance;
var attrhe: PSSDAttrEntry;
begin
  SelfCheck(soself,self);
  attrhe := PSSDAttrEntry(PSO_SubsystemOW(soself)^.ssdisp.FDispTab.AttrProc.Lookup(name));
  if Assigned(attrhe) then
    begin
      Result := attrhe^(name,soself,nil);
      if not Assigned(Result) then
        Result := init_subsys_attr_error( soself, name);
    end
  else
    Result := inherited GetMember(soself, name);
end;

class function TSOSubsystemOW.SetMember(soself, value: PSOInstance;
  const name: String): PSOInstance;
var attrhe: PSSDAttrEntry;
begin
  SelfCheck(soself,self);
  attrhe := PSSDAttrEntry(PSO_SubsystemOW(soself)^.ssdisp.FDispTab.AttrProc.Lookup(name));
  if Assigned(attrhe) then
    begin
      Result := attrhe^(name,soself,value);
      if not Assigned(Result) then
        Result := init_subsys_attr_error( soself, name);
    end
  else
    Result := inherited SetMember(soself, value, name);
end;

class function TSOSubsystemOW.MethodCall(soself: PSOInstance;
  const name: String; args: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
var methhe: PSSDMethEntry;
begin
  SelfCheck(soself,self);
  methhe := PSSDMethEntry(PSO_SubsystemOW(soself)^.ssdisp.FDispTab.MethProc.Lookup(name));
  if Assigned(methhe) then
    begin
      check_so_maxargs(argnum);
      Result := methhe^(name,soself,args,argnum);
      if not Assigned(Result) then
        Result := init_subsys_meth_error( soself, name );
    end
  else
    Result := inherited MethodCall(soself, name, args, argnum);
end;

class function TSOSubsystemOW.TypeQuery(soself: PSOInstance): String;
begin
  Result := C_SOTYPE_SUBSYS_BASENAME+PSO_SubsystemOW(soself)^.typename;
end;

(*******************************************************************************
  SUBSYSTEM DISPATCHER
 ******************************************************************************)

{ TSubSystemDispatcher }

procedure TSubSystemDispatcher.RegisterAttribute(const name: String;
  attrh: TSSDAttributHandlerM);
var pssdae: PSSDAttrEntry;
begin
  if not Assigned(attrh) then
     put_internalerror(2011121980);
  pssdae := New(PSSDAttrEntry);
  pssdae^ := attrh;
  pssdae := FDispTab.AttrProc.Add(Upcase(name),pssdae);
  if Assigned(pssdae) then
    put_internalerror(2011121981); // dup entry
end;

procedure TSubSystemDispatcher.RegisterMethod(const name: String;
  methh: TSSDMethodHandlerM);
var pssdme: PSSDMethEntry;
begin
  if not Assigned(methh) then
     put_internalerror(2011121982);
  pssdme := New(PSSDMethEntry);
  pssdme^ := methh;
  pssdme := FDispTab.MethProc.Add(Upcase(name),pssdme);
  if Assigned(pssdme) then
    put_internalerror(2011121983); // dup entry
end;

constructor TSubSystemDispatcher.Create;
begin
  FDispTab.AttrProc.Init(0,2);
  FDispTab.MethProc.Init(0,2);
end;

destructor TSubSystemDispatcher.Destroy;
begin
  FDispTab.AttrProc.ForEach(@pssdae_disposer,nil);
  FDispTab.AttrProc.Done;
  FDispTab.MethProc.ForEach(@pssdme_disposer,nil);
  FDispTab.MethProc.Done;
  inherited Destroy;
end;

procedure TSubSystemDispatcher.OnSubSystemLoad;
begin
  // nop
end;

procedure TSubSystemDispatcher.OnSubSystemUnLoad;
begin
  // nop
end;

(*******************************************************************************
  SYSTEM SINGLETON
 ******************************************************************************)

type
  PSubsystemEntry = ^TSubsystemEntry;
  TSubsystemEntry = record
    ssinstance: PSOInstance;
    canunload: Boolean;
    case oopwrapper: Boolean of
      true: ( dispcls: TSubSysDispatcherCls );
      false: ( load: TSystemLoadCallBack; unload: TSystemUnloadCallBack );
  end;

  PSO_System = ^TSO_System;
  TSO_System = object(TSOInstance)
    subsysteme: THashTrie { of PSubsystemEntry };
  end;

  TSOSystem = class(TSOTypeBase)
    protected
      class function IsMortal: Boolean; override;
      class function InstanceSize: MachineInt; override;
      class procedure PostConstructor(instance: PSOInstance); override;
      class procedure PreDestructor(instance: PSOInstance); override;
      class procedure GCEnumerator(instance: PSOInstance; tracer: TGarbageCollectorTracer); override;
    public
      class function BaseTypeName: String; override;
      class function GetMember(soself: PSOInstance; const name: String): PSOInstance; override;
      class function SetMember(soself, value: PSOInstance; const name: String): PSOInstance; override;
  end;

{ TSOSystem }

class function TSOSystem.IsMortal: Boolean;
begin
  Result := false;
end;

class function TSOSystem.InstanceSize: MachineInt;
begin
  Result := SizeOf(TSO_System);
end;

class procedure TSOSystem.PostConstructor(instance: PSOInstance);
begin
  PSO_System(instance)^.subsysteme.Init(0,2);
end;

function psse_disposer( const unused: String; data, unused2: Pointer ): Boolean;
begin
  Dispose(PSubsystemEntry(data));
  Result := true;
end;

class procedure TSOSystem.PreDestructor(instance: PSOInstance);
begin
  PSO_System(instance)^.subsysteme.ForEach(@psse_disposer,nil);
  PSO_System(instance)^.subsysteme.Done;
end;

function sys_collenum( const unused: String; data, xdata: Pointer ): Boolean;
begin
  if Assigned(PSubsystemEntry(data)^.ssinstance) then
    TGarbageCollectorTracer(xdata)(PSubsystemEntry(data)^.ssinstance);
  Result := true;
end;

class procedure TSOSystem.GCEnumerator(instance: PSOInstance;
  tracer: TGarbageCollectorTracer);
begin
  PSO_System(instance)^.subsysteme.ForEach(@sys_collenum,tracer);
end;

class function TSOSystem.BaseTypeName: String;
begin
  Result := C_SOTYPE_SYSTEM_NAME;
end;

class function TSOSystem.GetMember(soself: PSOInstance; const name: String
  ): PSOInstance;
var ssentry: PSubsystemEntry;
    f: PSO_SubsystemFW;
    o: PSO_SubsystemOW;
begin
  SelfCheck(soself,self);
  ssentry := PSubsystemEntry(PSO_System(soself)^.subsysteme.Lookup(name));
  if Assigned(ssentry) then
    begin
      if not Assigned(ssentry^.ssinstance) then
        begin
          {load}
          if ssentry^.oopwrapper then
            begin
              o := PSO_SubsystemOW(socore.InitInstance(TSOSubsystemOW));
              o^.typename := name;
              o^.ssdisp := nil;
              try
                o^.ssdisp := ssentry^.dispcls.Create;
              except
                on EInternalException do
                  raise;
                on e:Exception do
                  begin
                    put_debug(C_SOTYPE_SUBSYSTEM_NAME+' '+name+' Load Exception: ' + e.Message);
                    o := nil;
                  end;
              end;
              if Assigned(o) then
                begin
                  try
                    o^.ssdisp.OnSubSystemLoad;
                  except
                    on EInternalException do
                      raise;
                    on e: Exception do
                      begin
                        put_debug(C_SOTYPE_SUBSYSTEM_NAME+' '+name+' Load Exception: ' + e.Message);
                        o := nil;
                      end;
                  end;
                end;
              if Assigned(o) then
                ssentry^.ssinstance := o
              else
                put_critical(C_SOTYPE_SUBSYSTEM_NAME+' '+name+' Startup failed');
            end
          else
            begin
              f := PSO_SubsystemFW(socore.InitInstance(TSOSubsystemFW));
              f^.typename := name;
              f^.disptab.AttrProc.Init(0,2);
              f^.disptab.MethProc.Init(0,2);
              try
                ssentry^.load( f^.disptab, f^.data );
              except
                on EInternalException do
                  raise;
                on e:Exception do
                  begin
                    put_debug(C_SOTYPE_SUBSYSTEM_NAME+' '+name+' Load Exception: ' + e.Message);
                    f := nil;
                  end;
              end;
              if Assigned(f) then
                ssentry^.ssinstance := f
              else
                put_critical(C_SOTYPE_SUBSYSTEM_NAME+' '+name+' Startup failed');
              f^.unload := ssentry^.unload;
            end;
        end;
      Result := ssentry^.ssinstance;
      Result^.IncRef; // for passing
    end
  else
    Result := so_error_init('No such '+C_SOTYPE_SUBSYSTEM_NAME+': '+name);
end;

class function TSOSystem.SetMember(soself, value: PSOInstance;
  const name: String): PSOInstance;
var ssentry: PSubsystemEntry;
    i: PSOInstance;
begin
  ssentry := PSubsystemEntry(PSO_System(soself)^.subsysteme.Lookup(name));
  if value^.IsType(so_none_class) then
    begin
      if Assigned(ssentry) then
        begin
          if Assigned(ssentry^.ssinstance) then
            begin
              if ssentry^.canunload then
                begin
                  i := ssentry^.ssinstance;
                  if i^.GetRefs = 1 then
                    begin
                      ssentry^.ssinstance := nil;
                      i^.DecRef; // thats all
                    end
                  else
                    begin
                      // no, loading the subsystem again would introduce
                      // another subsystem of same type which does stuff agains the
                      // others instance knowledge
                      // special instances must be solved differently
                      Result := so_error_init(C_SOTYPE_SUBSYSTEM_NAME+' '+name+' is still in use');
                    end;
                end
              else
                Result := so_error_init(C_SOTYPE_SUBSYSTEM_NAME+' '+name+' does not allow unloading');
            end;
        end
      else
        Result := so_error_init('No such '+C_SOTYPE_SUBSYSTEM_NAME+': '+name);
    end
  else
    init_invargtype_error(soself,value,1,DEFAULT_METHOD_SetMember);
end;

(*******************************************************************************
  SYSTEM Methods
 ******************************************************************************)

{SYSTEM::Instances()}
function _System_GCInstances_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if argnum = 0 then
    begin
      Result := so_integer_init( TracedInstances );
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;

{SYSTEM::GCollect()}
function _System_GC_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if argnum = 0 then
    begin
      GarbageCollect;
      Result := so_nil;
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;

{SYSTEM::Dummy()}
function _System_DummyObject_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if argnum = 0 then
    Result := so_dummy_instance
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;

{SYSTEM::TypeOf(<any>)}
function _System_TypeOf_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if argnum = 1 then
    Result := so_string_init(soargs^[0]^.GetTypeCls.TypeQuery(soargs^[0]))
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;

{SYSTEM::Marker(str,str) -> nil (dynamic marker switching)}
function _System_Marker_( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
begin
  if argnum = 2 then
    begin
      if soargs^[0]^.IsType(so_string_class) then
        begin
          if soargs^[1]^.IsType(so_string_class) then
            begin
              cscan.StreamScannerSetMarker(so_string_get(soargs^[0]),so_string_get(soargs^[1]));
              Result := so_nil;
            end
          else
            Result := init_invargtype_error(soself,soargs^[1],2,mname);
        end
      else
        Result := init_invargtype_error(soself,soargs^[0],1,mname);
    end
  else
    Result := init_invargnum_error(soself,argnum,mname);
end;

(*******************************************************************************
  SYSTEM Registration
 ******************************************************************************)

procedure RegisterSubsystemObject( const subsysname: String; subsyso: TSubSysDispatcherCls; canunload: Boolean );
var subsyse: PSubsystemEntry;
begin
  subsyse := New(PSubsystemEntry);
  subsyse^.ssinstance := nil;
  subsyse^.canunload := canunload;
  subsyse^.oopwrapper := true;
  subsyse^.dispcls := subsyso;
  subsyse := PSO_System(so_system_instance)^.subsysteme.Add(upcase(subsysname),subsyse);
  if Assigned(subsyse) then
    put_internalerror(2011121984);
{$IFDEF DEBUG}
  internal_register_type(C_SOTYPE_SUBSYS_BASENAME+subsysname);
{$ENDIF}
end;

procedure RegisterSubSystemHandler(const subsysname: String;
  loader: TSystemLoadCallBack; unloader: TSystemUnloadCallBack;
  canunload: Boolean);
var subsyse: PSubsystemEntry;
begin
  subsyse := New(PSubsystemEntry);
  subsyse^.ssinstance := nil;
  subsyse^.canunload := canunload;
  subsyse^.oopwrapper := false;
  if not Assigned(loader) then
    put_internalerror(2011121992); // this should not be nil -> no registration, no methods.. useless object
  subsyse^.load := loader;
  subsyse^.unload := unloader;
  subsyse := PSO_System(so_system_instance)^.subsysteme.Add(upcase(subsysname),subsyse);
  if Assigned(subsyse) then
    put_internalerror(2011121985);
{$IFDEF DEBUG}
  internal_register_type(C_SOTYPE_SUBSYS_BASENAME+subsysname);
{$ENDIF}
end;

