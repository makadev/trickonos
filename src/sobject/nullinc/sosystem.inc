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
  PSO_SubsystemFW = ^TSO_SubSys_FW;
  TSO_SubSys_FW = object(TSOInstance)
     typename: String;
     data: Pointer;
     disptab: TDispTables;
     unload: TSystemUnloadCallBack
  end;

procedure socls_SubSys_FW_PreDestructor(instance: PSOInstance);
begin
  SetLength(PSO_SubsystemFW(instance)^.typename,0);
  if Assigned(PSO_SubsystemFW(instance)^.unload) then
    PSO_SubsystemFW(instance)^.unload(PSO_SubsystemFW(instance)^.data);
  PSO_SubsystemFW(instance)^.disptab.MethProc.Done;
  PSO_SubsystemFW(instance)^.disptab.AttrProc.Done;
end;

function SubSys_FW_Attr_Dispatch( attrinfo: PAttributeInfo ): PSOInstance; inline;
{Get/SetMember (member select) broker,
  lookup registered method for certain attribute name and call it
  or generate default error}
var attrh: TSSDAttributHandlerP;
begin
  with attrinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_subsys_fw_class);{$ENDIF}
      attrh := TSSDAttributHandlerP(PSO_SubsystemFW(soself)^.disptab.AttrProc.Lookup(name));
      if Assigned(attrh) then
        begin
          Result := attrh(attrinfo);
          if not Assigned(Result) then
            Result := init_subsys_attr_error( soself, name );
        end
      else
        Result := init_subsys_attr_error(soself,name);
    end;
end;

function socls_SubSys_Fw_MethodCall( callinfo: PMethodCallInfo ): PSOInstance;
{MethodCall broker,
  lookup registered method for certain attribute methodname and call it
  or return nil which generates default error}
var methh: TSSDMethodHandlerP;
    attrinfo: TAttributeInfo;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_subsys_fw_class);{$ENDIF}
      methh := TSSDMethodHandlerP(PSO_SubsystemFW(soself)^.disptab.MethProc.LookupByHash(hashk,name));
      if Assigned(methh) then
        begin
          check_so_maxargs(argnum);
          Result := methh(callinfo);
          if not Assigned(Result) then
            Result := init_subsys_meth_error( soself, name );
        end
      else
        begin
          {directly check getmember/setmember and dispatch, this allows overriding the
           getmember/setmember with MethodCall}
          if (hashk = DEFAULT_METHOD_GetMember_Hash) and
             (ucfs_compare(name,ci_us(ci_dm_getmember,false))=0) and
             (argnum = 1) and
             (soargs^[0]^.IsType(so_string_class)) then
            begin
              attrinfo.name := so_string_get_ucfs(soargs^[0],false);
              attrinfo.soself := soself;
              attrinfo.setter := nil;
              Result := SubSys_FW_Attr_Dispatch(@attrinfo);
            end
          else if (hashk = DEFAULT_METHOD_SetMember_Hash) and
                  (ucfs_compare(name,ci_us(ci_dm_setmember,false))=0) and
                  (argnum = 2) and
                  (soargs^[0]^.IsType(so_string_class)) then
            begin
              attrinfo.name := so_string_get_ucfs(soargs^[0],false);
              attrinfo.soself := soself;
              attrinfo.setter := soargs^[1];
              Result := SubSys_FW_Attr_Dispatch(@attrinfo);
            end
          else
            Result := init_subsys_meth_error(soself,name);
        end;
    end;
end;

function socls_SubSys_Fw_TypeQuery(soself: PSOInstance): String;
begin
  {$IFDEF SELFCHECK}SelfCheck(soself,so_subsys_fw_class);{$ENDIF}
  Result := C_SOTYPE_SUBSYS_BASENAME+PSO_SubsystemFW(soself)^.typename;
end;

(*******************************************************************************
  SUBSYSTEM Function Reg / Helpers
 ******************************************************************************)

function subsysfw_get_data( soself: PSOInstance ): Pointer;
begin
  {$IFDEF SELFCHECK}SelfCheck(soself,so_subsys_fw_class);{$ENDIF}
  Result := PSO_SubsystemFW(soself)^.data;
end;

procedure subsysfw_set_data( soself: PSOInstance; data: Pointer );
begin
  {$IFDEF SELFCHECK}SelfCheck(soself,so_subsys_fw_class);{$ENDIF}
  PSO_SubsystemFW(soself)^.data := data;
end;

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
  PSO_SubsystemOW = ^TSO_SubSys_OW;
  TSO_SubSys_OW = object(TSOInstance)
     typename: String;
     ssdisp: TSubSystemDispatcher;
  end;

procedure socls_SubSys_OW_PreDestructor(instance: PSOInstance);
begin
  SetLength(PSO_SubsystemOW(instance)^.typename,0);
  PSO_SubsystemOW(instance)^.ssdisp.OnSubSystemUnLoad;
  PSO_SubsystemOW(instance)^.ssdisp.Free;
end;

function SubSys_OW_Attr_Dispatch(attrinfo:PAttributeInfo): PSOInstance; inline;
{Attribute to Attr Handler dispatcher}
var attrhe: PSSDAttrEntry;
begin
  with attrinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_subsys_ow_class);{$ENDIF}
      attrhe := PSSDAttrEntry(PSO_SubsystemOW(soself)^.ssdisp.FDispTab.AttrProc.Lookup(name));
      if Assigned(attrhe) then
        begin
          Result := attrhe^(attrinfo);
          if not Assigned(Result) then
            Result := init_subsys_attr_error( soself, name);
        end
      else
        Result := init_subsys_attr_error( soself, name);
    end;
end;

function socls_SubSys_OW_MethodCall( callinfo: PMethodCallInfo ): PSOInstance;
{MethodCall dispatcher}
var methhe: PSSDMethEntry;
    attrinfo: TAttributeInfo;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_subsys_ow_class);{$ENDIF}
      methhe := PSSDMethEntry(PSO_SubsystemOW(soself)^.ssdisp.FDispTab.MethProc.LookupByHash(hashk,name));
      if Assigned(methhe) then
        begin
          check_so_maxargs(argnum);
          Result := methhe^(callinfo);
          if not Assigned(Result) then
            Result := init_subsys_meth_error( soself, name );
        end
      else
        begin
          {directly check getmember/setmember and dispatch, this allows overriding the
           getmember/setmember with MethodCall}
          if (hashk = DEFAULT_METHOD_GetMember_Hash) and
             (ucfs_compare(name,ci_us(ci_dm_getmember,false))=0) and
             (argnum = 1) and
             (soargs^[0]^.IsType(so_string_class)) then
            begin
              attrinfo.name := so_string_get_ucfs(soargs^[0],false);
              attrinfo.soself := soself;
              attrinfo.setter := nil;
              Result := SubSys_OW_Attr_Dispatch(@attrinfo);
            end
          else if (hashk = DEFAULT_METHOD_SetMember_Hash) and
                  (ucfs_compare(name,ci_us(ci_dm_setmember,false))=0) and
                  (argnum = 2) and
                  (soargs^[0]^.IsType(so_string_class)) then
            begin
              attrinfo.name := so_string_get_ucfs(soargs^[0],false);
              attrinfo.soself := soself;
              attrinfo.setter := soargs^[1];
              Result := SubSys_OW_Attr_Dispatch(@attrinfo);
            end
          else
            Result := init_subsys_meth_error(soself,name);
        end;
    end;
end;

function socls_SubSys_Ow_TypeQuery(soself: PSOInstance): String;
begin
  {$IFDEF SELFCHECK}SelfCheck(soself,so_subsys_ow_class);{$ENDIF}
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

var
  TMethodTrie_System: THashTrie;

procedure so_system_addmethod( const mname: String; methh: TSOMethodHandler );
begin
  if TMethodTrie_System.Add(UpCase(mname),methh) <> nil then
    put_internalerror(12012206);
end;

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

procedure socls_System_PostConstructor(instance: PSOInstance);
begin
  PSO_System(instance)^.subsysteme.Init(0,2);
end;

function psse_disposer( unused: PUCFS32String; data, unused2: Pointer ): Boolean;
begin
  Dispose(PSubsystemEntry(data));
  Result := true;
end;

procedure socls_System_PreDestructor(instance: PSOInstance);
begin
  PSO_System(instance)^.subsysteme.ForEach(@psse_disposer,nil);
  PSO_System(instance)^.subsysteme.Done;
end;

function sys_collenum( unused: PUCFS32String; data, xdata: Pointer ): Boolean;
begin
  if Assigned(PSubsystemEntry(data)^.ssinstance) then
    TGarbageCollectorTracer(xdata)(PSubsystemEntry(data)^.ssinstance);
  Result := true;
end;

procedure socls_System_GCEnumerator(instance: PSOInstance; tracer: TGarbageCollectorTracer);
begin
  PSO_System(instance)^.subsysteme.ForEach(@sys_collenum,tracer);
end;

function socls_System_TypeQuery( soself: PSOInstance ): String;
begin
  {$IFDEF SELFCHECK}SelfCheck(soself,so_system_class);{$ENDIF}
  Result := C_SOTYPE_SYSTEM_NAME;
end;

function socls_System_MethodCall( callinfo: PMethodCallInfo ): PSOInstance;
{generic, lookup method and call}
var m: TSOMethodHandler;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_system_class);{$ENDIF}
      m := TSOMethodHandler( TMethodTrie_System.LookupByHash(hashk,name) );
      if Assigned(m) then
        Result := m( callinfo )
      else
        Result := nil;
    end;
end;

{System::GetMember(string) -> subsystem}
function _System_GetMember_(callinfo: PMethodCallInfo): PSOInstance;
var ssentry: PSubsystemEntry;
    f: PSO_SubsystemFW;
    o: PSO_SubsystemOW;
    subsysname: PUCFS32String;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_system_class);{$ENDIF}
      if (argnum = 1) and
         (soargs^[0]^.IsType(so_string_class)) then
        begin
          subsysname := so_string_get_ucfs(soargs^[0],false);
        end
      else
        Exit(nil);
      ssentry := PSubsystemEntry(PSO_System(soself)^.subsysteme.Lookup(subsysname));
    end;

  if Assigned(ssentry) then
    begin
      if not Assigned(ssentry^.ssinstance) then
        begin
          {load}
          if ssentry^.oopwrapper then
            begin
              o := PSO_SubsystemOW(socore.InitInstance(so_subsys_ow_class));
              o^.typename := ucfs_to_utf8string(subsysname);
              o^.ssdisp := nil;
              try
                o^.ssdisp := ssentry^.dispcls.Create;
              except
                on EInternalException do
                  raise;
                on e:Exception do
                  begin
                    put_debug(C_SOTYPE_SUBSYSTEM_NAME+' '+ucfs_to_utf8string(subsysname)+' Load Exception: ' + e.Message);
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
                        put_debug(C_SOTYPE_SUBSYSTEM_NAME+' '+ucfs_to_utf8string(subsysname)+' Load Exception: ' + e.Message);
                        o := nil;
                      end;
                  end;
                end;
              if Assigned(o) then
                ssentry^.ssinstance := o
              else
                put_critical(C_SOTYPE_SUBSYSTEM_NAME+' '+ucfs_to_utf8string(subsysname)+' Startup failed');
            end
          else
            begin
              f := PSO_SubsystemFW(socore.InitInstance(so_subsys_fw_class));
              f^.typename := ucfs_to_utf8string(subsysname);
              f^.disptab.AttrProc.Init(0,2);
              f^.disptab.MethProc.Init(0,2);
              try
                ssentry^.load( f^.disptab, f^.data );
              except
                on EInternalException do
                  raise;
                on e:Exception do
                  begin
                    put_debug(C_SOTYPE_SUBSYSTEM_NAME+' '+ucfs_to_utf8string(subsysname)+' Load Exception: ' + e.Message);
                    f := nil;
                  end;
              end;
              if Assigned(f) then
                ssentry^.ssinstance := f
              else
                put_critical(C_SOTYPE_SUBSYSTEM_NAME+' '+ucfs_to_utf8string(subsysname)+' Startup failed');
              f^.unload := ssentry^.unload;
            end;
        end;
      Result := ssentry^.ssinstance;
      Result^.IncRef; // for passing
    end
  else
    Result := so_error_init('No such '+C_SOTYPE_SUBSYSTEM_NAME+': '+ucfs_to_utf8string(subsysname));
end;

{System::SetMember(string,value) -> value (only used for freeable subsystems)}
function _System_SetMember_(callinfo:PMethodCallInfo): PSOInstance;
var ssentry: PSubsystemEntry;
    i: PSOInstance;
    subsysname: PUCFS32String;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_system_class);{$ENDIF}
      if (argnum = 2) and
         (soargs^[0]^.IsType(so_string_class)) then
        begin
          subsysname := so_string_get_ucfs(soargs^[0],false);
        end
      else
        Exit(nil);
      ssentry := PSubsystemEntry(PSO_System(soself)^.subsysteme.Lookup(subsysname));
    end;
  if callinfo^.soargs^[1]^.IsType(so_none_class) then
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
                      Result := so_error_init(C_SOTYPE_SUBSYSTEM_NAME+' '+ucfs_to_utf8string(subsysname)+' is still in use');
                    end;
                end
              else
                Result := so_error_init(C_SOTYPE_SUBSYSTEM_NAME+' '+ucfs_to_utf8string(subsysname)+' does not allow unloading');
            end;
        end
      else
        Result := so_error_init('No such '+C_SOTYPE_SUBSYSTEM_NAME+': '+ucfs_to_utf8string(subsysname));
    end
  else
    Result := init_invargtype_error(callinfo^.soself,callinfo^.soargs^[1],1,callinfo^.name);
end;

(*******************************************************************************
  SYSTEM Methods
 ******************************************************************************)

{SYSTEM::Instances()}
function _System_GCInstances_(callinfo:PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_system_class);{$ENDIF}
      if argnum = 0 then
        Result := so_integer_init( TracedInstances )
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{SYSTEM::GCollect()}
function _System_GC_(callinfo:PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_system_class);{$ENDIF}
      if argnum = 0 then
        begin
          GarbageCollect;
          Result := so_nil;
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{SYSTEM::Dummy()}
function _System_DummyObject_(callinfo:PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_system_class);{$ENDIF}
      if argnum = 0 then
        Result := so_dummy_instance
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{SYSTEM::TypeOf(<any>)}
function _System_TypeOf_(callinfo:PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_system_class);{$ENDIF}
      if argnum = 1 then
        Result := so_string_init_utf8(soargs^[0]^.GetTypeCls^.TypeQuery(soargs^[0]))
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{SYSTEM::Marker(str,str) -> nil (dynamic marker switching)}
function _System_Marker_(callinfo:PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_system_class);{$ENDIF}
      if argnum = 2 then
        begin
          if soargs^[0]^.IsType(so_string_class) then
            begin
              if soargs^[1]^.IsType(so_string_class) then
                begin
                  cscan.cscan_setcodemarker(so_string_get_ucfs(soargs^[0],false),so_string_get_ucfs(soargs^[1],false));
                  Result := so_nil;
                end
              else
                Result := init_invargtype_error(soself,soargs^[1],2,name);
            end
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{SYSTEM::ECCMarker(str,str) -> nil (dynamic ecc comment marker switching)}
function _System_ECCMarker_(callinfo:PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_system_class);{$ENDIF}
      if argnum = 2 then
        begin
          if soargs^[0]^.IsType(so_string_class) then
            begin
              if soargs^[1]^.IsType(so_string_class) then
                begin
                  cscan.cscan_seteccmarker(so_string_get_ucfs(soargs^[0],false),so_string_get_ucfs(soargs^[1],false));
                  Result := so_nil;
                end
              else
                Result := init_invargtype_error(soself,soargs^[1],2,name);
            end
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{SYSTEM::LNMarker(str) -> nil (dynamic ln comment marker switching)}
function _System_LNMarker_(callinfo:PMethodCallInfo): PSOInstance;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_system_class);{$ENDIF}
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_string_class) then
            begin
              cscan.cscan_setlcmarker(so_string_get_ucfs(soargs^[0],false));
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

