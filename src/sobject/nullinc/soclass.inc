{   Include for sonull's Class and Instance Types

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

(*******************************************************************************
   Class
 ******************************************************************************)

var
  TMethodTrie_Class: THashTrie;

procedure so_class_addmethod( const mname: String; methh: TSOMethodHandler );
begin
  ASSERT( Upcase(mname) <> ci_str(ci_dm_create) );     // reserved for instancing
  ASSERT( Upcase(mname) <> ci_str(ci_dm_call) ); // reserved for instancing
  if TMethodTrie_Class.Add(UpCase(mname),methh) <> nil then
    put_internalerror(12012301);
end;

type
  PSO_Class = ^TSO_Class;
  TSO_Class = object(TSOInstance)
    clsname: String;         // classname for TypeQuery
    coderef: PCodeReference; // for comparision / source info
    methodtab: THashTrie;    // method table for instances
    ip: VMInt;               // for comparision / line info / exact point of decl
  end;

function socls_Class_TypeQuery( soself: PSOInstance ): String;
begin
  {$IFDEF SELFCHECK}SelfCheck(soself,so_class_class);{$ENDIF}
  Result := C_SOTYPE_CLASS_BASENAME + PSO_Class(soself)^.clsname;
end;

procedure socls_Class_PostConstructor( soinstance: PSOInstance );
begin
  PSO_Class(soinstance)^.methodtab.Init(0);
  PSO_Class(soinstance)^.ip := -1;
end;

procedure socls_Class_PreDestructor( soinstance: PSOInstance );
begin
  SetLength(PSO_Class(soinstance)^.clsname,0);
  PSO_Class(soinstance)^.methodtab.Done;
end;

procedure socls_Class_GCEnumerator(instance: PSOInstance; tracer: TGarbageCollectorTracer);
begin
  PSO_Class(instance)^.methodtab.ForEach(@hashtrie_gcenum,tracer);
end;

function socls_Class_MethodCall( callinfo: PMethodCallInfo ): PSOInstance;
{generic, lookup method and call}
var m: TSOMethodHandler;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_class_class);{$ENDIF}
      m := TSOMethodHandler( TMethodTrie_Class.LookupByHash(hashk,name) );
      if Assigned(m) then
        Result := m( callinfo )
      else
        Result := nil;
    end;
end;

function socls_Class_MethodCallOverride( callinfo: PMethodOverrideCallInfo ): PSOInstance;
{same trick as for function objects.
 MethodCallOverride returns an instance, which is checked by the vm.
 The vm sets up the actual constructor call.}
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_class_class);{$ENDIF}
      if ((hashk = SPECIAL_METHOD_Create_Hash) and
          (ucfs_compare(name,ci_us(ci_dm_create,false))=0)) or
         ((hashk = DEFAULT_METHOD_DirectCall_Hash) and
          (ucfs_compare(name,ci_us(ci_dm_call,false))=0)) then
        begin
          Result := so_instance_create(soself);
        end
      else
        Result := nil;
    end;
end;

function socls_Class_Compare( soself, rightop: PSOInstance ): TSOCompareResult;
begin
  {$IFDEF SELFCHECK}SelfCheck(soself,so_class_class);{$ENDIF}
  Result := socmp_NotComparable;
  {compare classes by coderef/ip (name _should_ always be same when ip/coderef are matching)}
  if rightop^.IsType(so_class_class) and
     (PSO_Class(soself)^.coderef = PSO_Class(rightop)^.coderef) and
     (PSO_Class(soself)^.ip = PSO_Class(rightop)^.ip) then
    Result := socmp_isEqual
end;

function so_class_create( const name: String; coderef: PCodeReference; ip: VMInt ): PSOInstance;
begin
  ASSERT( name <> '' );
  ASSERT( Assigned(coderef) );
  ASSERT( (ip>=0) and (ip < Length(coderef^.pbcode^.image)) );
  Result := InitInstance(so_class_class);
  PSO_Class(Result)^.clsname := Upcase(name);
  PSO_Class(Result)^.coderef := coderef;
  PSO_Class(Result)^.ip := ip;
end;

procedure so_class_methodreg(cls, funobj: PSOInstance; mname: PUCFS32String);
var oldmeth: PSOInstance;
begin
  {$IFDEF SELFCHECK}SelfCheck(cls,so_class_class);{$ENDIF}
  {$IFDEF SELFCHECK}SelfCheck(funobj,so_function_class);{$ENDIF}
  ASSERT( ucfs_length(mname) > 0 );
  funobj^.IncRef;
  oldmeth := PSO_Class(cls)^.methodtab.Add(mname,funobj);
  if Assigned(oldmeth) then
    oldmeth^.DecRef;
end;

function so_class_getconstructor(cls: PSOInstance): PSOInstance;
begin
  {$IFDEF SELFCHECK}SelfCheck(cls,so_class_class);{$ENDIF}
  Result := PSO_Class(cls)^.methodtab.LookupByHash(SPECIAL_METHOD_Create_Hash,ci_us(ci_dm_create,false));
  if Assigned(Result) then
    Result^.IncRef;
end;

(*******************************************************************************
   Instance
 ******************************************************************************)

var
  TMethodTrie_InstanceDefaults: THashTrie;

procedure so_instance_addmethod( const mname: String; methh: TSOMethodHandler );
{defaults, since incode overridable}
begin
  if TMethodTrie_InstanceDefaults.Add(UpCase(mname),methh) <> nil then
    put_internalerror(12012302);
end;

type
  PSO_ClsInstance = ^TSO_ClsInstance;
  TSO_ClsInstance = object(TSOInstance)
    cls: PSO_Class;        // class reference
    attrtab: THashTrie;      // object attributes
  end;

function socls_ClsInstance_TypeQuery( soself: PSOInstance ): String;
begin
  {$IFDEF SELFCHECK}SelfCheck(soself,so_instance_class);{$ENDIF}
  Result := C_SOTYPE_INSTANCE_BASENAME + PSO_ClsInstance(soself)^.cls^.clsname;
end;

procedure socls_ClsInstance_PostConstructor( soinstance: PSOInstance );
begin
  PSO_ClsInstance(soinstance)^.attrtab.Init(0);
end;

procedure socls_ClsInstance_PreDestructor( soinstance: PSOInstance );
begin
  PSO_ClsInstance(soinstance)^.attrtab.Done;
end;

procedure socls_ClsInstance_GCEnumerator(instance: PSOInstance; tracer: TGarbageCollectorTracer);
begin
  tracer(PSO_ClsInstance(instance)^.cls);
  PSO_ClsInstance(instance)^.attrtab.ForEach(@hashtrie_gcenum,tracer);
end;

function socls_ClsInstance_MethodCall( callinfo: PMethodCallInfo ): PSOInstance;
{generic, lookup method and call}
var m: TSOMethodHandler;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_instance_class);{$ENDIF}
      m := TSOMethodHandler( TMethodTrie_InstanceDefaults.LookupByHash(hashk,name) );
      if Assigned(m) then
        Result := m( callinfo )
      else
        Result := nil;
    end;
end;

function socls_ClsInstance_MethodCallOverride( callinfo: PMethodOverrideCallInfo ): PSOInstance;
{simple stub, get method from class}
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_instance_class);{$ENDIF}
      Result := PSOInstance( PSO_ClsInstance(soself)^.cls^.methodtab.LookupByHash(hashk,name) );
      if Assigned(Result) then
        Result^.IncRef; // incref, default convention since vmrun will decref after setup
    end;
end;

{<generic Instance>::SetMember(string,value) -> value
  -- same as environmental set, setting so_nil means deletion and NOT setting an attribute nil}
function _ClsInstance_SetMember_(callinfo:PMethodCallInfo): PSOInstance;
{lookupadd, incref, return}
var oldval: PSOInstance;
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_instance_class);{$ENDIF}
      if argnum = 2 then
        begin
          if soargs^[0]^.IsType(so_string_class) then
            begin
              {no lock checks, programmer must be aware}
              check_so_maxcollection(PSO_ClsInstance(soself)^.attrtab.GetCount+1);
              if soargs^[1] <> so_nil then
                begin
                  soargs^[1]^.IncRef; // << entry in table
                  soargs^[1]^.IncRef; // << incref again for result
                  oldval := PSOInstance(PSO_ClsInstance(soself)^.attrtab.Add(so_string_get_ucfs(soargs^[0],false),soargs^[1]));
                end
              else
                oldval := PSOInstance(PSO_ClsInstance(soself)^.attrtab.Delete(so_string_get_ucfs(soargs^[0],false)));
              if Assigned(oldval) then
                oldval^.DecRef;
              Result := soargs^[1];
            end
          else
            Result := init_invargtype_error(soself,soargs^[0],1,name);
        end
      else
        Result := init_invargnum_error(soself,argnum,name);
    end;
end;

{<generic Instance::GetMember(string) -> value}
function _ClsInstance_GetMember_(callinfo:PMethodCallInfo): PSOInstance;
{lookup, incref, return}
begin
  with callinfo^ do
    begin
      {$IFDEF SELFCHECK}SelfCheck(soself,so_instance_class);{$ENDIF}
      if argnum = 1 then
        begin
          if soargs^[0]^.IsType(so_string_class) then
            begin
              Result := PSO_ClsInstance(soself)^.attrtab.Lookup(so_string_get_ucfs(soargs^[0],false));
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

function so_instance_create( cls: PSOInstance ): PSOInstance;
begin
  {$IFDEF SELFCHECK}SelfCheck(cls,so_class_class);{$ENDIF}
  Result := InitInstance(so_instance_class);
  PSO_ClsInstance(Result)^.cls := PSO_Class(cls);
  cls^.IncRef;
end;

function so_instance_getclass(instance: PSOInstance): PSOInstance;
begin
  {$IFDEF SELFCHECK}SelfCheck(instance,so_instance_class);{$ENDIF}
  Result := PSO_ClsInstance(instance)^.cls;
end;


