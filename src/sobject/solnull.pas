{   Unit for base 0 (root) Script Objects

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

unit solnull;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, commontl, eomsg, socore, coreobj, ccache;


const
  {special method}
  C_STDCALLM_LIST__APPEND = 'Append';

  {iterator interface}
  C_STDCALLM_SEQ_ITERATOR            = 'Iterator';
  C_STDCALLM_ITER__NEXT              = 'Next';

  C_STDMEMBR_LISTITER__CURRENT       = 'Current'; // list iterator only
  C_STDMEMBR_DICTITER__CURRENTKEY    = 'CurrentKey'; // dict iterator only
  C_STDMEMBR_DICTITER__CURRENTVALUE  = 'CurrentValue'; // dict iterator only

const
  // External visible
  C_SOTYPE_ERROR_NAME            = 'Error';
  C_SOTYPE_NONE_NAME             = 'None';
  C_SOTYPE_BOOLEAN_NAME          = 'Boolean';
  C_SOTYPE_STRING_NAME           = 'String';
  C_SOTYPE_INTEGER_NAME          = 'Integer';
  C_SOTYPE_LIST_NAME             = 'List';
  C_SOTYPE_DICTIONARY_NAME       = 'Dict';
  C_SOTYPE_FUNCTION_NAME         = 'Func';   // <- function not possible, its language keyword

  C_SOTYPE_SYSTEM_NAME           = 'System';
  C_SOTYPE_IO_LISTITER_NAME      = 'ListIterator'; // with IO_ prefix
  C_SOTYPE_IO_DICTITER_NAME      = 'DictIterator'; // with IO_ prefix
  C_SOTYPE_DUMMY_NAME            = 'Dummy';

  // Internal
  C_SOTYPE_IO_NAME               = 'IO';
  C_SOTYPE_SUBSYSTEM_NAME        = 'SubSystem';
  C_SOTYPE_RTENV                 = '@RuntimeEnvironment';
  C_SOTYPE_RTSTACK               = '@RuntimeStack';
  C_SOTYPE_IO_BASENAME           = 'IO_';
  C_SOTYPE_SUBSYS_BASENAME       = 'SubSys_';

{generic}
function so_any_flatstring( soinstance: PSOInstance ): String; inline;
function so_type_name( soinstance: PSOInstance ): String; inline;

{error}
function so_error_init( const s: String ): PSOInstance; inline;
function so_error_get( soerror: PSOInstance ): String; inline;

function init_operation_error(soself: PSOInstance; const opername: String): PSOInstance; inline;
function init_invargtype_error(soself, arg: PSOInstance; argnum: MachineInt; const opname: String): PSOInstance; inline;
function init_invargvalue_error(soself, arg: PSOInstance; argnum: MachineInt; const opname: String): PSOInstance; inline;
function init_range_error(soself, arg: PSOInstance; argnum: MachineInt; const opname: String): PSOInstance; inline;
function init_invargnum_error(soself: PSOInstance; argnum: MachineInt; const opname: String): PSOInstance; inline;
function init_lockmod(soself: PSOInstance; const opname: String): PSOInstance; inline;

{string}
function so_string_init( const s: String ): PSOInstance; inline;
function so_string_get( sostring: PSOInstance ): String; inline;


{integer}
function so_integer_init( const i: VMInt ): PSOInstance; inline;
function so_integer_get( soint: PSOInstance ): VMInt; inline;


{list}
function so_list_init: PSOInstance; inline;
function so_list_append( soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: Integer ): PSOInstance; inline;

function so_list_count( plst: PSOInstance ): VMInt;

{DOC>> Low Level List Access. @br
  iter=nil: get first, iter<>nil: get next,
  returns @false if there is no next or first.
  non locking, no reference counting!}
function so_list_iter_next( plst: PSOInstance; var iter: Pointer ): Boolean;
{DOC>> Low Level List Access. @br
  iter=nil: get last, iter<>nil: get prev,
  returns @false if there is no prev or last.
  non locking, no reference counting!}
function so_list_iter_prev( plst: PSOInstance; var iter: Pointer ): Boolean;
{DOC>> get value from current position (no Reference Counting)}
function so_list_iter_getval( iter: Pointer ): PSOInstance;
{DOC>> set value at current position (no Reference Counting), returns the replaced
       value}
function so_list_iter_setval( iter: Pointer; val: PSOInstance ): PSOInstance;

{dict}
function so_dict_init: PSOInstance; inline;
function so_dict_get_member( d: PSOInstance; const s: String; noincref: Boolean=true ): PSOInstance;
procedure so_dict_set_member( d, value: PSOInstance; const s: String; noincref: Boolean=true );


{function}
function so_function_init( coderef: PCodeReference; argf, argn, slotn, ip: Integer; canvararg: Boolean ): PSOInstance; inline;

{rt env}
function so_rtenv_set_member( e: PSOInstance; const s: String; i: PSOInstance ): PSOInstance; inline;
function so_rtenv_get_member( e: PSOInstance; const s: String ): PSOInstance; inline;


{rt stack}
procedure so_rtstack_setlength( s: PSOInstance; len: PtrInt ); inline;
function so_rtstack_getlength( s: PSOInstance ): PtrInt; inline;
procedure so_rtstack_set( s, i: PSOInstance; idx: PtrInt ); inline;
function so_rtstack_get( s: PSOInstance; idx: PtrInt ): PSOInstance; inline;
function so_rtstack_getargf( s: PSOInstance; idx: PtrInt ): PSOMethodVarArgs; inline;
procedure so_rtstack_move( s: PSOInstance; sidx, didx: PtrInt ); inline;
procedure so_rtstack_switch( s: PSOInstance; sidx, didx: PtrInt ); inline;


(******************************************************************************
 CLASS POINTER (Mortal Base Types)
 ******************************************************************************)

function so_string_class: TSOClassType; inline;
function so_integer_class: TSOClassType; inline;
function so_dict_class: TSOClassType; inline;
function so_list_class: TSOClassType; inline;
function so_function_class: TSOClassType; inline;
function so_error_class: TSOClassType; inline;

(******************************************************************************
 CLASS POINTER (Immortal Base Types)
 ******************************************************************************)

function so_boolean_class: TSOClassType; inline;
function so_none_class: TSOClassType; inline;
function so_rtenv_class: TSOClassType; inline;
function so_rtstack_class: TSOClassType; inline;

(******************************************************************************
 Special (or Unique) Instances (Immortal Class Instances)
 ******************************************************************************)

{none}
function so_nil: PSOInstance; inline;

{boolean}
function so_true: PSOInstance; inline;
function so_false: PSOInstance; inline;

{system}
function so_system: PSOInstance; inline;

(******************************************************************************
 SUBSYSTEM EXTENSION (OOP)
 ******************************************************************************)

type
  TDispTables = record
    AttrProc: THashTrie;
    MethProc: THashTrie;
  end;

  { Subsystem Dispatcher. This (and the subsystem registration)
    can be used to extend "System", which loads subsystems ondemand.

    Subsystems _must not_ hold Instances, since the garbage collector
    does not know about them. (thought, it is possible by using an rtenv/
    rtstack and binding objects)
  }

  TSSDMethodHandlerM = function( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance of object;
  TSSDAttributHandlerM = function( const aname: String; soself: PSOInstance; setter: PSOInstance ): PSOInstance of object;

  TSubSystemDispatcher = class
    private
      FDispTab: TDispTables;
    protected
      procedure RegisterAttribute( const name: String; attrh: TSSDAttributHandlerM );
      procedure RegisterMethod( const name: String; methh: TSSDMethodHandlerM );
      {called by System, ondemand -- setup subsystem object and register Handler}
      procedure OnSubSystemLoad; virtual;
      {called by System, on termination or when unloaded (implies, unloading is supported)
       -- free allocated stuff}
      procedure OnSubSystemUnLoad; virtual;
    public
      {dont use}
      constructor Create; virtual;
      {dont use}
      destructor Destroy; override;
  end;

  TSubSysDispatcherCls = class of TSubSystemDispatcher;

procedure RegisterSubsystemObject( const subsysname: String; subsyso: TSubSysDispatcherCls; canunload: Boolean );

(******************************************************************************
 SUBSYSTEM EXTENSION (PROCS)
 ******************************************************************************)

type
  TSSDMethodHandlerP = function( const mname: String; var data: Pointer; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
  TSSDAttributHandlerP = function( const aname: String; var data: Pointer; soself: PSOInstance; setter: PSOInstance ): PSOInstance;
  TSystemLoadCallBack = procedure( var disptab: TDispTables; out data: Pointer );
  TSystemUnloadCallBack = procedure( var data: Pointer );

{use for subsystem registration}
procedure RegisterSubSystemHandler( const subsysname: String; loader: TSystemLoadCallBack;
                                    unloader: TSystemUnloadCallBack; canunload: Boolean );

{use for attr/meth registration in load callback}
procedure RegisterSubSystemAttribute( var disptab: TDispTables; const name: String; attrh: TSSDAttributHandlerP );
procedure RegisterSubSystemMethod( var disptab: TDispTables; const name: String; methh: TSSDMethodHandlerP );

(******************************************************************************
 INTERNAL OBJECT for Internal Objects that are introduces
 f.e. using a SubSystem. Int.Obj. are self dispatching and may be bound
 to one or more Subsystems such that the Garbage Collector respects use.
 (bound means, the Int.Obj. will reference the Subsystem, but you must
 do own book keeping with respect to the GC/Tracer if its not independent)

 Sample: A Registered Subsystem "SQL" using Method "Connect" which creates
         a Int.Obj., references the SQL Subsystem and uses some stuff from it.
         Now SQL got "Shutdown" which should shutdown all Connections. So
         SQL needs some table or so, which holds all instances.
         But Int.Obj. Connection may be freed due to Ref 0 (Garbage) so in that
         inverse case, Int.Obj. needs to access SQL and f.e. mark itself
         Disconnected etc.
         Just as little hinter, its not that much of a Problem, but a
         basic consideration on development. You cant simply kill the object
         which would rip a whole in the Garbage Collector/Tracer. ^^
         But dead/unusable objects (like a disconnected Connection) are ok..

 Remark: In general, InternalObjects dont need registration, but for debugging
         (name collision) it would be nice to have them added at program
         start ^^, see RegisterInternalObjectName
 ******************************************************************************)

type
  TInternalObject = class
    private
      FDispTab: TDispTables;
      FTypeName: String;
      function DispatchMethodCall( soself: PSOInstance; const name: String; args: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
      function DispatchAttrGet(const aname: String; soself: PSOInstance): PSOInstance;
      function DispatchAttrSet(const aname: String; soself: PSOInstance; setter: PSOInstance): PSOInstance;
    protected
      FReferings: array of PSOInstance;
      {if you need more/own refhandling}
      procedure GCTrace( tracer: TGarbageCollectorTracer ); virtual;

      {Add a Reference to FReferings, this will IncRef.}
      procedure AddReference( refins: PSOInstance );

      {for methods/attr applies same as for subsystem meths/attr.
       1. return nil or error (nil will be replaced by default operation error)
       2. in attribute handler, if setter=nil, then its a getter call
       3. dont check soself against self,
          soself is only provided to startup another internalobject which may
          reference this one.}
      procedure RegisterAttribute( const name: String; attrh: TSSDAttributHandlerM );
      procedure RegisterMethod( const name: String; methh: TSSDMethodHandlerM );

      {other handlers, return nil (or error for more informative messages) on misshandlings
       keep Refs intact. Passing -> incref, creation -> nothing, Collecting/Adding -> incref + AddReference}
      function SetIndex( soself, idx, value: PSOInstance ): PSOInstance; virtual;
      function GetIndex( soself, idx: PSOInstance): PSOInstance; virtual;
      function BinOpAdd( soself, rightop: PSOInstance ): PSOInstance; virtual;
      function BinOpSub( soself, rightop: PSOInstance ): PSOInstance; virtual;
      function BinOpMul( soself, rightop: PSOInstance ): PSOInstance; virtual;
      function BinOpDiv( soself, rightop: PSOInstance ): PSOInstance; virtual;
      function BinOpShl( soself, rightop: PSOInstance ): PSOInstance; virtual;
      function BinOpShr( soself, rightop: PSOInstance ): PSOInstance; virtual;
      function BinOpRol( soself, rightop: PSOInstance ): PSOInstance; virtual;
      function BinOpRor( soself, rightop: PSOInstance ): PSOInstance; virtual;
      function BinOpAnd( soself, rightop: PSOInstance ): PSOInstance; virtual;
      function BinOpOr( soself, rightop: PSOInstance ): PSOInstance; virtual;
      function UnOpNeg( soself: PSOInstance ): PSOInstance; virtual;
      function UnOpAbs( soself: PSOInstance ): PSOInstance; virtual;
      function UnOpNot( soself: PSOInstance ): PSOInstance; virtual;
      function DirectCall( soself: PSOInstance; args: PSOMethodVarArgs; argnum: VMInt ): PSOInstance; virtual;

      //DOC>> for own error messages, typename is assigned by Wrapper AFTER CONSTRUCTION ^^
      function TypeName: String;
    public

      {DOC>> called before instancing}
      constructor Create; virtual;

      {DOC>> called before destruction by the garbage collector.
       This method is called before possibly referenced Objects are Free
       (due to Ref 0 when this object is freed), so it is safe to access the
       referenced objects (but not decref/incref/... only your internal stuff).}
      procedure PreCollect; virtual;

      {DOC>> called before destruction by the garbage collector, this one may be
       called before/after references are killed, so dont assume they are alive.
       it should _only_ free internal datastructures.}
      destructor Destroy; override;
  end;

  TIntObjectClass = class of TInternalObject;


procedure RegisterInternalObjectName( const ioname: String );

{DOC>> this will create an InternalObject Instance Wrapper for iotcls.
       typename (ioname) is prefixed with "InternalObj_"}
function so_create_internalobject( const ioname: String; iotcls: TIntObjectClass ): PSOInstance;
{DOC>> same as so_create_internalobject, but registeres a referenced object after startup,
       mostly for int.objs which need the constructor (those referencing it
       so GC wont kill it as long as the io is alive)
       This routine will IncRef pref!! (better said, it uses AddReference, which Increfs)}
function so_create_internalobject_bound( const ioname: String; iotcls: TIntObjectClass; pref: PSOInstance ): PSOInstance;

{DOC>> return internalobject instance}
function so_internalobject_get( soios: PSOInstance ): TInternalObject;

(******************************************************************************
 stuff..
 ******************************************************************************)

procedure RegisterLevel0Types;

const
  ClassDelimiter = '::';

implementation

uses vmstate, cscan;

{$IFDEF DEBUG}
var
  TypeRegister: THashTrie;

{DOC>> simple registration of typenames to reduce/prevent typename collision for
       internal types}
procedure internal_register_type( const name: String );
begin
  if not TypeRegister.Exists(Upcase(name)) then
    TypeRegister.Add(Upcase(name),nil)
  else
    put_internalerror(2011122080); // dup type
end;

{$ENDIF}

(******************************************************************************
 Errors
 ******************************************************************************)

function init_object_attr_error(soself: PSOInstance; const opername: String): PSOInstance;
begin
  Result := so_error_init('Invalid/Unsupported/Wrong us of Object Attribute '+
    soself^.GetTypeCls.TypeQuery(soself)+'.'+opername);
end;

function init_object_meth_error(soself: PSOInstance; const opername: String): PSOInstance;
begin
  Result := so_error_init('Invalid/Unsupported/Wrong us of Object Method '+
    soself^.GetTypeCls.TypeQuery(soself)+'.'+opername);
end;

function init_subsys_attr_error(soself: PSOInstance; const opername: String): PSOInstance;
begin
  Result := so_error_init('Invalid/Unsupported/Wrong us of '+C_SOTYPE_SUBSYSTEM_NAME+' Attribute '+
    soself^.GetTypeCls.TypeQuery(soself)+'.'+opername);
end;

function init_subsys_meth_error(soself: PSOInstance; const opername: String): PSOInstance;
begin
  Result := so_error_init('Invalid/Unsupported/Wrong us of '+C_SOTYPE_SUBSYSTEM_NAME+' Method '+
    soself^.GetTypeCls.TypeQuery(soself)+'.'+opername);
end;

function init_operation_error(soself: PSOInstance; const opername: String): PSOInstance;
begin
  Result := so_error_init('Invalid/Unsupported Operation '+
    soself^.GetTypeCls.TypeQuery(soself)+'.'+opername);
end;

function init_invargtype_error(soself, arg: PSOInstance; argnum: MachineInt; const opname: String): PSOInstance;
begin
  Result := so_error_init('Invalid Type ('+arg^.GetTypeCls.TypeQuery(arg)+') for Argument ('+
    IntToStr(argnum)+') in '+soself^.GetTypeCls.TypeQuery(soself)+'.'+opname+'(...)');
end;

function init_invargvalue_error(soself, arg: PSOInstance; argnum: MachineInt; const opname: String): PSOInstance;
begin
  Result := so_error_init('Invalid Value ('+arg^.GetTypeCls.TypeQuery(arg)+') for Argument ('+
    IntToStr(argnum)+') in '+soself^.GetTypeCls.TypeQuery(soself)+'.'+opname+'(...)');
end;

function init_range_error(soself, arg: PSOInstance; argnum: MachineInt; const opname: String): PSOInstance;
begin
  Result := so_error_init('Value Out of Range ('+arg^.GetTypeCls.TypeQuery(arg)+') for Argument ('+
    IntToStr(argnum)+') in '+soself^.GetTypeCls.TypeQuery(soself)+'.'+opname+'(...)');
end;

function init_invargnum_error(soself: PSOInstance; argnum: MachineInt; const opname: String): PSOInstance;
begin
  Result := so_error_init('Invalid Number of Arguments ('+IntToStr(argnum)+')  in '+
    soself^.GetTypeCls.TypeQuery(soself)+'.'+opname+'(...)');
end;

function init_lockmod(soself: PSOInstance; const opname: String): PSOInstance;
begin
  Result  := so_error_init('Operation '+opname+' on '+
    IntToStr(soself^.GetLocks)+'-Locked Object ('+soself^.GetTypeCls.TypeQuery(soself)+')');
end;

(******************************************************************************
 Special (or Unique) Instances (Immortal Class Instances)
 ******************************************************************************)

var
  so_nil_instance: PSOInstance;
  so_true_instance: PSOInstance;
  so_false_instance: PSOInstance;
  so_system_instance: PSOInstance;
  so_dummy_instance: PSOInstance;

function so_nil: PSOInstance; begin Result := so_nil_instance; end;
function so_true: PSOInstance; begin Result := so_true_instance; end;
function so_false: PSOInstance; begin Result := so_false_instance; end;
function so_system: PSOInstance; begin Result := so_system_instance; end;

(******************************************************************************
 Stuff for object wrappers
 ******************************************************************************)

type
  PSSDAttrEntry = ^TSSDAttributHandlerM;
  PSSDMethEntry = ^TSSDMethodHandlerM;

function pssdae_disposer( const unused: String; data, unused2: Pointer ): Boolean;
begin
  Dispose(PSSDAttrEntry(data));
  Result := true;
end;

function pssdme_disposer( const unused: String; data, unused2: Pointer ): Boolean;
begin
  Dispose(PSSDMethEntry(data));
  Result := true;
end;

(******************************************************************************
 Internal Method Register (for TSOTypeBase decending Types)
 ******************************************************************************)

var
  btmethod: THashTrie;
  btattrb: THashTrie;

procedure AddInternalMethod( h: TSOMethodHandler; const methname, clsname: String ); inline;
begin
  h := TSOMethodHandler(btmethod.Add(upcase(clsname)+ClassDelimiter+upcase(methname),Pointer(h)));
  if Assigned(h) then
    put_internalerror(2011121800); // duplicate entry
end;

function GetInternalMethod( const methname, clsname: String ): TSOMethodHandler;  inline;
begin
  Result := TSOMethodHandler(btmethod.Lookup(upcase(clsname)+ClassDelimiter+upcase(methname)));
end;

procedure AddInternalAttribute( ah: TSOAttributHandler; const aname, clsname: String ); inline;
begin
  ah := TSOAttributHandler(btmethod.Add(upcase(clsname)+ClassDelimiter+upcase(aname),Pointer(ah)));
  if Assigned(ah) then
    put_internalerror(2011121801); // duplicate entry
end;

function GetInternalAttribute( const aname, clsname: String ): TSOAttributHandler; inline;
begin
  Result := TSOAttributHandler(btmethod.Lookup(upcase(clsname)+ClassDelimiter+upcase(aname)));
end;

{Dummy Type, TSOType Dec., Immortal, 1 Instance (so_dummy_instance), only for
 testing. Propagated with System.DummyObject()
 Prints (debug/info) every operation on itself and returns itself for
 any operation. ^^}
{$I nullinc/sodummyo.inc}

{Operations Type [sotif_baseOperType Interface], TSOOperType Dec.
 Base which returns errors for any operation and calls Internal Method/
 Internal Attribute handler _for methods/attributes not implied by the OperType_
 f.e. "^Add" wont be handled since it is VM side directed to
 TSOTypeBase.BinOpAdd}
{$I nullinc/sotypebase.inc}

{Internal Object Wrapper Type, TSOTypeBase Dec.
 Forwards calls to Internal Object Instance}
{$I nullinc/soiow.inc}

{Error Type, TSOType Dec.
 special since it is VM (Stack) side checked. (checked wether an error
 was pushed onto stuck or not).
 dont use this type directly or write Error instances into
 any environment since only the Stack knows about errors..}
{$I nullinc/soerror.inc}

{"None" Type, TSOTypeBase Dec., Immortal, 1 Instance (so_nil)
 most basic of these types.}
{$I nullinc/sonone.inc}

{Boolean Type, TSOTypeBase Dec., Immortal, 2 Instances (so_true/so_false)}
{$I nullinc/soboolean.inc}

{String Type, TSOTypeBase Dec.}
{$I nullinc/sostring.inc}

{Integer Type, TSOTypeBase Dec.}
{$I nullinc/sointeger.inc}

{List (duplicate linked) Type, TSOTypeBase Dec.}
{$I nullinc/solist.inc}

{Dictionary (Hash Trie) Type, TSOTypeBase Dec.}
{$I nullinc/sodict.inc}

{Function Type, TSOType Dec.
 Special since it knows only the Method "^Call" (DEFAULT_METHOD_DirectCall).
 MethodCallOverride("^Call") returns self,
 MethodCall("^Call" ...) sets up the stack using the Function Infos.}
{$I nullinc/sofunc.inc}

{Runtime Types (Runtime Environment/Runtime Stack), TSOType Dec., Immortal
 Used as Main Programstack/Global Environment.
 Base Anchor Types for the Garbage Collector. vmstate.pas wraps them in
 runtimestack_* and globalenv_*}
{$I nullinc/soruntime.inc}

{System Type, TSOTypeBase Dec. Immortal, 1 Instance (so_system).
 SubsystemFW (Function Wrapper), TSOTypeBase Dec.
 SubsystemOW (Object Wrapper), TSOTypeBase Dec.

 System is registered in GlobalEnv and Loads OnDemand (Attribute Access)
 the registered Subsystem. Subsystem Access (Attr Access/MethodCalls) are
 are propagated to the registered routines or create default errors.
 This allows simple extensions.

 (dont register Internal Methods/Attr for SubsystemFW/OW since they
  have overriden Attr/MethodCall handlers. This would be a waste and source
  for errors like "I've registered ... but it doesn't work" ;])
  Subsystems contain own dispatcher tries which are loaded on-demand instead
  of bloating everything from the very start.}
{$I nullinc/sosystem.inc}

(******************************************************************************
 CLASS POINTER & CLASS REGISTER
 ******************************************************************************)

function so_none_class: TSOClassType; begin Result := TSOTypeNone; end;
function so_boolean_class: TSOClassType; begin Result := TSOTypeBoolean; end;
function so_error_class: TSOClassType; begin Result := TSOTypeError; end;
function so_string_class: TSOClassType; begin Result := TSOTypeString; end;
function so_integer_class: TSOClassType; begin Result := TSOTypeInteger; end;
function so_list_class: TSOClassType; begin Result := TSOTypeList; end;
function so_function_class: TSOClassType; begin Result := TSOTypeFunction; end;
function so_dict_class: TSOClassType; begin Result := TSOTypeDict; end;
function so_rtenv_class: TSOClassType; begin Result := TSOTypeRTEnv; end;
function so_rtstack_class: TSOClassType; begin Result := TSOTypeRTStack; end;

procedure RegisterLevel0Types;
begin
{$IFDEF DEBUG}
  internal_register_type(C_SOTYPE_DUMMY_NAME);
  internal_register_type(C_SOTYPE_ERROR_NAME);
  internal_register_type(C_SOTYPE_NONE_NAME);
  internal_register_type(C_SOTYPE_BOOLEAN_NAME);
  internal_register_type(C_SOTYPE_STRING_NAME);
  internal_register_type(C_SOTYPE_INTEGER_NAME);
  internal_register_type(C_SOTYPE_LIST_NAME);
  internal_register_type(C_SOTYPE_DICTIONARY_NAME);
  internal_register_type(C_SOTYPE_FUNCTION_NAME);
  internal_register_type(C_SOTYPE_RTSTACK);
  internal_register_type(C_SOTYPE_RTENV);
  internal_register_type(C_SOTYPE_IO_NAME);
  internal_register_type(C_SOTYPE_SYSTEM_NAME);
  internal_register_type(C_SOTYPE_SUBSYSTEM_NAME);
  internal_register_type(C_SOTYPE_IO_BASENAME);
  internal_register_type(C_SOTYPE_SUBSYS_BASENAME);
{$ENDIF}

  so_nil_instance := InitInstance(TSOTypeNone);
  so_true_instance := InitInstance(TSOTypeBoolean);
  so_false_instance := InitInstance(TSOTypeBoolean);
  so_system_instance := InitInstance(TSOSystem);
  so_dummy_instance := InitInstance(TSODummyObject);

  {reg methods & attributes}
  AddInternalMethod(@_Int_ToString_,'ToStr',TSOTypeInteger.BaseTypeName);

  AddInternalMethod(@_String_Length_,'Length',TSOTypeString.BaseTypeName);
  AddInternalMethod(@_String_Trim_,'Trim',TSOTypeString.BaseTypeName);
  AddInternalMethod(@_String_TrimLeft_,'TrimLeft',TSOTypeString.BaseTypeName);
  AddInternalMethod(@_String_TrimRight_,'TrimRight',TSOTypeString.BaseTypeName);
  AddInternalMethod(@_String_Split_,'Split',TSOTypeString.BaseTypeName);
  AddInternalMethod(@_String_Join_,'Join',TSOTypeString.BaseTypeName);

  AddInternalMethod(@_List_Length_,'Length',TSOTypeList.BaseTypeName);
  AddInternalMethod(@_List_Append_,C_STDCALLM_LIST__APPEND,TSOTypeList.BaseTypeName);
  AddInternalMethod(@_List_Iterator_,C_STDCALLM_SEQ_ITERATOR,TSOTypeList.BaseTypeName);

  AddInternalMethod(@_Dict_Count_,'Count',TSOTypeDict.BaseTypeName);
  AddInternalMethod(@_Dict_Delete_,'Delete',TSOTypeDict.BaseTypeName);
  AddInternalMethod(@_Dict_Iterator_,C_STDCALLM_SEQ_ITERATOR,TSOTypeDict.BaseTypeName);

  AddInternalMethod(@_System_GC_,'GCollect',TSOSystem.BaseTypeName);
  AddInternalMethod(@_System_GCInstances_,'Instances',TSOSystem.BaseTypeName);
  AddInternalMethod(@_System_DummyObject_,C_SOTYPE_DUMMY_NAME,TSOSystem.BaseTypeName);
  AddInternalMethod(@_System_TypeOf_,'TypeOf',TSOSystem.BaseTypeName);
  AddInternalMethod(@_System_Marker_,'Marker',TSOSystem.BaseTypeName);
  AddInternalMethod(@_System_ECCMarker_,'ECCMarker',TSOSystem.BaseTypeName);
  AddInternalMethod(@_System_LNMarker_,'LNMarker',TSOSystem.BaseTypeName);
end;

(******************************************************************************
 Additional
 ******************************************************************************)

function so_any_flatstring(soinstance: PSOInstance): String;
begin
  Result := '';
  if soinstance = so_nil then
    begin
      Result := 'nil';
    end
  else if soinstance^.GetTypeCls = so_boolean_class then
    begin
      if soinstance = so_true then
        Result := 'true'
      else
        Result := 'false';
    end
  else if soinstance^.GetTypeCls = so_integer_class then
    begin
      Result := IntToStr(so_integer_get(soinstance));
    end
  else if soinstance^.GetTypeCls = so_string_class then
    begin
      Result := so_string_get(soinstance);
    end
  else
    begin
      {deep or non representable type}
      Result := '<Object '+soinstance^.GetTypeCls.TypeQuery(soinstance)+' $'+
        IntToHex(MachineWord(soinstance^.GetCollectorEntry),SizeOf(MachineWord)*2)+'>';
    end;
end;

function so_type_name(soinstance: PSOInstance): String;
begin
  Result := soinstance^.GetTypeCls.TypeQuery(soinstance);
end;

initialization
  btmethod.Init(16,16);
  btattrb.Init(16,16);
{$IFDEF DEBUG}
  TypeRegister.Init(16,32);
{$ENDIF}

finalization
  btmethod.Done;
  btattrb.Done;
{$IFDEF DEBUG}
  TypeRegister.Done;
{$ENDIF}

end.

