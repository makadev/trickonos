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
{ $X-} // see fpc bug 0017598, but good for debugging

interface

uses
  SysUtils, commontl, eomsg, socore, coreobj, corealg, ccache, ffpa, ucfs, cons;


const
  {special method}
  C_STDCALLM_LIST__APPEND            = 'APPEND';

  {iterator interface}
  C_STDCALLM_SEQ_ITERATOR            = 'ITERATOR';
  C_STDCALLM_ITER__NEXT              = 'NEXT';

  {typical methods}
  C_STDCALLM_LENGTH                  = 'LENGTH'; // ordered collection length
  C_STDCALLM_COUNT                   = 'COUNT';  // unordered collection count

  C_STDMEMBR_LISTITER__CURRENT       = 'CURRENT'; // list iterator only
  C_STDMEMBR_DICTITER__CURRENTKEY    = 'CURRENTKEY'; // dict iterator only
  C_STDMEMBR_DICTITER__CURRENTVALUE  = 'CURRENTVALUE'; // dict iterator only

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
  C_SOTYPE_CLASS_BASENAME        = 'ClassType_';
  C_SOTYPE_INSTANCE_BASENAME     = 'Class_';

{generic}
function so_any_flatstring( soinstance: PSOInstance ): String; inline;
function so_type_name( soinstance: PSOInstance ): String; inline;

{error}
function so_error_init( const s: String ): PSOInstance; inline;
function so_error_get( soerror: PSOInstance ): String; inline;

function init_operation_error(soself: PSOInstance; const opername: String): PSOInstance; deprecated;
function init_invargtype_error(soself, arg: PSOInstance; argnum: MachineInt; const opname: String): PSOInstance; deprecated;
function init_invargvalue_error(soself, arg: PSOInstance; argnum: MachineInt; const opname: String): PSOInstance; deprecated;
function init_range_error(soself, arg: PSOInstance; argnum: MachineInt; const opname: String): PSOInstance; deprecated;
function init_invargnum_error(soself: PSOInstance; argnum: MachineInt; const opname: String): PSOInstance; deprecated;
function init_lockmod(soself: PSOInstance; const opname: String): PSOInstance; deprecated;

function init_operation_error(soself: PSOInstance; opername: PUCFS32String): PSOInstance;
function init_invargtype_error(soself, arg: PSOInstance; argnum: MachineInt; opname: PUCFS32String): PSOInstance;
function init_invargvalue_error(soself, arg: PSOInstance; argnum: MachineInt; opname: PUCFS32String): PSOInstance;
function init_range_error(soself, arg: PSOInstance; argnum: MachineInt; opname: PUCFS32String): PSOInstance;
function init_invargnum_error(soself: PSOInstance; argnum: MachineInt; opname: PUCFS32String): PSOInstance;
function init_lockmod(soself: PSOInstance; opname: PUCFS32String): PSOInstance;

{string}
function so_string_init_a7( const s: String ): PSOInstance; inline;
function so_string_init_utf8( const s: String ): PSOInstance; inline;
function so_string_init_char( uc: TUCFS32Char ): PSOInstance; inline;
function so_string_init_ucfs( ps: PUCFS32String ): PSOInstance; inline;
function so_string_init_empty: PSOInstance; inline;
function so_string_length( sostring: PSOInstance ): VMInt; inline;
function so_string_get_ucfs( sostring: PSOInstance ): PUCFS32String; inline;
function so_string_get_a7( sostring: PSOInstance; dereplace: Boolean = false ): String; inline;
function so_string_get_utf8( sostring: PSOInstance ): String; inline;
procedure so_string_set_ucfs( sostring: PSOInstance; newstr: PUCFS32String; releaseold: Boolean = true ); inline;
procedure so_string_addmethod( const mname: String; methh: TSOMethodHandler ); inline;

{integer}
function so_integer_init( const i: VMInt ): PSOInstance; inline;
function so_integer_init_tfm( num: PTFM_Integer ): PSOInstance; inline;
function so_integer_fits( soint: PSOInstance ): Boolean; inline;
function so_integer_get( soint: PSOInstance; usecut: Boolean ): VMInt; inline;
function so_integer_string( soint: PSOInstance ): String; inline;


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
function so_function_codelnstr( f: PSOInstance ): String; inline;

{rt env}
function so_rtenv_set_member( e: PSOInstance; const s: String; i: PSOInstance; noretref: Boolean ): PSOInstance; inline;
function so_rtenv_get_member( e: PSOInstance; const s: String ): PSOInstance; inline;


{rt stack}
procedure so_rtstack_setlength( s: PSOInstance; len: PtrInt ); inline;
function so_rtstack_getlength( s: PSOInstance ): PtrInt; inline;
procedure so_rtstack_set( s, i: PSOInstance; idx: PtrInt ); inline;
function so_rtstack_get( s: PSOInstance; idx: PtrInt ): PSOInstance; inline;
function so_rtstack_getargf( s: PSOInstance; idx: PtrInt ): PSOMethodVarArgs; inline;
procedure so_rtstack_move( s: PSOInstance; sidx, didx: PtrInt ); inline;
procedure so_rtstack_switch( s: PSOInstance; sidx, didx: PtrInt ); inline;

{instance}
{DOC>> creates an instance of a class cls. DOES NOT CALL THE CONSTRUCTOR,
       constructor call is handled by the VM
       (when methodcall override returnes an object).}
function so_instance_create( cls: PSOInstance ): PSOInstance; inline;
{DOC>> Does NOT incref!}
function so_instance_getclass( instance: PSOInstance ): PSOInstance; inline;

{class}
function so_class_create( const name: String; coderef: PCodeReference; ip: VMInt ): PSOInstance;
procedure so_class_methodreg( cls, funobj: PSOInstance; const mname: String ); inline;
{DOC>> increfs if constructor object exists}
function so_class_getconstructor( cls: PSOInstance ): PSOInstance; inline;

(******************************************************************************
 CLASS POINTER (Mortal Base Types)
 ******************************************************************************)

function so_string_class: PSOTypeCls; inline;
function so_integer_class: PSOTypeCls; inline;
function so_dict_class: PSOTypeCls; inline;
function so_list_class: PSOTypeCls; inline;
function so_function_class: PSOTypeCls; inline;
function so_error_class: PSOTypeCls; inline;
function so_io_class: PSOTypeCls; inline;
function so_subsys_fw_class: PSOTypeCls; inline;
function so_subsys_ow_class: PSOTypeCls; inline;
function so_class_class: PSOTypeCls; inline;
function so_instance_class: PSOTypeCls; inline;

(******************************************************************************
 CLASS POINTER (Immortal Base Types)
 ******************************************************************************)

function so_boolean_class: PSOTypeCls; inline;
function so_none_class: PSOTypeCls; inline;
function so_rtenv_class: PSOTypeCls; inline;
function so_rtstack_class: PSOTypeCls; inline;
function so_system_class: PSOTypeCls; inline;
function so_dummy_class: PSOTypeCls; inline;

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

  TSSDMethodHandlerM = function( callinfo: PMethodCallInfo ): PSOInstance of object;
  TSSDAttributHandlerM = function( attrinfo: PAttributeInfo ): PSOInstance of object;

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
  TSSDMethodHandlerP = function( callinfo: PMethodCallInfo ): PSOInstance;
  TSSDAttributHandlerP = function( attrinfo: PAttributeInfo ): PSOInstance;
  TSystemLoadCallBack = procedure( var disptab: TDispTables; out data: Pointer );
  TSystemUnloadCallBack = procedure( var data: Pointer );

{access to data pointer for ssd methodhandler/attributehandler}
function subsysfw_get_data( soself: PSOInstance ): Pointer; inline;
procedure subsysfw_set_data( soself: PSOInstance; data: Pointer ); inline;

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
      function DispatchMethodCall( callinfo: PMethodCallInfo ): PSOInstance;
      function DispatchAttr( attrinfo: PAttributeInfo ): PSOInstance;
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

      //DOC>> for own error messages, typename is assigned by Wrapper AFTER CONSTRUCTION ^^
      function TypeName: String;
    public

      {DOC>> called before instancing}
      constructor Create; virtual;

      {DOC>> called before destruction by the garbage collector.}
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

var
   DEFAULT_METHOD_SetMember_Hash: MachineWord;
   DEFAULT_METHOD_GetMember_Hash: MachineWord;
   DEFAULT_METHOD_SetIndex_Hash: MachineWord;
   DEFAULT_METHOD_GetIndex_Hash: MachineWord;
   DEFAULT_METHOD_BinOpAdd_Hash: MachineWord;
   DEFAULT_METHOD_BinOpSub_Hash: MachineWord;
   DEFAULT_METHOD_BinOpMul_Hash: MachineWord;
   DEFAULT_METHOD_BinOpDiv_Hash: MachineWord;
   DEFAULT_METHOD_BinOpMod_Hash: MachineWord;
   DEFAULT_METHOD_BinOpShl_Hash: MachineWord;
   DEFAULT_METHOD_BinOpShr_Hash: MachineWord;
   DEFAULT_METHOD_BinOpRol_Hash: MachineWord;
   DEFAULT_METHOD_BinOpRor_Hash: MachineWord;
   DEFAULT_METHOD_BinOpAnd_Hash: MachineWord;
   DEFAULT_METHOD_BinOpOr_Hash: MachineWord;
   DEFAULT_METHOD_BinOpXor_Hash: MachineWord;
   DEFAULT_METHOD_UnOpNeg_Hash: MachineWord;
   DEFAULT_METHOD_UnOpAbs_Hash: MachineWord;
   DEFAULT_METHOD_UnOpNot_Hash: MachineWord;
   DEFAULT_METHOD_DirectCall_Hash: MachineWord;
   SPECIAL_METHOD_Create_Hash: MachineWord;

procedure RegisterLevel0Types;
function DefaultCompare(soself, rightop: PSOInstance): TSOCompareResult;

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
  if TypeRegister.Add(Upcase(name),nil) <> nil then
    put_internalerror(2011122080); // dup type
end;

{$ENDIF}

function DefaultCompare(soself, rightop: PSOInstance): TSOCompareResult;
begin
  if soself = rightop then
    Result := socmp_isEqual
  else
    Result := socmp_NotComparable;
end;

function hashtrie_gcenum( unused: PUCFS32String; data, xdata: Pointer ): Boolean;
{default enumerator for hastrie holding PSOInstances}
begin
  TGarbageCollectorTracer(xdata)(PSOInstance(data));
  Result := true;
end;

(******************************************************************************
 Errors
 ******************************************************************************)

function init_object_attr_error(soself: PSOInstance; const opername: String): PSOInstance;
begin
  Result := so_error_init('Invalid/Unsupported/Wrong us of Object Attribute '+
    soself^.GetTypeCls^.TypeQuery(soself)+'.'+opername);
end;

function init_object_meth_error(soself: PSOInstance; const opername: String): PSOInstance;
begin
  Result := so_error_init('Invalid/Unsupported/Wrong us of Object Method '+
    soself^.GetTypeCls^.TypeQuery(soself)+'.'+opername);
end;

function init_subsys_attr_error(soself: PSOInstance; const opername: String): PSOInstance;
begin
  Result := so_error_init('Invalid/Unsupported/Wrong us of '+C_SOTYPE_SUBSYSTEM_NAME+' Attribute '+
    soself^.GetTypeCls^.TypeQuery(soself)+'.'+opername);
end;

function init_subsys_meth_error(soself: PSOInstance; const opername: String): PSOInstance;
begin
  Result := so_error_init('Invalid/Unsupported/Wrong us of '+C_SOTYPE_SUBSYSTEM_NAME+' Method '+
    soself^.GetTypeCls^.TypeQuery(soself)+'.'+opername);
end;

function init_operation_error(soself: PSOInstance; const opername: String): PSOInstance;
begin
  Result := so_error_init('Invalid/Unsupported Operation '+
    soself^.GetTypeCls^.TypeQuery(soself)+'.'+opername);
end;

function init_invargtype_error(soself, arg: PSOInstance; argnum: MachineInt; const opname: String): PSOInstance;
begin
  Result := so_error_init('Invalid Type ('+arg^.GetTypeCls^.TypeQuery(arg)+') for Argument ('+
    IntToStr(argnum)+') in '+soself^.GetTypeCls^.TypeQuery(soself)+'.'+opname+'(...)');
end;

function init_invargvalue_error(soself, arg: PSOInstance; argnum: MachineInt; const opname: String): PSOInstance;
begin
  Result := so_error_init('Invalid Value ('+arg^.GetTypeCls^.TypeQuery(arg)+') for Argument ('+
    IntToStr(argnum)+') in '+soself^.GetTypeCls^.TypeQuery(soself)+'.'+opname+'(...)');
end;

function init_range_error(soself, arg: PSOInstance; argnum: MachineInt; const opname: String): PSOInstance;
begin
  Result := so_error_init('Value Out of Range ('+arg^.GetTypeCls^.TypeQuery(arg)+') for Argument ('+
    IntToStr(argnum)+') in '+soself^.GetTypeCls^.TypeQuery(soself)+'.'+opname+'(...)');
end;

function init_invargnum_error(soself: PSOInstance; argnum: MachineInt; const opname: String): PSOInstance;
begin
  Result := so_error_init('Invalid Number of Arguments ('+IntToStr(argnum)+')  in '+
    soself^.GetTypeCls^.TypeQuery(soself)+'.'+opname+'(...)');
end;

function init_lockmod(soself: PSOInstance; const opname: String): PSOInstance;
begin
  Result  := so_error_init('Operation '+opname+' on '+
    IntToStr(soself^.GetLocks)+'-Locked Object ('+soself^.GetTypeCls^.TypeQuery(soself)+')');
end;




function init_object_attr_error(soself: PSOInstance; opername: PUCFS32String): PSOInstance;
begin
  Result := so_error_init('Invalid/Unsupported/Wrong us of Object Attribute '+
    soself^.GetTypeCls^.TypeQuery(soself)+'.'+
    ucfs_to_utf8string(opername));
end;

function init_object_meth_error(soself: PSOInstance; opername: PUCFS32String): PSOInstance;
begin
  Result := so_error_init('Invalid/Unsupported/Wrong us of Object Method '+
    soself^.GetTypeCls^.TypeQuery(soself)+'.'+
    ucfs_to_utf8string(opername));
end;

function init_subsys_attr_error(soself: PSOInstance; opername: PUCFS32String): PSOInstance;
begin
  Result := so_error_init('Invalid/Unsupported/Wrong us of '+C_SOTYPE_SUBSYSTEM_NAME+' Attribute '+
    soself^.GetTypeCls^.TypeQuery(soself)+'.'+
    ucfs_to_utf8string(opername));
end;

function init_subsys_meth_error(soself: PSOInstance; opername: PUCFS32String): PSOInstance;
begin
  Result := so_error_init('Invalid/Unsupported/Wrong us of '+C_SOTYPE_SUBSYSTEM_NAME+' Method '+
    soself^.GetTypeCls^.TypeQuery(soself)+'.'+
    ucfs_to_utf8string(opername));
end;

function init_operation_error(soself: PSOInstance; opername: PUCFS32String): PSOInstance;
begin
  Result := so_error_init('Invalid/Unsupported Operation '+
    soself^.GetTypeCls^.TypeQuery(soself)+'.'+
    ucfs_to_utf8string(opername));
end;

function init_invargtype_error(soself, arg: PSOInstance; argnum: MachineInt; opname: PUCFS32String): PSOInstance;
begin
  Result := so_error_init('Invalid Type ('+arg^.GetTypeCls^.TypeQuery(arg)+') for Argument ('+
    IntToStr(argnum)+') in '+soself^.GetTypeCls^.TypeQuery(soself)+'.'+
    ucfs_to_utf8string(opname)+'(...)');
end;

function init_invargvalue_error(soself, arg: PSOInstance; argnum: MachineInt; opname: PUCFS32String): PSOInstance;
begin
  Result := so_error_init('Invalid Value ('+arg^.GetTypeCls^.TypeQuery(arg)+') for Argument ('+
    IntToStr(argnum)+') in '+soself^.GetTypeCls^.TypeQuery(soself)+'.'+
    ucfs_to_utf8string(opname)+'(...)');
end;

function init_range_error(soself, arg: PSOInstance; argnum: MachineInt; opname: PUCFS32String): PSOInstance;
begin
  Result := so_error_init('Value Out of Range ('+arg^.GetTypeCls^.TypeQuery(arg)+') for Argument ('+
    IntToStr(argnum)+') in '+soself^.GetTypeCls^.TypeQuery(soself)+'.'+
    ucfs_to_utf8string(opname)+'(...)');
end;

function init_invargnum_error(soself: PSOInstance; argnum: MachineInt; opname: PUCFS32String): PSOInstance;
begin
  Result := so_error_init('Invalid Number of Arguments ('+IntToStr(argnum)+')  in '+
    soself^.GetTypeCls^.TypeQuery(soself)+'.'+
    ucfs_to_utf8string(opname)+'(...)');
end;

function init_lockmod(soself: PSOInstance; opname: PUCFS32String): PSOInstance;
begin
  Result  := so_error_init('Operation '+ucfs_to_utf8string(opname)+' on '+
    IntToStr(soself^.GetLocks)+'-Locked Object ('+soself^.GetTypeCls^.TypeQuery(soself)+')');
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

function pssdae_disposer( unused: PUCFS32String; data, unused2: Pointer ): Boolean;
begin
  Dispose(PSSDAttrEntry(data));
  Result := true;
end;

function pssdme_disposer( unused: PUCFS32String; data, unused2: Pointer ): Boolean;
begin
  Dispose(PSSDMethEntry(data));
  Result := true;
end;

(******************************************************************************
 Internal Method Register (for TSOTypeBase decending Types)
 ******************************************************************************)

{Dummy Type, TSOType Dec., Immortal, 1 Instance (so_dummy_instance)}
{$I nullinc/sodummyo.inc}

{Internal Object Wrapper Type,
 Forwards calls to Internal Object Instance}
{$I nullinc/soiow.inc}

{Error Type, TSOType Dec.
 special since it is VM (Stack) side checked.
 dont use this type directly or write Error instances into
 any environment since only the Stack knows about errors..}
{$I nullinc/soerror.inc}

{"None" Type, Immortal, 1 Instance (so_nil)
 most basic of these types.}
{$I nullinc/sonone.inc}

{Boolean Type, Immortal, 2 Instances (so_true/so_false)}
{$I nullinc/soboolean.inc}

{String Type}
{$I nullinc/sostring.inc}

{Integer Type}
{$I nullinc/sointeger.inc}

{List (duplicate linked) Type}
{$I nullinc/solist.inc}

{Dictionary (Hash Trie) Type}
{$I nullinc/sodict.inc}

{Function Type,
 Special since it knows only the <Method DEFAULT_METHOD_DirectCall>.}
{$I nullinc/sofunc.inc}

{Runtime Types (Runtime Environment/Runtime Stack), Immortal
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

{Class Type,
 Create/Call method overrides are for instancing}
{$I nullinc/soclass.inc}

(******************************************************************************
 CLASS POINTER & CLASS REGISTER
 ******************************************************************************)

 {$I nulltypes.inc}

function so_none_class: PSOTypeCls; begin Result := @TSOClsNone; end;
function so_boolean_class: PSOTypeCls; begin Result := @TSOClsBoolean; end;
function so_error_class: PSOTypeCls; begin Result := @TSOClsError; end;
function so_string_class: PSOTypeCls; begin Result := @TSOClsString; end;
function so_integer_class: PSOTypeCls; begin Result := @TSOClsInteger; end;
function so_list_class: PSOTypeCls; begin Result := @TSOClsList; end;
function so_function_class: PSOTypeCls; begin Result := @TSOClsFunction; end;
function so_dict_class: PSOTypeCls; begin Result := @TSOClsDict; end;
function so_rtenv_class: PSOTypeCls; begin Result := @TSOClsRT_ENV; end;
function so_rtstack_class: PSOTypeCls; begin Result := @TSOClsRT_STACK; end;
function so_dummy_class: PSOTypeCls; begin Result := @TSOClsDummyObject; end;
function so_system_class: PSOTypeCls; begin Result := @TSOClsSystem; end;
function so_io_class: PSOTypeCls; begin Result := @TSOClsIO; end;
function so_subsys_fw_class: PSOTypeCls; begin Result := @TSOClsSubSys_FW; end;
function so_subsys_ow_class: PSOTypeCls; begin Result := @TSOClsSubSys_OW; end;
function so_class_class: PSOTypeCls; begin Result := @TSOClsClass; end;
function so_instance_class: PSOTypeCls; begin Result := @TSOClsClsInstance; end;


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
  internal_register_type(C_SOTYPE_CLASS_BASENAME);
  internal_register_type(C_SOTYPE_INSTANCE_BASENAME);
{$ENDIF}

  so_nil_instance := InitInstance(so_none_class);
  so_true_instance := InitInstance(so_boolean_class);
  so_false_instance := InitInstance(so_boolean_class);
  so_system_instance := InitInstance(so_system_class);
  so_dummy_instance := InitInstance(so_dummy_class);

  {reg methods & attributes}
  so_boolean_addmethod(ci_str(ci_dm_and),@_Boolean_And_);
  so_boolean_addmethod(ci_str(ci_dm_or),@_Boolean_Or_);
  so_boolean_addmethod(ci_str(ci_dm_xor),@_Boolean_Xor_);
  so_boolean_addmethod(ci_str(ci_dm_not),@_Boolean_Not_);

  so_string_addmethod(ci_str(ci_dm_add),@_String_Add_);
  so_string_addmethod(ci_str(ci_dm_getindex),@_String_GetIndex_);
  so_string_addmethod(ci_str(ci_dm_setindex),@_String_SetIndex_);
  so_string_addmethod(C_STDCALLM_LENGTH,@_String_Length_);

  so_integer_addmethod(ci_str(ci_dm_add),@_Integer_Add_);
  so_integer_addmethod(ci_str(ci_dm_and),@_Integer_And_);
  so_integer_addmethod(ci_str(ci_dm_sub),@_Integer_Sub_);
  so_integer_addmethod(ci_str(ci_dm_or),@_Integer_Or_);
  so_integer_addmethod(ci_str(ci_dm_xor),@_Integer_Xor_);
  so_integer_addmethod(ci_str(ci_dm_mul),@_Integer_Mul_);
  so_integer_addmethod(ci_str(ci_dm_div),@_Integer_Div_);
  so_integer_addmethod(ci_str(ci_dm_mod),@_Integer_Mod_);
  so_integer_addmethod(ci_str(ci_dm_rol),@_Integer_Rol_);
  so_integer_addmethod(ci_str(ci_dm_ror),@_Integer_Ror_);
  so_integer_addmethod(ci_str(ci_dm_shl),@_Integer_Shl_);
  so_integer_addmethod(ci_str(ci_dm_shr),@_Integer_Sar_);
  so_integer_addmethod(ci_str(ci_dm_abs),@_Integer_Abs_);
  so_integer_addmethod(ci_str(ci_dm_not),@_Integer_Not_);
  so_integer_addmethod(ci_str(ci_dm_neg),@_Integer_Neg_);
  so_integer_addmethod('_SHR',@_Integer_Shr_);
  so_integer_addmethod('TOSTR',@_Integer_ToStr_);

  so_list_addmethod(C_STDCALLM_LENGTH,@_List_Length_);
  so_list_addmethod(C_STDCALLM_LIST__APPEND,@_List_Append_);
  so_list_addmethod(C_STDCALLM_SEQ_ITERATOR,@_List_Iterator_);

  so_dict_addmethod(ci_str(ci_dm_getmember),@_Dict_GetMember_);
  so_dict_addmethod(ci_str(ci_dm_getindex),@_Dict_GetMember_);
  so_dict_addmethod(ci_str(ci_dm_setmember),@_Dict_SetMember_);
  so_dict_addmethod(ci_str(ci_dm_setindex),@_Dict_SetMember_);
  so_dict_addmethod(C_STDCALLM_COUNT,@_Dict_Count_);
  so_dict_addmethod('DELETE',@_Dict_Delete_);
  so_dict_addmethod(C_STDCALLM_SEQ_ITERATOR,@_Dict_Iterator_);

  so_system_addmethod(ci_str(ci_dm_getmember),@_System_GetMember_);
  so_system_addmethod(ci_str(ci_dm_setmember),@_System_SetMember_);
  so_system_addmethod('GCOLLECT',@_System_GC_);
  so_system_addmethod('INSTANCES',@_System_GCInstances_);
  so_system_addmethod(C_SOTYPE_DUMMY_NAME,@_System_DummyObject_);
  so_system_addmethod('TYPEOF',@_System_TypeOf_);
  so_system_addmethod('MARKER',@_System_Marker_);
  so_system_addmethod('ECCMARKER',@_System_ECCMarker_);
  so_system_addmethod('LNMARKER',@_System_LNMarker_);

  so_instance_addmethod(ci_str(ci_dm_getmember),@_ClsInstance_GetMember_);
  so_instance_addmethod(ci_str(ci_dm_setmember),@_ClsInstance_SetMember_);
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
      Result := so_integer_string(soinstance);
    end
  else if soinstance^.GetTypeCls = so_string_class then
    begin
      Result := so_string_get_utf8(soinstance);
    end
  else if soinstance^.GetTypeCls = so_function_class then
    begin
      Result := '<Object ' + so_function_codelnstr(soinstance) + '>';
    end
  else
    begin
      {deep or non representable type}
      Result := '<Object '+soinstance^.GetTypeCls^.TypeQuery(soinstance)+' $'+
        IntToHex(MachineWord(soinstance^.GetCollectorEntry),SizeOf(MachineWord)*2)+'>';
    end;
end;

function so_type_name(soinstance: PSOInstance): String;
begin
  Result := soinstance^.GetTypeCls^.TypeQuery(soinstance);
end;

initialization
  DEFAULT_METHOD_SetMember_Hash := mas3hash_sigma(ci_us(ci_dm_setmember,false));
  DEFAULT_METHOD_GetMember_Hash := mas3hash_sigma(ci_us(ci_dm_getmember,false));
  DEFAULT_METHOD_SetIndex_Hash := mas3hash_sigma(ci_us(ci_dm_setindex,false));
  DEFAULT_METHOD_GetIndex_Hash := mas3hash_sigma(ci_us(ci_dm_getindex,false));
  DEFAULT_METHOD_BinOpAdd_Hash := mas3hash_sigma(ci_us(ci_dm_add,false));
  DEFAULT_METHOD_BinOpSub_Hash := mas3hash_sigma(ci_us(ci_dm_sub,false));
  DEFAULT_METHOD_BinOpMul_Hash := mas3hash_sigma(ci_us(ci_dm_mul,false));
  DEFAULT_METHOD_BinOpDiv_Hash := mas3hash_sigma(ci_us(ci_dm_div,false));
  DEFAULT_METHOD_BinOpMod_Hash := mas3hash_sigma(ci_us(ci_dm_mod,false));
  DEFAULT_METHOD_BinOpShl_Hash := mas3hash_sigma(ci_us(ci_dm_shl,false));
  DEFAULT_METHOD_BinOpShr_Hash := mas3hash_sigma(ci_us(ci_dm_shr,false));
  DEFAULT_METHOD_BinOpRol_Hash := mas3hash_sigma(ci_us(ci_dm_rol,false));
  DEFAULT_METHOD_BinOpRor_Hash := mas3hash_sigma(ci_us(ci_dm_ror,false));
  DEFAULT_METHOD_BinOpAnd_Hash := mas3hash_sigma(ci_us(ci_dm_and,false));
  DEFAULT_METHOD_BinOpOr_Hash := mas3hash_sigma(ci_us(ci_dm_or,false));
  DEFAULT_METHOD_BinOpXor_Hash := mas3hash_sigma(ci_us(ci_dm_xor,false));
  DEFAULT_METHOD_UnOpNeg_Hash := mas3hash_sigma(ci_us(ci_dm_neg,false));
  DEFAULT_METHOD_UnOpAbs_Hash := mas3hash_sigma(ci_us(ci_dm_abs,false));
  DEFAULT_METHOD_UnOpNot_Hash := mas3hash_sigma(ci_us(ci_dm_not,false));
  DEFAULT_METHOD_DirectCall_Hash := mas3hash_sigma(ci_us(ci_dm_call,false));
  SPECIAL_METHOD_Create_Hash := mas3hash_sigma(ci_us(ci_dm_create,false));
  TMethodTrie_Boolean.Init(16);
  TMethodTrie_Integer.Init(16);
  TMethodTrie_String.Init(16);
  TMethodTrie_List.Init(16);
  TMethodTrie_Dict.Init(16);
  TMethodTrie_System.Init(16);
  TMethodTrie_Class.Init(16);
  TMethodTrie_InstanceDefaults.Init(16);
{$IFDEF DEBUG}
  TypeRegister.Init(16,32);
{$ENDIF}

finalization
{$IFDEF DEBUG}
  TypeRegister.Done;
{$ENDIF}
  TMethodTrie_Boolean.Done;
  TMethodTrie_Integer.Done;
  TMethodTrie_String.Done;
  TMethodTrie_List.Done;
  TMethodTrie_Dict.Done;
  TMethodTrie_System.Done;
  TMethodTrie_Class.Done;
  TMethodTrie_InstanceDefaults.Done;

end.

