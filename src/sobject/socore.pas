{   Unit for Script Object instancing, tracing, garbage collection

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

unit socore;

{$mode objfpc}{$H+}
{$PACKRECORDS 1}

interface

{ $DEFINE REFDEBUG}

uses SysUtils, commontl, eomsg, gens;

type
  TSOCompareResult = (
    socmp_NotComparable = 0,
    socmp_isLess,
    socmp_isEqual,
    socmp_isGreater
  );

  TSOTypeInterface = (
    sotif_baseType = 0,
    sotif_funcsetup,
    sotif_baseOperType
  );

  PSOInstance = ^TSOInstance;

  TGarbageCollectorTracer = procedure( soinstance: PSOInstance );
  TSOMethodVarArgs = array[0..CL_SO_MaxArgs-1] of PSOInstance;
  PSOMethodVarArgs = ^TSOMethodVarArgs;

  {DOC>> Scripting Objects Base Type Interface @br
    Contains Garbage Collector Interfacing/Information Routines,
    A Default MethodCall for any Calling on this Object (builtin call handling!),
    A MethodCallOverride which should be set whenever In Script Functions
    are Returned and a TypeQuery that returns the Type.}
  TSOType = class
    protected
      (* GC & Instancing Routines *)

      {DOC>> Wether this Type is Mortal or not. Used by Garbage Collecto/Tracer, not Mortal Types
             ignore RefCount (allways 1) and are those never freed. They can be killed using
             Instances "CutHighlander", thought there are several Problems involved. Killing an
             Immortal starts up a Sweep/Clean run (same as for other objects which hit Ref0) and
             effectively rips the Object from the System. This will crash the system or lead to
             unexpected behaviour if there are still referencers. Immortals are merely introduced
             for Singletons/Root referencers (like stack/environment), which makes it possible to
             scan for Garbage, starting a Mark/Sweep run from the Immortals.}
      class function IsMortal: Boolean; virtual; abstract;
      {DOC>> This Method must return the actual used (fixed) Typesize including the
             TSOInstance part. The Tracer allocates a Memblock of InstanceSize's size,
             Zeros it out, writes Infos in the TSOInstance Head and thats mostly all.
             Those a Zero size Instance has at least a Size of SizeOf(TSOInstance), which
             is the default Value.}
      class function InstanceSize: MachineInt; virtual;
      {DOC>> This Method is called after Construction (allocation) and should setup
             Internal Datastructures for the Instance.
             This Operation must be Atomic and should not call the Garbage Collector
             since the created instance is probably not bound in this state.}
      class procedure PostConstructor( instance: PSOInstance ); virtual;
      {DOC>> This Method is called before Collection. As counterpart to PostConstructor it should
             Free all _Internal Datastructures_. Dont DecRef, Kill, Access or Create SO instances here since
             the GC is in SweepMode at this state, Creations/another Sweep start will simply
             shutdown the Tracer. All Instances referenced (Enumerated using GCEnumerator)
             will be (or are already) DecRef'ed by the GC/Tracer.}
     class procedure PreDestructor( instance: PSOInstance ); virtual;
     {DOC>> This Method is quite similar to PreDestructor, but it is called when all objects
            are still alive (wheras PreDestructor is called while destroying).
            This allows some special inter object communication cases, where
            an object which is to be freed, needs to access one of its referenced objects.
            In PreDestructor, this referenced object may be already destroyed,
            but not in PreCollect}
      class procedure PreCollect( instance: PSOInstance ); virtual;
      {DOC>> This Method is called whenever the Garbage Collectors Mark/Sweep Algorithm starts or
             when a Sweep/Clean (due to Ref 0 or Kill) is started. It _must_ return
             all SO Instances referenced by instance, otherwise they may be ripped of as Garbage.
             Flat/Leaf Types (non referencing) dont need to do anything here, which is default.}
      class procedure GCEnumerator( instance: PSOInstance; tracer: TGarbageCollectorTracer ); virtual;
    public
    {$WARNING fix against deep recursion - add rec counter + checks or remove deep compare (can be made script side, more safe)}
      {DOC>> Default compare 2 Ops, used for =,<>,<=,>=,<,> calls -> ^Equals, ^NotEqual, ^GreaterLess, ...
             methodcalls (built-in!), this does default reference comparision.}
      class function Compare( soself, rightop: PSOInstance ): TSOCompareResult; virtual;
      {DOC>> Return a Base Interface, which allows the VM to either call by name or if there exists
             a certain interface, directly Call. Dont simply give the wrong Interface since this will
             surely crash the system. Default returns sotif_baseType.}
      class function BaseInterface: TSOTypeInterface; virtual;
      {DOC>> This Method is the default Handler for any Built-In call. The VM will use this for
             virtualy every Operation on this Object, after checking the MethodCall Override.
             (e.g. x+y is issues a x."^Add"(y) methodcall.)
             If there is no MethodCallOverride, this Method MUST return an SO Instance, either
             some created/passed Value or an Error Instance.}
      class function MethodCall( soself: PSOInstance; const name: String; args: PSOMethodVarArgs; argnum: VMInt ): PSOInstance; virtual; abstract;

      {DOC>> Return Function Object (and INCREF), or nil if there is no Method override. This is for In Script
             Overrides, which return a Function Object and those need the Stack Prepared/Setup for a call.
             Default returns nil.}
      class function MethodCallOverride( soself: PSOInstance; const name: String ): PSOInstance; virtual;

      {DOC>> Return a Typename, used for Typechecking}
      class function TypeQuery( soself: PSOInstance ): String; virtual; abstract;
  end;

  TSOClassType = class of TSOType;
  TSOOperClassType = class of TSOOperType;

  {DOC>> Script Object Basic Operable Type.
         All decending Types come along with a Basic Interface for
         Arithmetic/Logic/Member/Index/Call
         Methods which can be utlized by the VM directly instead of Calling by name.
         This reduces Lookup Overhead for the Basic Types.}
  TSOOperType = class(TSOType)
    public
      {DOC>> Return a BaseType Name}
      class function BaseTypeName: String; virtual; abstract;

      {DOC>> returns sotif_baseOperType}
      class function BaseInterface: TSOTypeInterface; override;

      (* Root Methods (directly called by VM) *)

      {DOC>> replaces soself.^SetMember('name',value) methodcall (built-in!)}
      class function SetMember( soself, value: PSOInstance; const name: String ): PSOInstance; virtual; abstract;
      {DOC>> replaces soself.^GetMember('name') methodcall (built-in!)}
      class function GetMember( soself: PSOInstance; const name: String ): PSOInstance; virtual; abstract;

      {DOC>> replaces soself.^SetIndex(idx,value) methodcall (built-in!)}
      class function SetIndex( soself, idx, value: PSOInstance ): PSOInstance; virtual; abstract;
      {DOC>> replaces soself.^GetIndex(idx) methodcall (built-in!)}
      class function GetIndex( soself, idx: PSOInstance): PSOInstance; virtual; abstract;

      {DOC>> replaces soself.^Add(rightop) methodcall (built-in!)}
      class function BinOpAdd( soself, rightop: PSOInstance ): PSOInstance; virtual; abstract;
      {DOC>> replaces soself.^Sub(rightop) methodcall (built-in!)}
      class function BinOpSub( soself, rightop: PSOInstance ): PSOInstance; virtual; abstract;
      {DOC>> replaces soself.^Mul(rightop) methodcall (built-in!)}
      class function BinOpMul( soself, rightop: PSOInstance ): PSOInstance; virtual; abstract;
      {DOC>> replaces soself.^Div(rightop) methodcall (built-in!)}
      class function BinOpDiv( soself, rightop: PSOInstance ): PSOInstance; virtual; abstract;

      {DOC>> replaces soself.^Shl(rightop) methodcall (built-in!)}
      class function BinOpShl( soself, rightop: PSOInstance ): PSOInstance; virtual; abstract;
      {DOC>> replaces soself.^Shr(rightop) methodcall (built-in!)}
      class function BinOpShr( soself, rightop: PSOInstance ): PSOInstance; virtual; abstract;
      {DOC>> replaces soself.^Rol(rightop) methodcall (built-in!)}
      class function BinOpRol( soself, rightop: PSOInstance ): PSOInstance; virtual; abstract;
      {DOC>> replaces soself.^Ror(rightop) methodcall (built-in!)}
      class function BinOpRor( soself, rightop: PSOInstance ): PSOInstance; virtual; abstract;

      {DOC>> replaces soself.^And(rightop) methodcall (built-in!)}
      class function BinOpAnd( soself, rightop: PSOInstance ): PSOInstance; virtual; abstract;
      {DOC>> replaces soself.^Or(rightop) methodcall (built-in!)}
      class function BinOpOr( soself, rightop: PSOInstance ): PSOInstance; virtual; abstract;

      {DOC>> replaces soself.^Neg() methodcall (built-in!)}
      class function UnOpNeg( soself: PSOInstance ): PSOInstance; virtual; abstract;
      {DOC>> replaces soself.^Abs() methodcall (built-in!)}
      class function UnOpAbs( soself: PSOInstance ): PSOInstance; virtual; abstract;
      {DOC>> replaces soself.^Not() methodcall (built-in!)}
      class function UnOpNot( soself: PSOInstance ): PSOInstance; virtual; abstract;
      {DOC>> replaces soself.^Call() methodcall (built-in!)}
      class function DirectCall( soself: PSOInstance; args: PSOMethodVarArgs; argnum: VMInt ): PSOInstance; virtual; abstract;
  end;

(******************************************************************************
 SOBJECT Instance
 ******************************************************************************)

  TSOInstance = packed object
    private
      clsptr: TSOClassType;
      centry: VMWord;
      refcnt: VMInt;
      lockcnt: VMWord;
    public
      function GetTypeCls: TSOClassType; inline;
      function GetOperTypeCls: TSOOperClassType; inline;
      function IsType(other: TSOClassType): Boolean; inline;

      function IsMortal: Boolean; inline;

      function GetLocked: Boolean; inline;
      function GetLocks: VMWord; inline;
      function GetRefs: VMInt; inline;
      function GetCollectorEntry: VMWord; inline;

      procedure IncRef;
      procedure DecRef;

      procedure IncLocks; inline;
      procedure DecLocks; inline;

      procedure CutHighlander;
  end;

  TSOMethodHandler = function( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
  TSOAttributHandler = function( const aname: String; soself, setter: PSOInstance ): PSOInstance;

const
    DEFAULT_METHOD_SetMember = '^SETMEMBER';
    DEFAULT_METHOD_GetMember = '^GETMEMBER';
    DEFAULT_METHOD_SetIndex = '^SETINDEX';
    DEFAULT_METHOD_GetIndex = '^GETINDEX';
    DEFAULT_METHOD_BinOpAdd = '^ADD';
    DEFAULT_METHOD_BinOpSub = '^SUB';
    DEFAULT_METHOD_BinOpMul = '^MUL';
    DEFAULT_METHOD_BinOpDiv = '^DIV';
    DEFAULT_METHOD_BinOpShl = '^SHL';
    DEFAULT_METHOD_BinOpShr = '^SHR';
    DEFAULT_METHOD_BinOpRol = '^ROL';
    DEFAULT_METHOD_BinOpRor = '^ROR';
    DEFAULT_METHOD_BinOpAnd = '^AND';
    DEFAULT_METHOD_BinOpOr = '^OR';
    DEFAULT_METHOD_UnOpNeg = '^NEG';
    DEFAULT_METHOD_UnOpAbs = '^ABS';
    DEFAULT_METHOD_UnOpNot = '^NOT';
    DEFAULT_METHOD_DirectCall = '^CALL';

(******************************************************************************
 GC/Create/Destroy Interface
 ******************************************************************************)

function InitInstance( classptr: TSOClassType ): PSOInstance;
procedure GarbageCollect;
function TracedInstances: VMInt; inline;
procedure SelfCheck(instance: PSOInstance; fromtype: TSOClassType); inline;

(******************************************************************************
 Tracer/Class Register Init&Fin
 ******************************************************************************)

procedure socore_init;
procedure socore_done;

implementation

(******************************************************************************
 Collector
 ******************************************************************************)

type
  TInstanceTable = specialize TQuadAllocList<PSOInstance>;

var
  SOInstanceTable: TInstanceTable;
{$IFDEF REFDEBUG}
  SOTrash: TInstanceTable;
{$ENDIF}
  SweepMode: Boolean;
  SweepPointer: MachineInt;

procedure SwapAndFix( i,j: MachineInt ); inline;
{swap entries in Tracer Table}
var dummy: PSOInstance;
begin
  if i <> j then
    begin
      dummy := SOInstanceTable.Items[i];
      SOInstanceTable.Items[i] := SOInstanceTable.Items[j];
      SOInstanceTable.Items[j] := dummy;
      SOInstanceTable.Items[i]^.centry := i;
      SOInstanceTable.Items[j]^.centry := j;
    end;
end;

procedure RemoveTrace( soinstance: PSOInstance );
{remove instance from tracer table. if its not the last entry in the Tracer
 Table then it will be swapped with another entry such that it is the last.
 Afterwards it is popped from the Tracer Table}
begin
  if soinstance^.centry <> SOInstanceTable.Count-1 then
    SwapAndFix(soinstance^.centry,SOInstanceTable.Count-1);
  SOInstanceTable.Items[SOInstanceTable.Count-1] := nil;
  SOInstanceTable.Pop;
{$IFDEF REFDEBUG}
  FillByte(soinstance^,soinstance^.clsptr.InstanceSize,0);
  soinstance^.refcnt := -1;
  soinstance^.centry := High(VMWord);
  SOTrash.Push(soinstance);
{$ENDIF}
end;

procedure DoneInstance(soinstance: PSOInstance); inline;
{Destroy an Instance, Remove the Trace and free the Instance Memory}
begin
  soinstance^.clsptr.PreDestructor(soinstance);
  RemoveTrace(soinstance);
{$IFNDEF REFDEBUG}
  Freemem(soinstance,soinstance^.clsptr.InstanceSize);
{$ENDIF}
end;

procedure DecRef(soinstance: PSOInstance); forward;

procedure FlatRelease(soinstance: PSOInstance); inline;
{Call DecRef for all Instances contained in collection soinstance}
begin
  soinstance^.clsptr.GCEnumerator(soinstance,@DecRef);
end;

procedure Sweep; inline;
{iterative decref and compact free objects}
var i: MachineInt;
begin
  {iterative decref garbage referenced objects until the garbage set stabilizes
   (sweepointer wont decrement no more)}
  i := SOInstanceTable.Count;
  while SweepPointer < i do
    begin
      Dec(i,1);
      FlatRelease(SOInstanceTable.Items[i]);
    end;
  {calll Pre Destructor now, with all objects still alive (allows some
   internal interchange between objects before destroy)}
  for i := SOInstanceTable.Count-1 downto SweepPointer do
      SOInstanceTable.Items[i]^.clsptr.PreCollect(SOInstanceTable.Items[i]);
  {enough interchange, clean the garbage}
  while SweepPointer < SOInstanceTable.Count do
    DoneInstance(SOInstanceTable.Items[SOInstanceTable.Count-1]); // trace removed in doneinstance
end;

procedure Ref0SweepCheck(soinstance: PSOInstance); inline;
{start ref0 check.
 in sweepmode, this is an instance that was ref0'ed because another instance
 will be release, so swap in above the sweeppointer for combined collection.
 otherwise start sweepmode.}
begin
  if SweepMode then
    begin
      if soinstance^.centry < SweepPointer then
        begin
          Dec(SweepPointer,1);
          SwapAndFix(soinstance^.centry,SweepPointer);
        end;
    end
  else
    begin
      SweepMode := true;
      SweepPointer := SOInstanceTable.Count-1;
      SwapAndFix(soinstance^.centry,SweepPointer);
      Sweep;
      SweepMode := false;
    end;
end;

function InitInstance(classptr: TSOClassType): PSOInstance;
{Create an Instance of specific class.
 Alloc Memory, Setup Tracer Table Entry and Refcount=1,
 Call Constructor if existend}
var isize: MachineInt;
begin
  if SweepMode then
    begin
      {Instance Creation on Destruction}
      put_internalerror(2011122020);
    end;
  isize := classptr.InstanceSize;
  Result := GetMem(isize);
  FillByte(Result^,isize,0);
  Result^.clsptr := classptr;
  Result^.centry := SOInstanceTable.Push(Result);
  Result^.refcnt := 1;
  Result^.lockcnt := 0;
  check_so_maxobjects(Result^.centry+1);
  classptr.PostConstructor(Result);
end;

procedure KillImmortal(soinstance: PSOInstance);
{kill an Immortal}
begin
  if soinstance^.IsMortal then
    put_internalerror(2011121700);

  soinstance^.refcnt := 0;

  if SweepMode then
    begin
      // simply... no (killing immortal from within a destructor of
      // another object... just use mortals for those case,
      // immortals are bare root tables/types, nothing to be killed or
      // removed by other instances)
      put_internalerror(2011121701);
    end;

  SweepMode := true;
  SweepPointer := SOInstanceTable.Count-1;
  SwapAndFix(soinstance^.centry,SweepPointer);
  Sweep;
  SweepMode := false;
end;

procedure IncRef(soinstance: PSOInstance);
{Increment the Reference Count}
begin
  if soinstance^.IsMortal then
    begin
      if soinstance^.refcnt > 0 then
        Inc(soinstance^.refcnt,1)
      else
        put_internalerror(2011121710); // incref on garbage
    end;
end;

procedure DecRef(soinstance: PSOInstance);
{Decrement the Reference Count, on Refcount 0 start a Ref0 Check and
 Release Referenced Instances}
begin
  if soinstance^.IsMortal then
    begin
      if soinstance^.refcnt > 0 then
        begin
          Dec(soinstance^.refcnt,1);
          if soinstance^.refcnt = 0 then
            Ref0SweepCheck(soinstance);
        end
      else
        put_internalerror(2011121711); // decref on garbage
    end;
end;

procedure MarkReachable(soinstance: PSOInstance);
{reorder all reachables below the sweep pointer}
begin
  if soinstance^.centry >= SweepPointer then
    begin
      SwapAndFix(soinstance^.centry,SweepPointer);
      Inc(SweepPointer,1);
    end;
end;

procedure GarbageCollect;
{Breath First Scan for Reachables. Wipe unreachables.}
var i: MachineInt;
begin
  if SweepMode then
    begin
      {Collector called out of Destructor?}
      put_internalerror(2011121702);
    end;
  SweepMode := true;

  SweepPointer := 0;
  {find all immortals}
  for i := 0 to SOInstanceTable.Count-1 do
    begin
      if not SOInstanceTable.Items[i]^.IsMortal then
        begin
          SwapAndFix(i,SweepPointer);
          Inc(SweepPointer,1);
        end;
    end;

  i := 0;
  {scan reachables / mark reachables by adjusting the sweeppointer}
  while (i < SweepPointer) and
        (SweepPointer < SOInstanceTable.Count) do
    begin
      SOInstanceTable.Items[i]^.clsptr.GCEnumerator(SOInstanceTable.Items[i],@MarkReachable);
      Inc(i,1);
    end;
{$ifdef REFDEBUG}
  WriteLn(StdErr,'COLLECTOR SCAN @ INDEX ',SweepPointer);
  for i := SOInstanceTable.Count-1 downto SweepPointer do
    begin
      WriteLn(StdErr,'TRACER.UNREACHABLE ',PSOInstance(SOInstanceTable.Items[i])^.centry,
                     ' - refs ',PSOInstance(SOInstanceTable.Items[i])^.refcnt,
                     ' CLASS OF ',PSOInstance(SOInstanceTable.Items[i])^.clsptr.BaseTypeName);

    end;
{$endif}
   Sweep;
   SweepMode := false;
end;

function TracedInstances: VMInt;
begin
  Result := SOInstanceTable.Count;
end;

procedure socore_init;
begin
  {tracer}
  SOInstanceTable := TInstanceTable.Create(10);
{$IFDEF REFDEBUG}
  SOTrash := TInstanceTable.Create(10);
{$ENDIF}
  SweepMode := false;
  SweepPointer := -1;
end;

procedure socore_done;
begin
  {tracer}
  if SOInstanceTable.Count > 0 then
    begin
      for SweepPointer := SOInstanceTable.Count-1 downto 0 do
        SOInstanceTable.Items[SweepPointer]^.clsptr.PreCollect(SOInstanceTable.Items[SweepPointer]);
      for SweepPointer := SOInstanceTable.Count-1 downto 0 do
        DoneInstance(SOInstanceTable.Items[SweepPointer]);
    end;
  SOInstanceTable.Free;
{$IFDEF REFDEBUG}
  SOTrash.Free;
{$ENDIF}
end;

{ TSOOperType }

class function TSOOperType.BaseInterface: TSOTypeInterface;
begin
  Result := sotif_baseOperType;
end;

{ TSOType }

class function TSOType.InstanceSize: MachineInt;
begin
  Result := SizeOf(TSOInstance);
end;

class procedure TSOType.PostConstructor(instance: PSOInstance);
begin
  // nop
end;

class procedure TSOType.PreDestructor(instance: PSOInstance);
begin
  // nop
end;

class procedure TSOType.PreCollect(instance: PSOInstance);
begin
  // nop
end;

class procedure TSOType.GCEnumerator(instance: PSOInstance;
  tracer: TGarbageCollectorTracer);
begin
  // nop
end;

class function TSOType.Compare(soself, rightop: PSOInstance): TSOCompareResult;
begin
  SelfCheck(soself,self);
  if soself = rightop then
    Result := socmp_isEqual
  else
    Result := socmp_NotComparable;
end;

class function TSOType.BaseInterface: TSOTypeInterface;
begin
  Result := sotif_baseType;
end;

class function TSOType.MethodCallOverride(soself: PSOInstance;
  const name: String): PSOInstance;
begin
  Result := nil;
end;

{ TSOInstance }

function TSOInstance.GetTypeCls: TSOClassType;
begin
  Result := clsptr;
end;

function TSOInstance.GetOperTypeCls: TSOOperClassType;
begin
  if clsptr.BaseInterface = sotif_baseOperType then
    begin
      Result := TSOOperClassType(clsptr);
    end
  else
    begin
      put_internalerror(2011122070);
      Result := nil;
    end;
end;

function TSOInstance.IsType(other: TSOClassType): Boolean;
begin
  Result := clsptr = other;
end;

function TSOInstance.IsMortal: Boolean;
begin
  Result := clsptr.IsMortal;
end;

function TSOInstance.GetLocked: Boolean;
begin
  Result := lockcnt > 0;
end;

function TSOInstance.GetLocks: VMWord;
begin
  Result := lockcnt;
end;

function TSOInstance.GetRefs: VMInt;
begin
  Result := refcnt;
end;

function TSOInstance.GetCollectorEntry: VMWord;
begin
  Result := centry;
end;

procedure TSOInstance.IncRef;
begin
  socore.IncRef(@self);
end;

procedure TSOInstance.DecRef;
begin
  socore.DecRef(@self);
end;

procedure TSOInstance.IncLocks;
begin
  Inc(lockcnt,1);
end;

procedure TSOInstance.DecLocks;
begin
  if lockcnt > 0 then
    Dec(lockcnt,1)
  else
    put_internalerror(2011121730);
end;

procedure TSOInstance.CutHighlander;
begin
  KillImmortal(@self);
end;

procedure SelfCheck(instance: PSOInstance; fromtype: TSOClassType);
begin
  if (not Assigned(instance)) or
     (not instance^.IsType(fromtype)) then
    put_internalerror(2011121777);
end;

end.

