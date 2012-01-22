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

uses SysUtils, commontl, corealg, eomsg, gens;

type
  TSOCompareResult = (
    socmp_NotComparable = 0,
    socmp_isLess,
    socmp_isEqual,
    socmp_isGreater
  );

  PSOInstance = ^TSOInstance;

  TInstanceProc = procedure( soinstance: PSOInstance );
  TGarbageCollectorTracer = TInstanceProc;
  TSOMethodVarArgs = array[0..CL_SO_MaxArgs-1] of PSOInstance;
  PSOMethodVarArgs = ^TSOMethodVarArgs;

  TGCEnumeratorProc = procedure( instance: PSOInstance; tracer: TGarbageCollectorTracer );
  TInstanceCompareProc = function( soself, rightop: PSOInstance ): TSOCompareResult;
  TInstanceStrProc = function( soself: PSOInstance ): String;

  TSOMethodHandlerOverride = function( hashk: MachineWord; const name: String; soself: PSOInstance ): PSOInstance;
  TSOMethodHandler = function( hashk: MachineWord; const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;
  TSOMethodHandlerPure = function( const mname: String; soself: PSOInstance; soargs: PSOMethodVarArgs; argnum: VMInt ): PSOInstance;

  PSOTypeCls = ^TSOTypeCls;
  {DOC>> Scripting Objects Base Type Interface}
  TSOTypeCls = record
    (* GC & Instancing Information *)

    {DOC>> Wether this Type is Mortal or not. Used by Garbage Collecto/Tracer, not Mortal Types
           ignore RefCount (allways 1) and are those never freed.}
    IsMortal: Boolean;

    {DOC>> Wether to wipe (zero) Instances or not. Wiping introduces overhead but sometimes, f.e.
           for Dynamic Array/String type fields it is needed since fpc generates reference counting
           code which can fail since Getmem doesnt setup fields. (wich creates
           very random faults f.e. on freeing or string operations and so on)
           So, if your field only contains pointers (also objects), static arrays,
           fixed size values (boolean,byte,word,integer,..) which is setup in
           PostConstructor, then wiping is not needed.}
    Wipe: Boolean;

    {DOC>> Actual used (fixed) Instance Size including the TSOInstance part.
           The Tracer allocates a Memblock of InstanceSize's size.
           At least SizeOf(TSOInstance).}
    InstanceSize: MachineInt;

    {DOC>> This Function is called after Construction (allocation) and should setup
           Internal Datastructures for the Instance.
           This Operation must be Atomic and should not call the Garbage Collector
           since the created instance is probably not bound in this state.
           This field may be nil.}
    PostConstructor: TInstanceProc;

    {DOC>> This Function is called before Collection. As counterpart to PostConstructor it should
           Free all _Internal Datastructures_. Dont DecRef, Kill, Access or Create SO instances here since
           the GC is in SweepMode at this state, Creations/another Sweep start will simply
           kill the Tracer. All Instances referenced (Enumerated using GCEnumerator)
           will be (or are already) DecRef'ed by the GC/Tracer.
           This field may be nil.}
    PreDestructor: TInstanceProc;

    {DOC>> This Function is called whenever the Garbage Collectors Mark/Sweep Algorithm starts or
           when a Sweep/Clean (due to Ref 0 or Kill) is started. It _must_ return
           all SO Instances referenced by instance, otherwise they may be ripped of as Garbage.
           Flat/Leaf Types (non referencing) dont need to do anything here, which is default.
           This field may be nil.}
    GCEnumerator: TGCEnumeratorProc;

    (* VM Call Handling *)

    {DOC>> This Function is the default Handler for any Built-In call. The VM will use this for
           virtualy every Operation on this Object, after checking the MethodCall Override.
           (e.g. x+y is issues a x."^Add"(y) methodcall.)
           If there is no MethodCallOverride, this Method MUST return an SO Instance, either
           some created/passed Value or an Error Instance.
           This field may be nil.}
    MethodCall: TSOMethodHandler;

    {DOC>> Return Function Object (and INCREF), or nil if there is no Method override. This is for In Script
           Overrides, which return a Function Object and those need the Stack Prepared/Setup for a call.
           Default returns nil.
           This field may be nil.}
    MethodCallOverride: TSOMethodHandlerOverride;

    {$WARNING fix against deep recursion - add rec counter + checks or remove deep compare (can be made script side, more safe)}
    {DOC>> Default compare 2 Ops, used for =,<>,<=,>=,<,> calls -> ^Equals, ^NotEqual, ^GreaterLess, ...
           methodcalls (built-in!), this does default reference comparision.}
    Compare: TInstanceCompareProc;

    {DOC>> Return a Typename, used for Typechecking}
    TypeQuery: TInstanceStrProc;
  end;

(******************************************************************************
 SOBJECT Instance
 ******************************************************************************)

  TSOInstance = packed object
    private
      clsptr: PSOTypeCls;
      centry: VMWord;
      refcnt: VMInt;
      lockcnt: VMWord;
    public
      function GetTypeCls: PSOTypeCls; inline;
      function IsType(other: PSOTypeCls): Boolean; inline;

      function GetLocked: Boolean; inline;
      function GetLocks: VMWord; inline;
      function GetRefs: VMInt; inline;
      function GetCollectorEntry: VMWord; inline;

      procedure IncRef; inline;
      procedure DecRef; inline;

      procedure IncLocks; inline;
      procedure DecLocks; inline;
  end;

const
    DEFAULT_METHOD_SetMember = 'SETMEMBER';
    DEFAULT_METHOD_GetMember = 'GETMEMBER';
    DEFAULT_METHOD_SetIndex = 'SETINDEX';
    DEFAULT_METHOD_GetIndex = 'GETINDEX';
    DEFAULT_METHOD_BinOpAdd = 'ADD';
    DEFAULT_METHOD_BinOpSub = 'SUB';
    DEFAULT_METHOD_BinOpMul = 'MUL';
    DEFAULT_METHOD_BinOpDiv = 'OP_DIV';
    DEFAULT_METHOD_BinOpMod = 'OP_MOD';
    DEFAULT_METHOD_BinOpShl = 'OP_SHL';
    DEFAULT_METHOD_BinOpShr = 'OP_SHR';
    DEFAULT_METHOD_BinOpRol = 'OP_ROL';
    DEFAULT_METHOD_BinOpRor = 'OP_ROR';
    DEFAULT_METHOD_BinOpAnd = 'OP_AND';
    DEFAULT_METHOD_BinOpOr = 'OP_OR';
    DEFAULT_METHOD_BinOpXor = 'OP_XOR';
    DEFAULT_METHOD_UnOpNeg = 'NEG';
    DEFAULT_METHOD_UnOpAbs = 'ABS';
    DEFAULT_METHOD_UnOpNot = 'OP_NOT';
    DEFAULT_METHOD_DirectCall = 'CALL';

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

(******************************************************************************
 GC/Create/Destroy Interface
 ******************************************************************************)

function InitInstance( classptr: PSOTypeCls ): PSOInstance; inline;
procedure GarbageCollect;
function TracedInstances: VMInt; inline;
procedure SelfCheck(instance: PSOInstance; fromtype: PSOTypeCls); inline;

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

procedure DoneInstance(soinstance: PSOInstance); inline;
{remove instance from tracer table. if its not the last entry in the Tracer
 Table then it will be swapped with another entry such that it is the last.
 Afterwards it is popped from the Tracer Table and Freed}
begin
  if soinstance^.centry <> SOInstanceTable.Count-1 then
    SwapAndFix(soinstance^.centry,SOInstanceTable.Count-1);
  SOInstanceTable.Items[SOInstanceTable.Count-1] := nil;
  SOInstanceTable.Pop;
  Freemem(soinstance,soinstance^.clsptr^.InstanceSize);
end;

procedure DecRefProc(soinstance: PSOInstance);
{decref wrapper}
begin
  soinstance^.DecRef;
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
      if Assigned(SOInstanceTable.Items[i]^.clsptr^.GCEnumerator) then
        SOInstanceTable.Items[i]^.clsptr^.GCEnumerator(SOInstanceTable.Items[i],@DecRefProc);
    end;
  {call Pre Destructor now, with all objects still alive (allows some
   internal interchange between objects before destroy)}
  for i := SOInstanceTable.Count-1 downto SweepPointer do
    begin
      if Assigned(SOInstanceTable.Items[i]^.clsptr^.PreDestructor) then
        SOInstanceTable.Items[i]^.clsptr^.PreDestructor(SOInstanceTable.Items[i]);
    end;
  {clean the garbage}
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

function InitInstance(classptr: PSOTypeCls): PSOInstance;
{Create an Instance of specific class.
 Alloc Memory, Setup Tracer Table Entry and Refcount=1,
 Call Constructor if existend}
var isize: MachineInt;
begin
  ASSERT( not SweepMode ); // Instance Creation on Destruction (destroys trace table sweep order)
  isize := classptr^.InstanceSize;
  Result := GetMem(isize);
  if classptr^.Wipe then
    FillByte(Result^,isize,0);
  Result^.clsptr := classptr;
  Result^.centry := SOInstanceTable.Push(Result);
  Result^.refcnt := 1;
  Result^.lockcnt := 0;
  check_so_maxobjects(Result^.centry+1);
  if Assigned(classptr^.PostConstructor) then
    classptr^.PostConstructor(Result);
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
      if not SOInstanceTable.Items[i]^.clsptr^.IsMortal then
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
      if Assigned(SOInstanceTable.Items[i]^.clsptr^.GCEnumerator) then
        SOInstanceTable.Items[i]^.clsptr^.GCEnumerator(SOInstanceTable.Items[i],@MarkReachable);
      Inc(i,1);
    end;
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
  SOInstanceTable := TInstanceTable.Create(1024);
  SweepMode := false;
  SweepPointer := -1;
end;

procedure socore_done;
begin
  {tracer}
  if SOInstanceTable.Count > 0 then
    begin
      for SweepPointer := SOInstanceTable.Count-1 downto 0 do
        begin
          if Assigned(SOInstanceTable.Items[SweepPointer]^.clsptr^.PreDestructor) then
            SOInstanceTable.Items[SweepPointer]^.clsptr^.PreDestructor(SOInstanceTable.Items[SweepPointer]);
        end;
      for SweepPointer := SOInstanceTable.Count-1 downto 0 do
        DoneInstance(SOInstanceTable.Items[SweepPointer]);
    end;
  SOInstanceTable.Free;
end;

{ TSOInstance }

function TSOInstance.GetTypeCls: PSOTypeCls;
begin
  Result := clsptr;
end;

function TSOInstance.IsType(other: PSOTypeCls): Boolean;
begin
  Result := clsptr = other;
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

procedure IncRef(soinstance: PSOInstance); inline;
begin
end;

procedure TSOInstance.IncRef;
{Increment the Reference Count}
begin
  if clsptr^.IsMortal then
    begin
      ASSERT( refcnt > 0 ); // incref on garbage
      Inc(refcnt,1)
    end;
end;

procedure TSOInstance.DecRef;
{Decrement the Reference Count, on Refcount 0 start a Ref0 Check and
 Release Referenced Instances}
begin
  if clsptr^.IsMortal then
    begin
      ASSERT( refcnt > 0 ); // decref on garbage
      Dec(refcnt,1);
      if refcnt = 0 then
        Ref0SweepCheck(@self);
    end;
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

procedure SelfCheck(instance: PSOInstance; fromtype: PSOTypeCls);
begin
  if (not Assigned(instance)) or
     (not instance^.IsType(fromtype)) then
    put_internalerror(2011121777);
end;

initialization
  DEFAULT_METHOD_SetMember_Hash := mas3hash(DEFAULT_METHOD_SetMember[1],Length(DEFAULT_METHOD_SetMember));
  DEFAULT_METHOD_GetMember_Hash := mas3hash(DEFAULT_METHOD_GetMember[1],Length(DEFAULT_METHOD_GetMember));
  DEFAULT_METHOD_SetIndex_Hash := mas3hash(DEFAULT_METHOD_SetIndex[1],Length(DEFAULT_METHOD_SetIndex));
  DEFAULT_METHOD_GetIndex_Hash := mas3hash(DEFAULT_METHOD_GetIndex[1],Length(DEFAULT_METHOD_GetIndex));
  DEFAULT_METHOD_BinOpAdd_Hash := mas3hash(DEFAULT_METHOD_BinOpAdd[1],Length(DEFAULT_METHOD_BinOpAdd));
  DEFAULT_METHOD_BinOpSub_Hash := mas3hash(DEFAULT_METHOD_BinOpSub[1],Length(DEFAULT_METHOD_BinOpSub));
  DEFAULT_METHOD_BinOpMul_Hash := mas3hash(DEFAULT_METHOD_BinOpMul[1],Length(DEFAULT_METHOD_BinOpMul));
  DEFAULT_METHOD_BinOpDiv_Hash := mas3hash(DEFAULT_METHOD_BinOpDiv[1],Length(DEFAULT_METHOD_BinOpDiv));
  DEFAULT_METHOD_BinOpMod_Hash := mas3hash(DEFAULT_METHOD_BinOpMod[1],Length(DEFAULT_METHOD_BinOpMod));
  DEFAULT_METHOD_BinOpShl_Hash := mas3hash(DEFAULT_METHOD_BinOpShl[1],Length(DEFAULT_METHOD_BinOpShl));
  DEFAULT_METHOD_BinOpShr_Hash := mas3hash(DEFAULT_METHOD_BinOpShr[1],Length(DEFAULT_METHOD_BinOpShr));
  DEFAULT_METHOD_BinOpRol_Hash := mas3hash(DEFAULT_METHOD_BinOpRol[1],Length(DEFAULT_METHOD_BinOpRol));
  DEFAULT_METHOD_BinOpRor_Hash := mas3hash(DEFAULT_METHOD_BinOpRor[1],Length(DEFAULT_METHOD_BinOpRor));
  DEFAULT_METHOD_BinOpAnd_Hash := mas3hash(DEFAULT_METHOD_BinOpAnd[1],Length(DEFAULT_METHOD_BinOpAnd));
  DEFAULT_METHOD_BinOpOr_Hash := mas3hash(DEFAULT_METHOD_BinOpOr[1],Length(DEFAULT_METHOD_BinOpOr));
  DEFAULT_METHOD_BinOpXor_Hash := mas3hash(DEFAULT_METHOD_BinOpXor[1],Length(DEFAULT_METHOD_BinOpXor));
  DEFAULT_METHOD_UnOpNeg_Hash := mas3hash(DEFAULT_METHOD_UnOpNeg[1],Length(DEFAULT_METHOD_UnOpNeg));
  DEFAULT_METHOD_UnOpAbs_Hash := mas3hash(DEFAULT_METHOD_UnOpAbs[1],Length(DEFAULT_METHOD_UnOpAbs));
  DEFAULT_METHOD_UnOpNot_Hash := mas3hash(DEFAULT_METHOD_UnOpNot[1],Length(DEFAULT_METHOD_UnOpNot));
  DEFAULT_METHOD_DirectCall_Hash := mas3hash(DEFAULT_METHOD_DirectCall[1],Length(DEFAULT_METHOD_DirectCall));

end.

