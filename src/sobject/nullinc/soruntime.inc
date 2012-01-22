{   Include for sonull's Runtime Types

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
  RTENV
 ******************************************************************************)

type
  {rt env}
  PSO_RuntimeEnv = ^TSO_RT_ENV;
  TSO_RT_ENV = TSO_Dict;

procedure socls_RT_ENV_GCEnumerator(instance: PSOInstance; tracer: TGarbageCollectorTracer);
begin
  PSO_RuntimeEnv(instance)^.dict.ForEach(@dict_collenum,tracer);
end;

procedure socls_RT_ENV_PostConstructor(instance: PSOInstance);
begin
  PSO_RuntimeEnv(instance)^.dict.Init(CL_LinearRegrow_Medium);
end;

procedure socls_RT_ENV_PreDestructor(instance: PSOInstance);
begin
  PSO_RuntimeEnv(instance)^.dict.Done;
end;

function socls_RT_ENV_TypeQuery(soself: PSOInstance): String;
begin
  SelfCheck(soself,so_rtenv_class);
  Result := C_SOTYPE_RTENV;
end;

(*******************************************************************************
  RTENV Helpers
 ******************************************************************************)

function so_rtenv_set_member( e: PSOInstance; const s: string; i: PSOInstance; noretref: Boolean ): PSOInstance;
var oldval: PSOInstance;
begin
  SelfCheck(e,so_rtenv_class);
  ASSERT(i<>nil);
  ASSERT(s=UpCase(s));
  if i <> so_nil then
    begin
      check_so_maxcollection(PSO_RuntimeEnv(e)^.dict.GetCount+1);
      i^.IncRef; // << entry in table
      if not noretref then
        i^.IncRef; // << incref again for result
      oldval := PSOInstance(PSO_RuntimeEnv(e)^.dict.Add(s,i))
    end
  else
    oldval := PSOInstance(PSO_RuntimeEnv(e)^.dict.Delete(s));
  if Assigned(oldval) then
    oldval^.DecRef;
  Result := i;
end;

function so_rtenv_get_member( e: PSOInstance; const s: string ): PSOInstance;
begin
  SelfCheck(e,so_rtenv_class);
  Result := PSO_RuntimeEnv(e)^.dict.Lookup(s);
  if Assigned(Result) then
    Result^.IncRef
  else
    Result := so_nil;
end;

(*******************************************************************************
  RTSTACK
 ******************************************************************************)

type
  {rt stack}
  PSO_RuntimeStack = ^TSO_RT_STACK;
  TSO_RT_STACK = object(TSOInstance)
    stack: array of PSOInstance;
  end;

procedure socls_RT_STACK_GCEnumerator(instance: PSOInstance; tracer: TGarbageCollectorTracer);
var i: MachineInt;
begin
  for i := 0 to High(PSO_RuntimeStack(instance)^.stack) do
    if Assigned(PSO_RuntimeStack(instance)^.stack[i]) then
      tracer(PSO_RuntimeStack(instance)^.stack[i]);
end;

procedure socls_RT_STACK_PostConstructor(instance: PSOInstance);
begin
  SetLength(PSO_RuntimeStack(instance)^.stack,0);
end;

procedure socls_RT_STACK_PreDestructor(instance: PSOInstance);
begin
  SetLength(PSO_RuntimeStack(instance)^.stack,0);
end;

function socls_RT_STACK_TypeQuery(soself: PSOInstance): String;
begin
  Result := C_SOTYPE_RTSTACK;
end;

(*******************************************************************************
  RTSTACK Helpers
 ******************************************************************************)

{$WARNING replace ptrint with machineint}
procedure so_rtstack_setlength( s: PSOInstance; len: PtrInt );
var oldl: PtrInt;
begin
   oldl := Length(PSO_RuntimeStack(s)^.stack);
   if oldl > len then
     begin
       {shrink, explicit decref stuff}
       for oldl := oldl-1 downto len do
         if Assigned(PSO_RuntimeStack(s)^.stack[oldl]) then
           PSO_RuntimeStack(s)^.stack[oldl]^.DecRef;
       SetLength(PSO_RuntimeStack(s)^.stack,len);
     end
   else if oldl < len then
     begin
       {grow and trash}
       SetLength(PSO_RuntimeStack(s)^.stack,len);
       FillByte(PSO_RuntimeStack(s)^.stack[oldl],SizeOf(PSOInstance)*(len-(oldl+1)),0);
     end;
end;

function so_rtstack_getlength( s: PSOInstance ): PtrInt;
begin
  Result := Length(PSO_RuntimeStack(s)^.stack);
end;

procedure so_rtstack_set( s, i: PSOInstance; idx: PtrInt );
var oldval: PSOInstance;
begin
  if Assigned(PSO_RuntimeStack(s)^.stack[idx]) then
    begin
      oldval := PSO_RuntimeStack(s)^.stack[idx];
      PSO_RuntimeStack(s)^.stack[idx] := i;
      oldval^.DecRef;
    end
  else
    PSO_RuntimeStack(s)^.stack[idx] := i;
end;

function so_rtstack_get( s: PSOInstance; idx: PtrInt ): PSOInstance;
begin
  if Assigned(PSO_RuntimeStack(s)^.stack[idx]) then
    Result := PSO_RuntimeStack(s)^.stack[idx]
  else
    Result := so_nil;
end;

function so_rtstack_getargf(s: PSOInstance; idx: PtrInt): PSOMethodVarArgs;
begin
  Result := @PSO_RuntimeStack(s)^.stack[idx];
end;

procedure so_rtstack_move( s: PSOInstance; sidx, didx: PtrInt );
var oldval,cpval: PSOInstance;
begin
  if sidx <> didx then
    begin
      oldval := PSO_RuntimeStack(s)^.stack[didx];
      cpval := PSO_RuntimeStack(s)^.stack[sidx];
      PSO_RuntimeStack(s)^.stack[didx] := cpval;
      if Assigned(cpval) then
        cpval^.IncRef;
      if Assigned(oldval) then
        oldval^.DecRef;
    end;
end;

procedure so_rtstack_switch(s: PSOInstance; sidx, didx: PtrInt);
var tmpval: PSOInstance;
begin
  if sidx <> didx then
    begin
      tmpval := PSO_RuntimeStack(s)^.stack[didx];
      PSO_RuntimeStack(s)^.stack[didx] := PSO_RuntimeStack(s)^.stack[sidx];
      PSO_RuntimeStack(s)^.stack[sidx] := tmpval;
    end;
end;

