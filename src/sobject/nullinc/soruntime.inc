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
  PSO_RuntimeEnv = ^TSO_RuntimeEnv;
  TSO_RuntimeEnv = TSO_Dict;

  {Class for <RTEnv> Type}
  TSOTypeRTEnv = class(TSOType)
    protected
      class function InstanceSize: MachineInt; override;
      class function IsMortal: Boolean; override;
      class procedure GCEnumerator(instance: PSOInstance; tracer: TGarbageCollectorTracer); override;
      class procedure PostConstructor(instance: PSOInstance); override;
      class procedure PreDestructor(instance: PSOInstance); override;
    public
      class function MethodCall(soself: PSOInstance; const name: String; args: PSOMethodVarArgs; argnum: VMInt): PSOInstance; override;
      class function SetMember(soself, value: PSOInstance; const name: String): PSOInstance;
      class function GetMember(soself: PSOInstance; const name: String): PSOInstance;
      class function TypeQuery(soself: PSOInstance): String; override;
  end;

{ TSOTypeRTEnv }

class procedure TSOTypeRTEnv.GCEnumerator(instance: PSOInstance;
  tracer: TGarbageCollectorTracer);
begin
  PSO_RuntimeEnv(instance)^.dict.ForEach(@dict_collenum,tracer);
end;

class procedure TSOTypeRTEnv.PostConstructor(instance: PSOInstance);
begin
  PSO_RuntimeEnv(instance)^.dict.Init(CL_LinearRegrow_Medium);
end;

class procedure TSOTypeRTEnv.PreDestructor(instance: PSOInstance);
begin
  PSO_RuntimeEnv(instance)^.dict.Done;
end;

class function TSOTypeRTEnv.MethodCall(soself: PSOInstance; const name: String;
  args: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
begin
  put_internalerror(1); // no
  Result := nil;
end;

class function TSOTypeRTEnv.SetMember(soself, value: PSOInstance; const name: String
  ): PSOInstance;
{lookupadd, incref, return}
var oldval: PSOInstance;
begin
  SelfCheck(soself,self);
  if value <> so_nil then
    begin
      check_so_maxcollection(PSO_RuntimeEnv(soself)^.dict.GetCount+1);
      value^.IncRef; // << entry in table
      value^.IncRef; // << incref again for result
      oldval := PSOInstance(PSO_RuntimeEnv(soself)^.dict.Add(name,value))
    end
  else
    oldval := PSOInstance(PSO_RuntimeEnv(soself)^.dict.Delete(name));
  if Assigned(oldval) then
    oldval^.DecRef;
  Result := value;
end;

class function TSOTypeRTEnv.GetMember(soself: PSOInstance; const name: String
): PSOInstance;
{lookup, incref, return}
begin
  SelfCheck(soself,self);
  Result := PSO_RuntimeEnv(soself)^.dict.Lookup(name);
  if Assigned(Result) then
    Result^.IncRef
  else
    Result := so_nil;
end;

class function TSOTypeRTEnv.TypeQuery(soself: PSOInstance): String;
begin
  SelfCheck(soself,self);
  Result := C_SOTYPE_RTENV;
end;

class function TSOTypeRTEnv.InstanceSize: MachineInt;
begin
  Result := SizeOf(TSO_RuntimeEnv);
end;

class function TSOTypeRTEnv.IsMortal: Boolean;
begin
  Result := false;
end;

(*******************************************************************************
  RTENV Helpers
 ******************************************************************************)

function so_rtenv_set_member( e: PSOInstance; const s: string; i: PSOInstance ): PSOInstance;
begin
  Result := TSOTypeRTEnv.SetMember(e,i,s);
end;

function so_rtenv_get_member( e: PSOInstance; const s: string ): PSOInstance;
begin
  Result := TSOTypeRTEnv.GetMember(e,s);
end;

(*******************************************************************************
  RTSTACK
 ******************************************************************************)

type
  {rt stack}
  PSO_RuntimeStack = ^TSO_RuntimeStack;
  TSO_RuntimeStack = object(TSOInstance)
    stack: array of PSOInstance;
  end;

  {Class for <RTStack> Type}
  TSOTypeRTStack = class(TSOType)
    protected
      class function InstanceSize: MachineInt; override;
      class function IsMortal: Boolean; override;
      class procedure GCEnumerator(instance: PSOInstance; tracer: TGarbageCollectorTracer); override;
      class procedure PostConstructor(instance: PSOInstance); override;
      class procedure PreDestructor(instance: PSOInstance); override;
    public
      class function MethodCall(soself: PSOInstance; const name: String; args: PSOMethodVarArgs; argnum: VMInt): PSOInstance; override;
      class function TypeQuery(soself: PSOInstance): String; override;
  end;

{ TSOTypeRTStack }

class function TSOTypeRTStack.InstanceSize: MachineInt;
begin
  Result := SizeOf(TSO_RuntimeStack);
end;

class function TSOTypeRTStack.IsMortal: Boolean;
begin
  Result := false;
end;

class procedure TSOTypeRTStack.GCEnumerator(instance: PSOInstance;
  tracer: TGarbageCollectorTracer);
var i: MachineInt;
begin
  for i := 0 to High(PSO_RuntimeStack(instance)^.stack) do
    if Assigned(PSO_RuntimeStack(instance)^.stack[i]) then
      tracer(PSO_RuntimeStack(instance)^.stack[i]);
end;

class procedure TSOTypeRTStack.PostConstructor(instance: PSOInstance);
begin
  SetLength(PSO_RuntimeStack(instance)^.stack,0);
end;

class procedure TSOTypeRTStack.PreDestructor(instance: PSOInstance);
begin
  SetLength(PSO_RuntimeStack(instance)^.stack,0);
end;

class function TSOTypeRTStack.MethodCall(soself: PSOInstance;
  const name: String; args: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
begin
  put_internalerror(1); //no
  Result := nil;
end;

class function TSOTypeRTStack.TypeQuery(soself: PSOInstance): String;
begin
  Result := C_SOTYPE_RTSTACK;
end;

(*******************************************************************************
  RTSTACK Helpers
 ******************************************************************************)

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

