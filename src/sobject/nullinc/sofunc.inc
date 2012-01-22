{   Include for sonull's Function Type

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
  FUNC -- very special Object
    it has only one Method "^Call", returns itself for MethodOverride
    such that the VM sets up the Function as Caller Object for
    plain function calls.
 ******************************************************************************)

type
  TSO_FunctionType = record
    code: PCodeReference;
    ip: Integer;         // addr of first Instruction in coderef
    argf: Integer;       // fixed args (without default expr/vararg)
    argn: Integer;       // complete nr args
    slotn: Integer;      // Function Stack Frame size (argn+local vars)
    canvarargs: Boolean; // function was declared vararg (every arg_i with i >= argn is passed in a list)
  end;

  PSO_Function = ^TSO_Function;
  TSO_Function = object(TSOInstance)
    fun: TSO_FunctionType;
  end;

function socls_Function_TypeQuery(soself: PSOInstance): String;
begin
  SelfCheck(soself,so_function_class);
  Result := C_SOTYPE_FUNCTION_NAME;
end;

function socls_Function_MethodCall( hashk: MachineWord; const name: String; soself: PSOInstance; args: PSOMethodVarArgs; argnum: VMInt): PSOInstance;
var vararglist: PSOInstance;
begin
  SelfCheck(soself,so_function_class);
  Result := nil;
  if (hashk = DEFAULT_METHOD_DirectCall_Hash) and
     (name = DEFAULT_METHOD_DirectCall) then
    begin
      {function setup}
      // check nrargs, pack varargs ..
      if (PSO_Function(soself)^.fun.argf <= argnum) and
         ((PSO_Function(soself)^.fun.argn >= argnum) or
          PSO_Function(soself)^.fun.canvarargs) then
        begin
          // push code addr
          templatestack_push(PSO_Function(soself)^.fun.code,
                             PSO_Function(soself)^.fun.ip,true);

          // fix framepointer
          templatestack_fix_framepointer(
            runtimestack_relptr(argnum)-1);

          if PSO_Function(soself)^.fun.canvarargs then
            begin
              if argnum >= PSO_Function(soself)^.fun.argn then
                begin
                  {pack varargs into a list}
                  vararglist := so_list_append(so_list_init,
                                  runtimestack_getargf(argnum-PSO_Function(soself)^.fun.argn),
                                  argnum-(PSO_Function(soself)^.fun.argn-1));
                  vararglist^.DecRef; // decrefcount, since it is 2 (1=init, 2 for append)
                  // the list is dangling, so dont try calling gc on pop
                  runtimestack_pop(argnum-(PSO_Function(soself)^.fun.argn-1));
                  runtimestack_push(vararglist);
                  {fix argnum}
                  argnum := PSO_Function(soself)^.fun.argn;
                end;
            end;
          {fix ip (jump offset), correct position in functions jumptable}
          template_ip_setrel(PSO_Function(soself)^.fun.argn - argnum,true);
        end
      else
        begin
          Result := so_error_init('Invalid Nr of Arguments for Call to '+so_function_codelnstr(soself));
          Exit(Result);
        end;

      // grow frame
      runtimestack_framegrow(PSO_Function(soself)^.fun.slotn-argnum);
    end;
end;

function socls_Function_MethodCallOverride( hashk: MachineWord; const name: String;soself: PSOInstance): PSOInstance;
begin
  SelfCheck(soself,so_function_class);
  if (hashk = DEFAULT_METHOD_DirectCall_Hash) and
     (name = DEFAULT_METHOD_DirectCall) then
    begin
      {return self on call}
      Result := soself;
      Result^.IncRef;
    end
  else
    Result := nil;
end;

function socls_Function_Compare(soself, rightop: PSOInstance): TSOCompareResult;
begin
  if soself = rightop then
    Result := socmp_isEqual
  else
    begin
       if (PSO_Function(soself)^.fun.code = PSO_Function(rightop)^.fun.code) and
          (PSO_Function(soself)^.fun.ip = PSO_Function(rightop)^.fun.ip) then
         Result := socmp_isEqual
       else
         Result := socmp_NotComparable;
    end;
end;

(*******************************************************************************
  FUNC Methods/Attr/Helpers
 ******************************************************************************)

function so_function_init(coderef: PCodeReference; argf, argn, slotn,
  ip: Integer; canvararg: Boolean): PSOInstance;
begin
  Result := InitInstance(so_function_class);
  PSO_Function(Result)^.fun.code := coderef;
  PSO_Function(Result)^.fun.ip := ip;
  PSO_Function(Result)^.fun.argf := argf;
  PSO_Function(Result)^.fun.argn := argn;
  PSO_Function(Result)^.fun.slotn := slotn;
  PSO_Function(Result)^.fun.canvarargs := canvararg;
end;

function so_function_codelnstr( f: PSOInstance ): String;
var resstring: String;
begin
  SelfCheck(f,so_function_class);
  ASSERT( Assigned(PSO_Function(f)^.fun.code) );
  ASSERT( Assigned(PSO_Function(f)^.fun.code^.pbcode) );
  ASSERT( Length(PSO_Function(f)^.fun.code^.pbcode^.image) > (PSO_Function(f)^.fun.ip-1) );
  ASSERT( PSO_Function(f)^.fun.ip >= 0 );

  // report error on the decl jump, since it has correct lineinfo
  WriteStr(resstring,
    'Function(...)@',
    PSO_Function(f)^.fun.code^.shortname,'.l',
    PSO_Function(f)^.fun.code^.pbcode^.image[PSO_Function(f)^.fun.ip-1].GetLine,'.c',
    PSO_Function(f)^.fun.code^.pbcode^.image[PSO_Function(f)^.fun.ip-1].GetColumn);
  Result := resstring;
end;
