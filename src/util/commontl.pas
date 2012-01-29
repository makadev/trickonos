{   Unit for Common Types - Limits, Restrictions, Checks.

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

{>>DOC
 Unit for Common Types,
 Limits (or Hard restrictions),
 Basic Conversion, Limit Checks and so on.}
unit commontl;

{$mode objfpc}{$H+}
{$MACRO ON}

interface

uses SysUtils;

type
{$IF (SizeOf(PtrInt) = SizeOf(PtrUInt)) and
     (SizeOf(LongInt) = SizeOf(LongWord)) and
     (SizeOf(LongInt) = 4)}
  {$IF (SizeOf(PtrInt) = 4) or (SizeOf(PtrInt) = 8)}
    {$DEFINE MACHINETYPE}
  {$ELSE}
    {$WARNING explicit mw setting, may be less efficient}
    {$UNDEF MACHINETYPE}
  {$ENDIF}
{$ELSE}
  {$FATAL Thats definitly wrong...}
{$ENDIF}

{1. MachineWord/Int be at least 32bit and consistent.. just in case}
{$IFDEF MACHINETYPE}
  MachineWord = PtrUInt;
  MachineInt = PtrInt;
{$ELSE}
  MachineWord = LongWord;
  MachineInt = LongInt;
{$ENDIF}

{2. Virtual Machine Word/Int (internal) is 32bit, allways}
  VMWord = LongWord;
  VMInt = LongInt;

{3. Any File/Streamsize used, for processed (scanned/compiled/loaded into vm)
    files is bound to the VM Limitations, signed 32bit (31bit positive,
    enough for processing MB wise Data, for a single Stream/File).}
  StreamInt = VMInt;

const
   {Thats it, for the Types, lets set some hard Limits and Restrictions for
    Internals. Iternals should check this, such that modified versions may be
    created for Less/More restrictive Systems/Plattforms.
    Furthermore, some of these restrictions should aid in Bugtracking.}

   {further restrict StreamSize, for reduced overflow possibilities}
   CL_MAX_StreamSize = 1073741824; // 1GB

   {upper limit for _tempory_ buffer}
   CL_MAX_TmpBufferSize = 1048576; // 1MB

   {default buffer size for small chunks}
   CL_Default_FileBuffer = 1024; // 1kb

   {Stricter limits for Bytecode (per file)}
   CL_BC_MaxStab = 1048576; {Stab Entries - 1m entries}
   CL_BC_MaxCode = 1048576; {Code Entries - 1m entries}
   CL_BC_MaxSize = 33554432; {Overall (loaded) BC Size - 32MB}

   {Again, harder restrict the overall loading of Bytecode.}
   CL_BC_CompoundSize = 134217728; // 128MB

   {Restrict Nr of Parser/CC recursions.}
   CL_Max_CCRecurse = 16384;

   {Limits and Restrictions for Growin/Shrinking Internal Types, may be used
    initially and further extended}
   CL_LinearRegrow_Fast = 1024;
   CL_LinearRegrow_Medium = 256;
   CL_LinearRegrow_Slow = 16;

   {Limits and Restrictions for the Script Object System, running into these
    retrictions may be either a reference/implementation bug.}
   CL_SO_MaxItems = 4194304; {per collection restriction}
   CL_SO_MaxObjects = 16777216; {overall instance table restriction}
   CL_SO_MaxShortBuffer = 16777216; {max buffer, f.e. for strings}
   CL_SO_MaxArgs = 65536; {max number of arguments to be passed to any function}
   CL_SO_MaxFrame = 65536*2; {max function frame size}

   {Limits and Restrictions for the Machine}
   CL_VM_MaxRTStack = 4194304; {max number of stack slots (runtime stack)}
   CL_VM_MaxTPLStack = 65536; {max tpl stacksize (inclusion/callstack)}

procedure check_so_maxargs( argnum: VMInt ); inline;
procedure check_so_shortbuf( asize: MachineInt ); inline;
procedure check_so_maxcollection( asize: VMInt ); inline;
procedure check_so_maxobjects( asize: MachineInt ); inline;

procedure check_vm_maxrtstack( asize: VMInt ); inline;
procedure check_vm_maxtplstack( asize: VMInt ); inline;

function check_cc_maxargs( argnum: VMInt ): Boolean; inline;
function check_cc_maxframe( frmsize: VMInt ): Boolean; inline;
procedure check_cc_maxrecurse( nrrec: Machineint ); inline;

function check_bc_maxstab( n: VMInt ): Boolean; inline;
function check_bc_maxcode( n: MachineInt ): Boolean; inline;
function check_bc_maxsize( n: MachineInt ): Boolean; inline;
procedure check_bc_maxcomp( n: MachineInt ); inline;

implementation

uses eomsg;

procedure check_so_maxargs(argnum: VMInt);
begin
  if (argnum < 0) or
     (argnum > CL_SO_MaxArgs) then
    put_critical('SO Call Argument Limit - Terminate');
end;

procedure check_so_shortbuf(asize: MachineInt);
begin
  if (asize < 0) or
     (asize > CL_SO_MaxShortBuffer) then
    put_critical('SO Shortbuffer Limit Exceeded - Terminate');
end;

procedure check_so_maxcollection( asize: VMInt );
begin
  if (asize < 0) or
     (asize > CL_SO_MaxItems) then
    put_critical('SO Collection Limit Exceeded - Terminate');
end;

procedure check_so_maxobjects(asize: MachineInt);
begin
  if (asize < 0) or
     (asize > CL_SO_MaxObjects) then
    put_critical('SO Object Limit Exceeded - Terminate');
end;

procedure check_vm_maxrtstack(asize: VMInt);
begin
  if (asize < 0) or
     (asize > CL_VM_MaxRTStack) then
    put_critical('VM Object Stack Limit Exceeded - Terminate');
end;

procedure check_vm_maxtplstack(asize: VMInt);
begin
  if (asize < 0) or
     (asize > CL_VM_MaxTPLStack) then
    put_critical('VM Code Stack Limit Exceeded - Terminate');
end;

function check_cc_maxargs(argnum: VMInt): Boolean;
begin
  Result := (argnum >= 0) or
            (argnum <= CL_SO_MaxArgs);
end;

function check_cc_maxframe(frmsize: VMInt): Boolean;
begin
  Result := (frmsize >= 0) or
            (frmsize <= CL_SO_MaxFrame);
end;

procedure check_cc_maxrecurse(nrrec: Machineint);
begin
  if (nrrec < 0) or
     (nrrec > CL_Max_CCRecurse) then
    put_critical('Compiler Recursion Depth Exceeded - Terminate');
end;

function check_bc_maxstab(n: VMInt): Boolean;
begin
  Result := (n >= 0) and
            (n <= CL_BC_MaxStab);
end;

function check_bc_maxcode(n: MachineInt): Boolean;
begin
  Result := (n >= 0) and
            (n <= CL_BC_MaxCode);
end;

function check_bc_maxsize(n: MachineInt): Boolean;
begin
  Result := (n >= 0) and
            (n <= CL_BC_MaxSize);
end;

procedure check_bc_maxcomp(n: MachineInt);
begin
  if (n < 0) or
     (n > CL_BC_CompoundSize) then
    put_critical('Loader Limit Exceeded - Terminate');
end;

end.

