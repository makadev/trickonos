{   Unit for code caching and loading

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

{DOC>>
  Unit for File/Node Caching.
}

unit ccache;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, coreobj, gens, bytecode, fpath;

type
  PCodeReference = ^TCodeReference;
  TCodeReference = object
    pbcode: PByteCodeBlock;
    statfpath: String;
    fullname: String;
    shortname: String;
    isstray: Boolean;
    freeonleave: Boolean;
  end;

{DOC>> add or lookup cache entry, using (relative path) filename name,
       loader.statfpath as absolute predecessor (for relative lookup) or
       current relpath (i.g. pdw) if loader=nil.
       if lookup failed, a new entry is created with pbyte=nil}
function fcache_cache(loader: PCodeReference; const name: String): PCodeReference;

{DOC>> add a stray codeblock (bootstrab/hardcoded/dynamic), using loaders
       statfpath or current relpath if loader=nil}
function fcache_addstray(const shortname: String;
                         loader: PCodeReference;
                         pcode: PByteCodeBlock;
                         freeonleave: Boolean): PCodeReference;

procedure fcache_init;
procedure fcache_fini;

implementation

type
  TCodeRefList = specialize TSimpleList<PCodeReference>;

var
  NodeCache: THashTrie;
  CodeTable: TCodeRefList;

function fcache_cache(loader: PCodeReference; const name: String
  ): PCodeReference;
var fullname: String;
begin
  if Assigned(loader) then
    fpath_rel_set(loader^.statfpath);
  fullname := fpath_rel_enter(name,true);
  Result := NodeCache.Lookup(fullname);
  if not Assigned(Result) then
    begin
      Result := fcache_addstray(ExtractFileName(name),nil,nil,true);
      Result^.fullname := fullname;
      NodeCache.Add(fullname,Result);
    end;
  fpath_rel_leave;
  if Assigned(loader) then
    fpath_rel_leave;
end;

function fcache_addstray(const shortname: String; loader: PCodeReference;
  pcode: PByteCodeBlock; freeonleave: Boolean): PCodeReference;
begin
  Result := New(PCodeReference);
  Result^.pbcode := pcode;
  Result^.shortname := shortname;
  Result^.fullname := '^'+Result^.shortname;
  if Assigned(loader) then
    Result^.statfpath := loader^.statfpath
  else
    Result^.statfpath := fpath_current_rel;
  Result^.freeonleave := freeonleave;
  CodeTable.Push(Result);
end;

procedure fcache_init;
begin
  NodeCache.Init(10);
  CodeTable := TCodeRefList.Create;
end;

procedure fcache_fini;
var i: PtrInt;
begin
  for i := 0 to CodeTable.Count-1 do
    begin
      if Assigned(CodeTable.Items[i]^.pbcode) and
         CodeTable.Items[i]^.freeonleave then
        begin
          CodeTable.Items[i]^.pbcode^.Done;
          Dispose(CodeTable.Items[i]^.pbcode);
        end;
      Dispose(CodeTable.Items[i]);
    end;
  CodeTable.Free;
  NodeCache.Clear;
  NodeCache.Pack;
end;

end.

