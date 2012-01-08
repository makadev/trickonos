{   Unit for Path/File handling

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

{DOC>> Unit for Path/File Handling.

  In general, Resource Identifieres are used, like <proto>://<uri> where
  <proto> is one of:
  @unorderedList(
    @item abs   - Absolute Path (system dependend)
    @item intr  - Interpreter Path Relative (never changes)
    @item pwd   - Working Path Relative (never changes)

    @item rel   - Template File Relative (initial rel=pwd)
    @item out   - Template Outpath Relative (initial out=pwd)
    @item pkg   - Package Path(es) Relative
  )
  and <uri> is <component>(/<component>)* @br @br

  For convenience, System dependend Pathes are allowed, which is on @br
   Windows [<drive>':'\]<component>(\<component>)* and @br
   Unix [/]<component>(/<component)* @br
}

unit fpath;

{$mode objfpc}{$H+}

interface

uses SysUtils, eomsg;

type
  TRIServicePath = (
    sspUnknown,
    sspAbsolute,
    sspPackage,
    sspFileRelative,
    sspInterpreter,
    sspOutpath,
    sspWorkDir
  );

  TRIServicePathSet = set of TRIServicePath;

const
  C_SERVICE_STRING: array[TRIServicePath] of String = (
    'unknown',
    'abs',
    'pkg',
    'rel',
    'intr',
    'out',
    'pwd'
  );

{DOC>> Add Package Path.@br
  Allowed Service Prefixes: intr, pwd, pkg, abs @br
  Automagic Converts: absolute, relative to pwd or intr @br
  ignores if path doesn't exist or uses illegal prefix
}
procedure fpath_add_pkg( const suri: String );

{DOC>> Push (Template) File Path on Relative File Path Stack.@br
  Allowed Service Prefixes: pwd, pkg, rel @br
  Automagic Converts: relative to rel @br
  Fails if path/file doesn't exist or uses illegal prefix
}
function fpath_rel_enter( const suri: String; onfile: Boolean ): String;

{DOC>> Set absolute (Template) File Path, Push on Relative File Path Stack.@br
For internal use, allows pushing an Absolut Path. If Path is not absolute,
fpath_rel_enter is used}
procedure fpath_rel_set( const suri: String );

{DOC>> Push (Output) File Path on Output Path Stack.@br
  Allowed Service Prefixes: pwd, out, abs @br
  Automagic Converts: absolute, relative to out @br
  Fails if path doesn't exist (when createpath=false) or uses illegal prefix
  Fails if file already exists.
}
function fpath_out_enter( const suri: String; onfile: Boolean; createpath: Boolean = False ): String;

procedure fpath_rel_leave;
procedure fpath_out_leave;

function fpath_current_rel: String;

procedure fpath_init;
procedure fpath_fini;

implementation

{$IF not(declared(IncludeLeadingPathDelimiter))}

{2.4.0 compat declare ILPD}

function IncludeLeadingPathDelimiter(const s: String): String;
{wont check for drive prefix on windows, so take care}
begin
  if (Length(s) > 0) and
     (s[1] <> PathDelim) then
    Result := PathDelim + s
  else
    Result := s; 
end;

{$ENDIF}

var
  {never change}
  IntrPath: String;
  WorkDir: String;

{package list}
var
  PkgPathes: array of String;

procedure PackagePathAdd( const p: String );
var i: Integer;
begin
  {check existing}
  if DirectoryExists(p) then
    begin
      {check dups}
      for i := 0 to High(PkgPathes) do
        if p = PkgPathes[i] then
          Exit;
      {add}
      SetLength(PkgPathes,Length(PkgPathes)+1);
      PkgPathes[High(PkgPathes)] := p;
    end
  else
    put_debug('Pkg Path does not exist, ignored: '+p);
end;

{filepath stack}
var
  FilePathStack: array of String;

procedure FilePathPush( const p: String );
begin
  SetLength(FilePathStack,Length(FilePathStack)+1);
  FilePathStack[High(FilePathStack)] := p;
end;

procedure FilePathPop;
begin
  SetLength(FilePathStack,Length(FilePathStack)-1);
end;

{outpath stack}
var
  OutPathStack: array of String;

procedure OutPathPush( const p: String );
begin
  SetLength(OutPathStack,Length(OutPathStack)+1);
  OutPathStack[High(OutPathStack)] := p;
end;

procedure OutPathPop;
begin
  SetLength(OutPathStack,Length(OutPathStack)-1);
end;

{suri helpers}

type
  TSURIPath = record
    servtype: TRIServicePath;
    containsbs: Boolean;
    comps: array of String;
  end;

function ServiceToType( const s: String ): TRIServicePath;
var i: TRIServicePath;
    sl: String;
begin
  Result := sspUnknown;
  sl := LowerCase(s);
  for i := sspUnknown to sspWorkDir do
    if sl = C_SERVICE_STRING[i] then
      Exit(i);
end;

procedure StrToSURIPath( const s: String; out surip: TSURIPath );
var state, pos, comp: Integer;
    currcomp: String;

    procedure makepathcomp;
    begin
      if (Length(currcomp) > 0) and
         (currcomp <> '.') then
        begin
          if (Length(currcomp) = 2) and
             (currcomp = '..') then
            surip.containsbs := true;
          SetLength(surip.comps,comp+1);
          if FileNameCaseSensitive then
            surip.comps[comp] := currcomp
          else
            surip.comps[comp] := Lowercase(currcomp);
          Inc(comp,1);
          currcomp := '';
        end;
    end;

begin
  SetLength(surip.comps,0);
  surip.containsbs := false;
  surip.servtype := sspUnknown;
  pos := 1;
  state := 1;
  comp := 0;

  {check for abs path without abs servicet}
{$ifdef windows}
  currcomp := ExtractFileDrive(s);
  if Length(currcomp) > 0 then
    begin
      surip.servtype := sspAbsolute;
      SetLength(surip.comps,1);
      surip.comps[0] := ExcludeTrailingPathDelimiter(Lowercase(currcomp));
      comp := 1;
      pos := 3;
      state := 3;
    end;
{$else unix or ri}
  if (Length(s) > 0) and
     (s[1] = '/') then
    begin
      surip.servtype := sspAbsolute;
      pos := 2;
      state := 2;
    end;
{$endif}

  currcomp := '';
  while pos <= Length(s) do
    begin
      case state of
        1:
          begin
            {reading mixed, may be suri or local path}
            if (s[pos] = ':') and
               (pos < (Length(s)-1)) and
               (s[pos+1] = '/') and
               (s[pos+2] = '/') then
             begin
               surip.servtype := ServiceToType(currcomp);
               if surip.servtype <> sspUnknown then
                 begin
                   state := 2;
                   Inc(pos,3);
                   currcomp := '';
                   continue;
                 end;
             end;

            if (s[pos] = '\') then
               begin
                 makepathcomp;
                 state := 3;
                 Inc(pos,1);
                 continue;
               end;

            {check for unix slashes}
            if (s[pos] = '/') then
               begin
                 makepathcomp;
                 state := 2;
                 Inc(pos,1);
                 continue;
               end;
          end;
        2:
          begin
            {reading unix like path, unix or ri
              strip multi slashes}
            if (s[pos] = '/') then
               begin
                 makepathcomp;
                 Inc(pos,1);
                 continue;
               end;
          end;
        3:
          begin
            {reading windows path,
              strip multi backslashes,
              lowercase}
            if (s[pos] = '\') then
               begin
                 makepathcomp;
                 Inc(pos,1);
                 continue;
               end;
          end;
      end;
      currcomp := currcomp + s[pos];
      Inc(pos,1);
    end;

  // add whats left
  makepathcomp;
end;

function SURICompRebuild( const suri: TSURIPath ): String;
var i: Integer;
begin
  Result := '';
  if 0 <= High(suri.comps) then
    Result := suri.comps[0];
  for i := 1 to High(suri.comps) do
    Result := Result + DirectorySeparator + suri.comps[i];
end;

function SURIRebuildUnified( const s: String ): String;
var surip: TSURIPath;
    i,j: Integer;
begin
  StrToSURIPath(s,surip);
  i := 0;
  j := 0;
  while i <= High(surip.comps) do
    begin
      if surip.comps[i] <> '..' then
        begin
          surip.comps[j] := surip.comps[i];
          Inc(j,1);
        end
      else
        begin
          Dec(j,1);
          if j <= 0 then
            begin
              SetLength(surip.comps,0);
              put_critical('Denoted Path overrides Root: '+Result);
            end;
        end;
      Inc(i,1);
    end;
  SetLength(surip.comps,j);
  if surip.servtype = sspAbsolute then
    begin
{$ifdef windows}
      Result := SURICompRebuild(surip);
      if Length(ExtractFileDrive(Result)) <= 0 then
        Result := IncludeTrailingPathDelimiter(ExtractFileDrive(WorkDir))+ Result;
{$else}
      Result := IncludeLeadingPathDelimiter(SURICompRebuild(surip));
{$endif}
    end
  else
    Result := ExcludeTrailingPathDelimiter(SURICompRebuild(surip));
  SetLength(surip.comps,0);
end;

function SURIRewrite( const suri: TSURIPath ): String;
begin
  case suri.servtype of
{$ifdef windows}
    sspAbsolute:
      begin
        Result := SURICompRebuild(suri);
        if Length(ExtractFileDrive(Result)) <= 0 then
          Result := IncludeTrailingPathDelimiter(ExtractFileDrive(WorkDir))+ Result;
      end;
{$else}
    sspAbsolute: Result := IncludeLeadingPathDelimiter(SURICompRebuild(suri));
{$endif}
    sspFileRelative: Result :=
      IncludeTrailingPathDelimiter(FilePathStack[High(FilePathStack)]) +
      SURICompRebuild(suri);
    sspInterpreter: Result :=
      IncludeTrailingPathDelimiter(IntrPath) +
      SURICompRebuild(suri);
    sspOutpath: Result :=
      IncludeTrailingPathDelimiter(OutPathStack[High(OutPathStack)]) +
      SURICompRebuild(suri);
    sspWorkDir: Result :=
      IncludeTrailingPathDelimiter(IntrPath) +
      SURICompRebuild(suri);
    else
      put_internalerror(201111090);
  end;
  if suri.containsbs then
    Result := SURIRebuildUnified( ExcludeTrailingPathDelimiter(Result) )
  else
    Result := ExcludeTrailingPathDelimiter(Result);
end;

function PKGSURIRewrite( const suri: TSURIPath; pkgi: Integer ): String;
begin
  Result := IncludeTrailingPathDelimiter(PkgPathes[pkgi]) +
            SURICompRebuild(suri);
  if suri.containsbs then
    Result := SURIRebuildUnified( ExcludeTrailingPathDelimiter(Result) )
  else
    Result := ExcludeTrailingPathDelimiter(Result);
end;

procedure CreateDirDeep( const path: String );
var surip: TSURIPath;
    p: String;
    i: Integer;
begin
  StrToSURIPath(path,surip);
  p := surip.comps[0];
  for i := 1 to High(surip.comps) do
    begin
      p := p + DirectorySeparator + surip.comps[i];
      if not DirectoryExists(p) then
        if not CreateDir(p) then
          begin
            SetLength(surip.comps,0);
            put_warning('There may be Garbage Pathes: Check '+path);
            put_critical('Couldn''t create Path: '+p);
          end;
    end;
end;

{pkg path add}

procedure fpath_add_pkg(const suri: String);
{
  - Checks for Service Path (intr,pwd,pkg,abs), on match -> expand.
  - fail if servicepath is rel,out - not allow for package pathes
  - on fail, Checks for Pwd Relative Path, on match -> expand
  - on fail, Checks for Interpreter Relative Path, on match -> expand
  - fail
}
var surip: TSURIPath;
    p: String;
    pkgi: Integer;
begin
  StrToSURIPath(suri,surip);
  if surip.servtype <> sspUnknown then
    begin
      if surip.servtype in [sspInterpreter,sspWorkDir,sspAbsolute,sspPackage] then
        begin
          if surip.servtype = sspPackage then
            begin
              if surip.containsbs then
                begin
                  put_warning('Illegal Package Path: '+suri);
                  SetLength(surip.comps,0);
                  Exit;
                end;
              pkgi := 0;
              while pkgi <= High(PkgPathes) do
                begin
                  p := PKGSURIRewrite(surip,pkgi);
                  if DirectoryExists(p) then
                    break;
                  Inc(pkgi,1)
                end;
              if pkgi > High(PkgPathes) then
                begin
                  put_warning('Couldn''t find Package Path: '+suri);
                  SetLength(surip.comps,0);
                  Exit;
                end;
            end
          else
            begin
              p := SURIRewrite(surip);
            end;
        end
      else
        put_warning('Illegal Package Path: '+suri);
    end
  else
    begin
      surip.servtype := sspWorkDir;
      p := SURIRewrite(surip);
      if not DirectoryExists(p) then
        begin
          surip.servtype := sspInterpreter;
          surip.servtype := sspInterpreter;
          p := SURIRewrite(surip);
        end;
    end;
  if not DirectoryExists(p) then
    begin
      put_warning('Couldn''t find Package Path: '+suri);
      SetLength(surip.comps,0);
      Exit;
    end;
  SetLength(surip.comps,0);
  PackagePathAdd(p);
end;

function fpath_rel_enter(const suri: String; onfile: Boolean ): String;
{
  Allowed Service Prefixes: pwd, pkg, rel
  relative to rel
  Fails if path doesn't exist or uses illegal prefix
}
var surip: TSURIPath;
    p,f: String;
    pkgi: Integer;
begin
  StrToSURIPath(suri,surip);
  f := '';
  if onfile and (Length(surip.comps) > 0) then
    begin
      f := surip.comps[High(surip.comps)];
      SetLength(surip.comps,Length(surip.comps)-1);
{$ifdef windows}
      if (Length(surip.comps) <= 0) and
         (surip.servtype = sspAbsolute) then
        put_critical('Illegal Filename: '+suri); {<< stripped Drive Component}
{$endif}
    end;
  if onfile and
     (Length(f) <= 0) then
    begin
      SetLength(surip.comps,0);
      put_critical('No Filename given');
    end;
  if surip.servtype = sspUnknown then
    begin
      surip.servtype := sspFileRelative;
      p := SURIRewrite(surip);
      if onfile then
        begin
          Result := IncludeTrailingPathDelimiter(p)+f;
          if not FileExists(Result) then
            surip.servtype := sspPackage
          else
            begin
              FilePathPush(p);
              SetLength(surip.comps,0);
              Exit(Result);
            end;
        end
      else
        begin
          Result := p;
          if not DirectoryExists(Result) then
            surip.servtype := sspPackage
          else
            begin
              FilePathPush(p);
              SetLength(surip.comps,0);
              Exit(Result);
            end;
        end;
    end;
  if not (surip.servtype in [sspFileRelative,sspWorkDir,sspPackage]) then
    begin
      SetLength(surip.comps,0);
      put_critical('Illegal File Path: '+suri);
    end;
  case surip.servtype of
    sspFileRelative, sspWorkDir:
      begin
        p := SURIRewrite(surip);
        if onfile then
          begin
            Result := ExcludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(p)+f);
            if not FileExists(Result) then
              begin
                SetLength(surip.comps,0);
                put_critical('File doesnt Exist: '+suri);
              end;
          end
        else
          begin
            Result := p;
            if not DirectoryExists(Result) then
              begin
                SetLength(surip.comps,0);
                put_critical('Directory doesnt Exist: '+suri);
              end;
          end;
      end;
    sspPackage:
      begin
        if surip.containsbs then
          begin
            SetLength(surip.comps,0);
            put_critical('Illegal Package Path: '+suri);
            Exit;
          end;
        pkgi := 0;
        while pkgi <= High(PkgPathes) do
          begin
            p := PKGSURIRewrite(surip,pkgi);
            if onfile then
              begin
                Result := ExcludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(p)+f);
                if FileExists(Result) then
                  break;
              end
            else
              begin
                Result := p;
                if DirectoryExists(Result) then
                  break;
              end;
            Inc(pkgi,1)
          end;
        if pkgi > High(PkgPathes) then
          begin
            SetLength(surip.comps,0);
            put_critical('Invalid Path or File: '+suri);
          end;
      end;
  end;
  SetLength(surip.comps,0);
  FilePathPush(p);
end;

procedure fpath_rel_set(const suri: String);
var surip: TSURIPath;
    p: String;
begin
  StrToSURIPath(suri,surip);
  if surip.servtype = sspAbsolute then
    begin
      p := SURIRewrite(surip);
      SetLength(surip.comps,0);
      FilePathPush(p);
    end
  else
    begin
      SetLength(surip.comps,0);
      fpath_rel_enter(suri,false);
    end;
end;

function fpath_out_enter(const suri: String; onfile: Boolean;
  createpath: Boolean): String;
{
  Allowed Service: pwd, out, abs
  Automagic Converts: absolute, relative to out
  Fails if path doesn't exist (when createpath=false) or uses illegal prefix
  Fails if file already exists.
}
var surip: TSURIPath;
    p,f: String;
begin
  StrToSURIPath(suri,surip);
  f := '';
  if onfile and (Length(surip.comps) > 0) then
    begin
      f := surip.comps[High(surip.comps)];
      SetLength(surip.comps,Length(surip.comps)-1);
      if Length(surip.comps) <= 0 then
        put_critical('Illegal Filename: '+suri);
    end;
  if onfile and
     (Length(f) <= 0) then
    begin
      SetLength(surip.comps,0);
      put_critical('Illegal Filename: '+suri);
    end;
  if surip.servtype = sspUnknown then
    surip.servtype := sspOutpath;
  if not (surip.servtype in [sspWorkDir,sspOutpath,sspAbsolute]) then
    begin
      SetLength(surip.comps,0);
      put_critical('Illegal Out Path: '+suri);
    end;

  p := SURIRewrite(surip);
  SetLength(surip.comps,0);
  if onfile then
    begin
      Result := ExcludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(p)+f);
      if FileExists(Result) then
        begin
          put_critical('File Exists: '+suri);
        end
      else
        begin
          if not DirectoryExists(p) then
            begin
              if createpath then
                CreateDirDeep(p)
              else
                put_critical('Path doesnt Exists: '+suri);
            end;
        end;
    end
  else
    begin
      Result := p;
      if not DirectoryExists(p) then
        begin
          if createpath then
            CreateDirDeep(p)
          else
            put_critical('Path doesnt Exists: '+suri);
        end;
    end;
end;

procedure fpath_rel_leave;
begin
  if Length(FilePathStack) > 0 then
    SetLength(FilePathStack,Length(FilePathStack)-1)
  else
    put_critical('FilePath Underflow');
end;

procedure fpath_out_leave;
begin
  if Length(OutPathStack) > 0 then
    SetLength(OutPathStack,Length(OutPathStack)-1)
  else
    put_critical('OutPath Underflow');
end;

function fpath_current_rel: String;
begin
  Result := FilePathStack[High(FilePathStack)];
end;

{init&fin}
procedure fpath_init;
begin
  IntrPath := ExcludeTrailingBackslash(ExtractFilePath(ParamStr(0)));
  if not DirectoryExists(IntrPath) then
    put_critical('Couldn''t determine Interpreter Location (intr).');
  WorkDir := ExcludeTrailingBackslash(ExpandFileName(GetCurrentDir));
  if not DirectoryExists(WorkDir) then
    begin
      put_warning('Couldn''t determine Working Directory (pwd), reset to intr.');
      WorkDir := IntrPath;
    end;
  SetLength(PkgPathes,0);
  SetLength(FilePathStack,0);
  SetLength(OutPathStack,0);
  {def out/filepath}
  FilePathPush(WorkDir);
  OutPathPush(WorkDir);
  {def pkgpath}
  //PackagePathAdd(IntrPath+DirectorySeparator+'pkg');
end;

procedure fpath_fini;
begin
  SetLength(PkgPathes,0);
  SetLength(FilePathStack,0);
  SetLength(OutPathStack,0);
end;

end.

