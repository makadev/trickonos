{   Unit for transparent compilation and bytecode loading

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

unit compload;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, commontl, eomsg, ccbase, cscan, csyms, ccstatn,
     assembl, fpath, ccache, bytecode, opcode;

{ use fpath_rel_enter before calling load,
    this results in an absolute path, so cache works more reliable.
  code writing may fail and emits a warning, but doesnt terminate as
    its not interesting for the actual template processing}
function LoadTemplate( incmode: TInsMIncludeMode; loader: PCodeReference;
                       const fname: String ): PCodeReference;

procedure SetByteCodeMode( WriteBC: Boolean );

procedure SetByteCodeDebugMode( switch: Boolean );

const
  Precompiled_Extension: array[TInsMIncludeMode] of String =
    ( '',
      '.pct',
      '.pcu',
      '' );

implementation

var
  compoundsize: VMInt;
  writecode: Boolean;
  bytecodedebug: Boolean;

procedure SetByteCodeDebugMode( switch: Boolean );
begin
  bytecodedebug := switch;
end;

procedure codewrite(c: PCodeReference);
var i: Integer;
    sout: String;
begin
  put_debug_for(-1,-1,'','PRINTING BYTECODE ');
  put_debug_for(-1,-1,'','  STAB('+c^.shortname+')');
  for i := 0 to High(c^.pbcode^.stab) do
    begin
      WriteStr(sout,'    <',i,',"',c^.pbcode^.stab[i],'">');
      put_debug_for(-1,-1,'',sout);
    end;
  put_debug_for(-1,-1,'','  CODE('+c^.shortname+')');
  for i := 0 to High(c^.pbcode^.image) do
    begin
      WriteStr(sout,'    @IP ',i:4,': ',c^.pbcode^.image[i].ASMCode);
      put_debug_for(c^.pbcode^.image[i].GetLine,c^.pbcode^.image[i].GetColumn,'',sout);
    end;
  put_debug_for(-1,-1,'','END ');
end;

procedure SetByteCodeMode( WriteBC: Boolean );
begin
  writecode := WriteBC;
end;

procedure TryLoadPreCompiledTemplate( incmode: TInsMIncludeMode; cacheref: PCodeReference );
var
  mstream: TMemoryStream;
  ext: String;
  err: TBCLErrorCode;
begin
  ext := Precompiled_Extension[incmode];

  if FileExists(cacheref^.fullname+ext) then
    begin
      mstream := TMemoryStream.Create;
      try
        mstream.LoadFromFile(cacheref^.fullname+ext);
      except on e: Exception do
        begin
          put_warning('Loading "'+cacheref^.fullname+ext+'" failed, recompiling');
          mstream.Free;
          mstream := nil;
        end;
      end;
    end
  else
    Exit;

  cacheref^.pbcode := New(PByteCodeBlock);
  Fillbyte(cacheref^.pbcode^,SizeOf(TByteCodeBlock),0);
  if Assigned(mstream) then
    begin
      cacheref^.pbcode^.Init;
      try
        try
          err := cacheref^.pbcode^.LoadDecode(mstream);
          if err <> bcec_validated then
            begin
              put_warning('Decoding "'+cacheref^.fullname+ext+'" failed, recompiling');
              if debug_mode then
                begin
                  WriteStr(ext,'Decoding returned ',err);
                  put_debug(ext);
                end;
              cacheref^.pbcode^.Done;
              Dispose(cacheref^.pbcode);
              cacheref^.pbcode := nil;
            end;
        finally
          mstream.Free;
        end;
      except on e: Exception do
        begin
          cacheref^.pbcode^.Done;
          Dispose(cacheref^.pbcode);
          cacheref^.pbcode := nil;
        end;
      end;
    end;
end;

function LoadTemplate( incmode: TInsMIncludeMode; loader: PCodeReference;
  const fname: String ): PCodeReference;
var pnode: TParserNode;
    fstream: TFileStream;
begin
  Result := fcache_cache(loader,fname);
  if not Assigned(Result^.pbcode) then
    begin
      TryLoadPreCompiledTemplate(incmode,Result);
      if not Assigned(Result^.pbcode) then
        begin
          put_info('Compiling "'+Result^.fullname+'"');
          {setup compiler/parser/scanner}
          InitCC;
          if incmode = mincl_use then
            InitScanner(true,Result^.shortname,Result^.fullname)
          else
            InitScanner(false,Result^.shortname,Result^.fullname);
          {parse}
          pnode := TTemplateFile.Parse;
          if not Assigned(pnode) then
            put_internalerror(2011120501); // <- should'nt happen
          {compile}
          InitAssembler;
          if pnode.Compile then
            begin
              Result^.pbcode := New(PByteCodeBlock);
              Fillbyte(Result^.pbcode^,SizeOf(TByteCodeBlock),0);
              Result^.pbcode^.Init;
              Result^.pbcode^.inclmode := incmode;
              if pnode.Assembly.LinkAndWrite(Result^.pbcode^) then
                begin
                  if writecode then
                    begin
                      if not FileExists(Result^.fullname+Precompiled_Extension[incmode]) then
                        begin
                          try
                            fstream := TFileStream.Create(Result^.fullname+Precompiled_Extension[incmode],fmCreate or fmOpenReadWrite);
                            try
                              Result^.pbcode^.WriteEncode(fstream);
                            finally
                              fstream.Free;
                            end;
                          except on e:Exception do
                            begin
                              put_warning('Couldn''t write "'+Result^.fullname+Precompiled_Extension[incmode]+'"');
                              if debug_mode then
                                put_debug('Exception caught: '+e.Message);
                            end;
                          end;
                        end
                      else
                        put_warning(Result^.fullname+Precompiled_Extension[incmode]+' exists, wont rewrite');
                    end;
                end
              else
                begin
                  put_error_for(CurrentStreamLine,-1,Result^.shortname,'Assembling failed');
                  put_critical('Couldn''t load "'+Result^.fullname+'"');
                end;
            end
          else
            begin
              put_error_for(CurrentStreamLine,-1,Result^.shortname,'Compilation failed');
              put_critical('Couldn''t load "'+Result^.fullname+'"');
            end;
          ReleaseCC;
        end;
      Inc(compoundsize,Result^.pbcode^.bcsize);
      check_bc_maxcomp(compoundsize);
      if bytecodedebug and debug_mode then
        codewrite(Result);
    end;
  if Assigned(Result^.pbcode) and
     (Result^.pbcode^.inclmode <> incmode) then
    put_critical('Wrong Inclusion Mode (reinclusion in differrent mode not supported!)');
end;

initialization
  compoundsize := 0;
  bytecodedebug := false;
  writecode := false;

end.

