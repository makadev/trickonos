{   Unit for the output stack and output handling

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

unit outstack;

{$mode objfpc}{$H+}

interface

uses SysUtils, commontl, eomsg, coreobj, fpath, ucfs;

type
  TOutPutFileMode = (
    opfm_open,
    opfm_reopen,
    opfm_path
  );

procedure outstack_flush;
procedure outstack_push( const name: String; omode: TOutPutFileMode );
function outstack_pop: Boolean;

procedure OutputWrite( us: PUCFS32String; nl: Boolean = false );
procedure OutputWrite( s: String; nl: Boolean = false ); deprecated;

procedure SetStdOut( allowed: Boolean );

procedure outstack_init;
procedure outstack_fini;

implementation

type
  POutputRecord = ^TOutputRecord;
  TOutputRecord = record
    fn: PUCFS32String;
    omode: TOutPutFileMode;
    instance: VMInt;
    outfile: THandle;
    outbuffer: array of Byte;
    outptr: VMInt;
  end;

var
  OutTab: THashTrie;
  OutRStack: array of POutputRecord;
  allowstdout: Boolean;

procedure PushRecord( orec: POutputRecord );
begin
  SetLength(OutRStack,Length(OutRStack)+1);
  OutRStack[High(OutRStack)] := orec;
  Inc(orec^.instance,1);
end;

procedure outstack_push(const name: String; omode: TOutPutFileMode);
var fname: String;
    fus: PUCFS32String;
    orec: POutputRecord;
begin
  if (Length(OutRStack)+1) >= CL_MAX_OutStack then
    put_critical('Hit Path and File Limit.');
  {translate file/path}
  fname := fpath_out_enter(name,omode in [opfm_reopen,opfm_open] {,omode = opfm_path});
  if (omode = opfm_open) and
     FileExists(fname) then
    put_critical('Open on Existing File not allowed. Stop');
  fus := ucfs_utf8us(name);
  if OutTab.Exists(fus) then
    begin
      orec := OutTab.Lookup(fus);
      ucfs_release(fus);
      if orec^.omode in [opfm_reopen,opfm_open] then
        begin
          if (orec^.omode = opfm_reopen) and
             (omode = opfm_open) then
            put_critical('Open on Open File would destroy content. Stop');
          if omode = opfm_path then
            put_critical('Open File on Path. Stop');
        end
      else
        begin
          if omode <> opfm_path then
            put_critical('Open Path on File. Stop');
        end;
      PushRecord(orec);
    end
  else
    begin
      orec := New(POutputRecord);
      FillByte(orec^,SizeOf(TOutputRecord),0);
      orec^.fn := fus;
      OutTab.Add(fus,orec);
      ucfs_release(fus);
      orec^.omode := omode;
      orec^.instance := 0;
      orec^.outptr := 0;
      PushRecord(orec);
      if omode in [opfm_reopen,opfm_open] then
        begin
          if (omode = opfm_open) or
             ((omode = opfm_reopen) and
              (not FileExists(fname))) then
            orec^.outfile := FileCreate(fname)
          else if omode = opfm_reopen then
            begin
              orec^.outfile := FileOpen(fname,fmOpenReadWrite);
              FileSeek(orec^.outfile,0,fsFromEnd);
            end;
          SetLength(orec^.outbuffer,CL_Default_FileBuffer);
        end;
    end;
end;

function outstack_pop: Boolean;
var orec: POutputRecord;
begin
  outstack_flush;
  if Length(OutRStack) > 0 then
    begin
      Result := true;
      orec := OutRStack[High(OutRStack)];
      if orec^.instance <= 1 then
        begin
          if orec^.omode in [opfm_reopen,opfm_open] then
            begin
              {close file}
              OutTab.Delete(orec^.fn);
              FileClose(orec^.outfile);
              SetLength(orec^.outbuffer,0);
              ASSERT(orec^.outptr <= 0);  // lost data, flush should have removed it
            end;
          Dispose(orec);
        end
      else
        Dec(orec^.instance,1);
      SetLength(OutRStack,Length(OutRStack)-1);
      fpath_out_leave;
    end
  else
    Result := false;
end;

procedure outstack_flush;
var orec: POutputRecord;
begin
  if (Length(OutRStack) > 0) and
     (OutRStack[High(OutRStack)]^.omode in [opfm_reopen,opfm_open]) then
    begin
      orec := OutRStack[High(OutRStack)];
      if orec^.outptr > 0 then
        begin
          FileWrite(orec^.outfile,
                    orec^.outbuffer[0],
                    orec^.outptr);
          orec^.outptr := 0;
        end;
    end
  else
    Flush(StdOut);
end;

procedure OutputWrite(us: PUCFS32String; nl: Boolean = false);
var orec: POutputRecord;
    dcbuf: TFSUtf8Char;
    pos: VMInt;
begin
  if (Length(OutRStack) > 0) and
     (OutRStack[High(OutRStack)]^.omode in [opfm_reopen,opfm_open]) then
    begin
      orec := OutRStack[High(OutRStack)];
      pos := 1;
      while pos <= ucfs_length(us) do
        begin
          if orec^.outptr >= CL_Default_FileBuffer then
            begin
              FileWrite(orec^.outfile,
                        orec^.outbuffer[0],
                        orec^.outptr);
              orec^.outptr := 0;
            end;
          ucfs_u32c_to_utf8c(ucfs_getc(us,pos),@dcbuf);
          ASSERT(dcbuf.len >= 1);
          ASSERT(dcbuf.len <= 6);
          if dcbuf.len <= 1 then
            begin
              orec^.outbuffer[orec^.outptr] := dcbuf.cbytes[0];
              Inc(orec^.outptr,1);
            end
          else
            begin
              if (dcbuf.len+orec^.outptr) >= CL_Default_FileBuffer then
                begin
                  FileWrite(orec^.outfile,
                            orec^.outbuffer[0],
                            orec^.outptr);
                  orec^.outptr := 0;
                end;
              Move( dcbuf.cbytes[0], orec^.outbuffer[orec^.outptr], dcbuf.len );
              Inc(orec^.outptr,dcbuf.len);
            end;
          Inc(pos,1);
        end;
      if nl then
        begin
          {$NOTE allow mode setting}
          dcbuf.len := Length(LineEnding);
          Move(String(LineEnding)[1],dcbuf.cbytes[0],dcbuf.len);

          if (dcbuf.len+orec^.outptr) >= CL_Default_FileBuffer then
            begin
              FileWrite(orec^.outfile,
                        orec^.outbuffer[0],
                        orec^.outptr);
              orec^.outptr := 0;
            end;
          Move( dcbuf.cbytes[0], orec^.outbuffer[orec^.outptr], dcbuf.len );
          Inc(orec^.outptr,dcbuf.len);
        end;
    end
  else
    begin
      if allowstdout then
        begin
          if nl then
            Writeln(StdOut,ucfs_to_utf8string(us))
          else
            Write(StdOut,ucfs_to_utf8string(us));
        end;
    end;
end;

procedure OutputWrite(s: String; nl: Boolean);
var tmps: PUCFS32String;
begin
  tmps := ucfs_utf8us(s);
  OutputWrite(tmps,nl);
  ucfs_release(tmps);
end;

procedure SetStdOut(allowed: Boolean);
begin
  allowstdout := allowed;
end;

procedure outstack_init;
begin
  SetLength(OutRStack,0);
  OutTab.Init(10);
end;

procedure outstack_fini;
begin
  while outstack_pop do ;
  SetLength(OutRStack,0);
  OutTab.Done;
end;

initialization
  allowstdout := true;

end.

