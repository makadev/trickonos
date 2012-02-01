{   Unit for application/vm/so startup and commandline interface

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

unit appstart;

{$mode objfpc}{$H+}

interface

uses SysUtils, eomsg, bytecode, fpath, ccache,
     vmstate, compload, cscan, opcode, vmrun, ucfs, outstack
     {$ifdef CLEANSHUTDOWN}, ccbase {$endif};

const
{$Include pver.inc}

  COPYING_TEXT = '  Trickonos version '+VERSION_TAG+', Copyright (C) 2011-2012 Matthias Karbe'+LineEnding+
                 '  This program comes with ABSOLUTELY NO WARRANTY; for details see COPYING.txt;'+LineEnding+
                 '  This is free software, and you are welcome to redistribute it'+LineEnding+
                 '  under certain conditions; for details see the source files and COPYING.txt;';

(* first part, init & fin *)

type
  TInitProc = procedure;
  TFinProc = procedure;

procedure RegisterInit( i: TInitProc );
procedure RegisterFin( f: TFinProc );

(* second, for those adding parameter handling *)

type
  TParamStrings = array of String;
  TParamHandler = function( var params: TParamStrings ):Boolean;

procedure RegisterOpt( const longopt, shortopt, descr: String; NrArgs: Integer;
                       paramhandler: TParamHandler );

(* third, for those instructions/macros contributing to the bootstrap/shutdown
   code *)

type
  TStartupInjector = procedure( var ARootBlock: TByteCodeBlock );
  TShutdownInjector = procedure( var ARootBlock: TByteCodeBlock );

procedure RegisterStartupInjector( AInjector: TStartupInjector );
procedure RegisterShutdownInjector( AInjector: TStartupInjector );

(* machine boot *)

procedure Setup;
procedure Run;
procedure Shutdown;

implementation

(*******************************************************************************
 * Bootstrap/Finalization Register
 ******************************************************************************)
var
  Inits: array of TInitProc;
  Fins: array of TFinProc;

procedure RegisterInit( i: TInitProc );
begin
  if not Assigned(i) then
     put_internalerror(201110235);
  SetLength(Inits,Length(Inits)+1);
  Inits[High(Inits)] := i;
end;

procedure RegisterFin( f: TFinProc );
begin
  if not Assigned(f) then
     put_internalerror(201110234);
  SetLength(Fins,Length(Fins)+1);
  Fins[High(Fins)] := f;
end;

(*******************************************************************************
 * Options Entry Register
 ******************************************************************************)

type
  TOptEntry = record
    longopt: String;
    shortopt: String;
    descr: String;
    nrargs: Integer;
    handler: TParamHandler;
  end;

var
  OptTable: array of TOptEntry;

procedure RegisterOpt(const longopt, shortopt, descr: String; NrArgs: Integer;
  paramhandler: TParamHandler);
{$ifdef debug}
var
  i: Integer;
{$endif}
begin
{$ifdef debug}
  for i := 0 to High(OptTable) do
    if ((Length(longopt) > 0) and (longopt = OptTable[i].longopt)) or
       ((Length(shortopt) > 0) and (shortopt = OptTable[i].shortopt)) then
     put_internalerror(201110231);
  if (NrArgs < 0) or (NrArgs > 10) then
     put_internalerror(201110232);
  if not Assigned(paramhandler) then
     put_internalerror(201110233);
{$endif}
  SetLength(OptTable,Length(OptTable)+1);
  OptTable[High(OptTable)].longopt := longopt;
  OptTable[High(OptTable)].shortopt := shortopt;
  OptTable[High(OptTable)].descr := descr;
  OptTable[High(OptTable)].nrargs := NrArgs;
  OptTable[High(OptTable)].handler := paramhandler;
end;

(*******************************************************************************
 * Default and Registered Options Detection
 ******************************************************************************)

function VersionHandler( var unused: TParamStrings ): Boolean;
begin
  WriteLn(StdErr,'  Trickonos '+VERSION_LONG);
  WriteLn(StdErr);
  WriteLn(StdErr,'  This program version was compiled on ',{$I %DATE%});
  Writeln(StdErr,'  Free Pascal Compiler version: ',{$I %FPCVERSION%});
  Writeln(StdErr,'  Target CPU: ',{$I %FPCTARGETCPU%},'-',{$I %FPCTARGETOS%});
  WriteLn(StdErr);
  WriteLn(StdErr,COPYING_TEXT);
  Halt(0);
  Result := true;
end;

procedure HelpAndHalt;
const
  Com_Spaces = '  ';
  Descr_Spaces = '        ';

var i,j,k: Integer;
begin
  WriteLn(StdErr);
  WriteLn(StdErr,'Usage: ',ParamStr(0),' [-opts] [--opts] <templatefile>');
  for i := 0 to High(OptTable) do
    begin
      WriteLn(StdErr);
      if Length(OptTable[i].shortopt) > 0 then
        Write(StdErr,Com_Spaces,'-',OptTable[i].shortopt);
      if Length(OptTable[i].longopt) > 0 then
        Write(StdErr,Com_Spaces,'--',OptTable[i].longopt);
      if OptTable[i].nrargs > 0 then
        WriteLn(StdErr,' ( ',OptTable[i].nrargs:2,' args ): ')
      else
        WriteLn(StdErr,': ');
      k := 0;
      for j := 1 to Length(OptTable[i].descr) do
        begin
          if OptTable[i].descr[j] = #10 then
            begin
              WriteLn(StdErr,Descr_Spaces,Copy(OptTable[i].descr,k+1,j-(k+1)));
              k := j;
            end;
        end;
      if Length(OptTable[i].descr)-k > 0 then
        begin
          WriteLn(StdErr,Descr_Spaces,Copy(OptTable[i].descr,k+1,Length(OptTable[i].descr)-k));
        end;
    end;
  WriteLn(StdErr);
  WriteLn(StdErr,COPYING_TEXT);
  WriteLn(StdErr);
  Halt(1);
end;

var
  fisset: Boolean;
  fname: String;

function InitFileHandler( var aonefile: TParamStrings ): Boolean;
begin
  if fisset then
    begin
      WriteLn(StdErr,'Already one File specified: ',fname);
      WriteLn(StdErr,'Cant use File: ',aonefile[0]);
      Exit(false);
    end;
  Result := true;
  fisset := true;
  fname := aonefile[0];
end;

function VerbosityHandler( var parms: TParamStrings ): Boolean;
begin
  Result := true;
  case LowerCase(parms[0][1]) of
    'd': if (Length(parms[0])=1) or
            (LowerCase(parms[0])='debug') then
           set_outmode(omDebug)
         else
           Exit(False);
    'i': if (Length(parms[0])=1) or
            (LowerCase(parms[0])='info') then
           set_outmode(omInfo)
         else
           Exit(False);
    'w': if (Length(parms[0])=1) or
            (LowerCase(parms[0])='warning') then
           set_outmode(omWarning)
         else
           Exit(False);
    'e': if (Length(parms[0])=1) or
            (LowerCase(parms[0])='error') then
           set_outmode(omError)
         else
           Exit(False);
    'c': if (Length(parms[0])=1) or
            (LowerCase(parms[0])='critical') then
           set_outmode(omCritical)
         else
           Exit(False);
    's': if (Length(parms[0])=1) or
            (LowerCase(parms[0])='silent') then
           set_outmode(omSilent)
         else
           Exit(False);
    else
      Result := false;
  end;
end;

procedure GetOpts;
var i, j, nargs: Integer;
    suffixarg, isshort: Boolean;
    handler: TParamHandler;
    parmstring: String;
    CurrentParms: array of String;

  procedure scanlongopt;
  {check "--opt" or "--opt=value"}
  var x: Integer;
  begin
    j := 0;
    while j <= High(OptTable) do
      begin
        x := Length(parmstring)-2;
        suffixarg := (x > (Length(OptTable[j].longopt)+1)) and
                     (parmstring[Length(OptTable[j].longopt)+3] = '=');
        if ((x = Length(OptTable[j].longopt)) or
            (suffixarg and (OptTable[j].nrargs>0))) then
           begin
             x := Length(OptTable[j].longopt);
             while (x > 0) and (OptTable[j].longopt[x] = parmstring[x+2]) do
               Dec(x,1);
             if x <= 0 then
                begin
                  isshort := false;
                  handler := OptTable[j].handler;
                  nargs := OptTable[j].nrargs;
                  if suffixarg then
                    begin
                      SetLength(CurrentParms,1);
                      CurrentParms[0] := Copy(parmstring,Length(OptTable[j].longopt)+4,
                                                         Length(parmstring)-
                                                         (Length(OptTable[j].longopt)+3));
                    end;
                  Exit;
                end;
           end;
        Inc(j,1);
      end;
  end;

  procedure scanshortopt;
  {check "-opt" or "-optValue"}
  var x: Integer;
  begin
    j := 0;
    while j <= High(OptTable) do
      begin
        x := Length(parmstring)-1;
        suffixarg := x > (Length(OptTable[j].shortopt));
        if ((x = Length(OptTable[j].shortopt)) or
            (suffixarg and (OptTable[j].nrargs>0))) then
           begin
             x := Length(OptTable[j].shortopt);
             while (x > 0) and (OptTable[j].shortopt[x] = parmstring[x+1]) do
               Dec(x,1);
             if x<=0 then
                begin
                  isshort := true;
                  handler := OptTable[j].handler;
                  nargs := OptTable[j].nrargs;
                  if suffixarg then
                    begin
                      SetLength(CurrentParms,1);
                      CurrentParms[0] := Copy(parmstring,Length(OptTable[j].shortopt)+2,
                                                         Length(parmstring)-
                                                         (Length(OptTable[j].shortopt)+1));
                    end;
                  Exit;
                end;
           end;
        Inc(j,1);
      end;
  end;

begin
  SetLength(CurrentParms,0);
  i := 1;
  while i <= Paramcount do
    begin
      {check for short/long opt}
      parmstring := ParamStr(i);
      handler := nil;
      if (Length(parmstring) > 1) and
         (parmstring[1] = '-') then
        begin
          scanshortopt;
          if (not Assigned(handler)) and
             (Length(parmstring) > 2) and
             (parmstring[2] = '-') then
            scanlongopt;
        end;
      if Assigned(handler) then
        begin
          {get params}
          while (Length(CurrentParms) < nargs) and
                (i < Paramcount) do
            begin
              Inc(i,1);
              SetLength(CurrentParms,Length(CurrentParms)+1);
              CurrentParms[High(CurrentParms)] := ParamStr(i);
            end;
          if Length(CurrentParms) = nargs then
            begin
              if not handler(CurrentParms) then
                begin
                  if isshort then
                    WriteLn(StdErr,'>>  Invalid Arguments for -',OptTable[j].shortopt)
                  else
                    WriteLn(StdErr,'>>  Invalid Arguments for --',OptTable[j].longopt);
                  HelpAndHalt;
                end;
              SetLength(CurrentParms,0);
            end
          else
            begin
              if isshort then
                WriteLn(StdErr,'>>  Missing Arguments for -',OptTable[j].shortopt)
              else
                WriteLn(StdErr,'>>  Missing Arguments for --',OptTable[j].longopt);
              HelpAndHalt;
            end;
        end
      else
        begin
          {no handler, check for last opt}
          if i<>Paramcount then
            begin
              WriteLn(StdErr,'>>  Dont know about Option ',parmstring);
              HelpAndHalt;
            end
          else
            break;
        end;
      Inc(i,1);
    end;
  if (i = Paramcount) then
    begin
      SetLength(CurrentParms,1);
      CurrentParms[0] := ParamStr(i);
      if not InitFileHandler(CurrentParms) then
        Halt(1)
      else
        SetLength(CurrentParms,0);
    end;
  if not fisset then
    begin
      WriteLn(StdErr,'>>  No File specified - stop');
      HelpAndHalt;
    end;
end;

function HelpHandler( var Unused: TParamStrings ): Boolean;
begin
  Result := true;
  HelpAndHalt;
end;

(*******************************************************************************
 * VMS Boot / Shutdown Code Injector
 ******************************************************************************)

var
  FStartBootCoder: array of TStartupInjector;
  FShutdownBootCoder: array of TShutdownInjector;

procedure RegisterStartupInjector( AInjector: TStartupInjector );
begin
  if not Assigned(AInjector) then
    put_internalerror(2011102310);
  SetLength(FStartBootCoder,Length(FStartBootCoder)+1);
  FStartBootCoder[High(FStartBootCoder)] := AInjector;
end;

procedure RegisterShutdownInjector( AInjector: TStartupInjector );
begin
  if not Assigned(AInjector) then
    put_internalerror(2011102311);
  SetLength(FShutdownBootCoder,Length(FShutdownBootCoder)+1);
  FShutdownBootCoder[High(FShutdownBootCoder)] := AInjector;
end;

(*******************************************************************************
 * VMS Setup / Start / Shutdown
 ******************************************************************************)

var
  OnlyCompile: Boolean;


function NoStdOutHandler( var unused: TParamStrings ): Boolean;
begin
  SetStdOut(false);
  Result := true;
end;

function OutFileHandler( var aonefile: TParamStrings ): Boolean;
begin
  if FileExists(aonefile[0]) then
    begin
      WriteLn(StdErr,'Outfile already Exists: ',aonefile[0]);
      Exit(false);
    end;
  Result := true;
  outstack_push(aonefile[0],opfm_open);
end;

function OutPathHandler( var aonepath: TParamStrings ): Boolean;
begin
  if not DirectoryExists(aonepath[0]) then
    begin
      WriteLn(StdErr,'Directory does not Exist: ',aonepath[0]);
      Exit(false);
    end;
  Result := true;
  {$NOTE no protect?}
  fpath_out_enter(aonepath[0],false);
end;

function TPLPathHandler( var aonepath: TParamStrings ): Boolean;
begin
  if not DirectoryExists(aonepath[0]) then
    begin
      WriteLn(StdErr,'Directory does not Exist: ',aonepath[0]);
      Exit(false);
    end;
  Result := true;
  fpath_rel_enter(aonepath[0],false);
end;

function LogOutHandler( var aonefile: TParamStrings ): Boolean;
begin
  if FileExists(aonefile[0]) then
    begin
      WriteLn(StdErr,'Log File already Exists: ',aonefile[0]);
      Exit(false);
    end;
  Result := true;
  set_outfile( aonefile[0] );
end;

function FailTestModeHandler( var unused: TParamStrings ): Boolean;
begin
  set_failtest_mode( true );
  Result := true;
end;

function EnableScanDebug( var unused: TParamStrings ): Boolean;
begin
  SetScanDebugMode( true );
  Result := true;
end;

function EnableCodeDebug( var unused: TParamStrings ): Boolean;
begin
  SetByteCodeDebugMode( true );
  Result := true;
end;

function WriteByteCodeHandler( var unused: TParamStrings ): Boolean;
begin
  SetByteCodeMode( true );
  Result := true;
end;

function CompileOnlyHandler( var unused: TParamStrings ): Boolean;
begin
  OnlyCompile := true;
  Result := true;
end;

var
  bootblock: TByteCodeBlock;

procedure Setup;
var i: Integer;
    tmps: PUCFS32String;
begin
  SetLength(OptTable,0);
  SetLength(FStartBootCoder,0);
  SetLength(FShutdownBootCoder,0);
  eomsg_init;
  set_currentlineinfo(-1,-1);
  set_currentfile('preboot');
  fpath_init;
  fcache_init;
  machine_init;
  outstack_init;
  {setup default commandline input}
  RegisterOpt( 'Version', 'V', 'print version and short copying notice', 0, @VersionHandler );
  RegisterOpt( 'help', 'h', 'print this', 0, @HelpHandler );
  RegisterOpt( 'file', 'f', 'specify file to process', 1, @InitFileHandler );
  RegisterOpt( 'verbosity', 'v', 'set verbosity, valid modes are'+#10+
                            ' d or debug     - lots of info'+#10+
                            ' i or info      - additional infos'+#10+
                            ' w or warning   - only errors, warnings and criticals'+#10+
                            ' e or error     - only errors and criticals'+#10+
                            ' c or critical  - only criticals'+#10+
                            ' s or silent    - be as quite as possible',
                            1, @VerbosityHandler );
  RegisterOpt( 'logfile', 'log', 'log to file rather then stderr', 1, @LogOutHandler );
  RegisterOpt( 'outfile', 'out', 'set initial output file', 1, @OutFileHandler );
  RegisterOpt( 'nostdout', 'no', 'only output into files, if no outfile is open then ignore output', 0, @NoStdOutHandler );
  RegisterOpt( 'init-outpath', 'O', 'set initial outpath', 1, @OutPathHandler );
  RegisterOpt( 'init-tplpath', 'C', 'set initial template path', 1, @TPLPathHandler );
  RegisterOpt( 'write-pct', 'pct', 'write precompiled templates', 0, @WriteByteCodeHandler );
  RegisterOpt( 'only-compile', 'oc', 'do not execute, just compile a template', 0, @CompileOnlyHandler );
  RegisterOpt( 'scan-debug', '', 'enable scandebug mode (outputs scanned tokens)',
                               0, @EnableScanDebug );
  RegisterOpt( 'pct-debug', '', 'enable pctdebug mode (outputs stabs/code after load)',
                               0, @EnableCodeDebug );
  RegisterOpt( 'failtest', '', 'enable failtest mode (exitcode=0 on critical errors, otherwise <> 0)',
                               0, @FailTestModeHandler );
  {run initers}
  for i := 0 to High(Inits) do
    Inits[i]();
  SetLength(Inits,0);
  {call and handle options from commandline}
  GetOpts;
  SetLength(OptTable,0);
  if not fisset then
    put_internalerror(201110237);
  {setup boot and shutdown code}
  bootblock.Init;
  for i := 0 to High(FStartBootCoder) do
    FStartBootCoder[i](bootblock);                    // bootcode
  SetLength(FStartBootCoder,0);
  if OnlyCompile then
    begin
      LoadTemplate(true,fname);
      ExitCode := 0;
    end
  else
    begin
      {assemble an include on stab}
      tmps := ucfs_utf8us(fname);
      if bootblock.StabAdd(tmps) < 0 then
        put_internalerror(2011123000); // put_critical('Stab Limit Exceeded');
      ucfs_release(tmps);
      if bootblock.OpcodeAdd < 0 then
        put_internalerror(2011123001);
      bootblock.image[High(bootblock.image)].SetOpcode(isc_include_stab);
      bootblock.image[High(bootblock.image)].SetOperand(High(bootblock.stab));
    end;
  for i := 0 to High(FShutdownBootCoder) do
    FShutdownBootCoder[i](bootblock);                 // shutdown code
  SetLength(FShutdownBootCoder,0);
  // append Halt, so machine knows its all done
  if bootblock.OpcodeAdd < 0 then
    put_internalerror(2011123002);
  bootblock.image[High(bootblock.image)].SetOpcode(isc_soload_true_ign);
  bootblock.image[High(bootblock.image)].SetOperand(0);
  if bootblock.OpcodeAdd < 0 then
    put_internalerror(2011123003);
  bootblock.image[High(bootblock.image)].SetOpcode(isc_halt_ign);
  bootblock.image[High(bootblock.image)].ClearOperand;
  {set lineinfo instruction based}
  for i := 0 to High(bootblock.image) do
    bootblock.image[i].SetLineInfo(i,0);
end;

procedure Run;
var bref: PCodeReference;
begin
  try
    if OnlyCompile then
      Exit;
    {boot&run machine}
    bref := fcache_addstray('post',@bootblock,false);
    templatestack_push(bref,0,false);
    repeat
      //WriteLn(StdErr,templatestack_tos^.shortname,' Decoding ',template_ip_opcode,',',template_ip_operandI);
      VMOpcodeHandler[template_ip_opcode]();
      if not machine_halted then
        begin
          if not template_ip_next then
            begin
              repeat
                templatestack_pop;
                if not ( template_ip_opcode in CTPLIncludeOpcodes ) then
                  put_internalerror(2011120556); // returned on end of template, only Includeops allow this
              until template_ip_next;
            end;
        end;
    until machine_halted;
    {check machine exit}
    if machine_aborted then
      ExitCode := machine_exitcode
    else
      ExitCode := 0;
  finally
    flush_outfile;
  end;
end;

procedure Shutdown;
begin
  SetLength(OptTable,0);
  SetLength(FStartBootCoder,0);
  SetLength(FShutdownBootCoder,0);
{$ifdef CLEANSHUTDOWN}
  ReleaseCC;
{$endif}
  outstack_fini;
  machine_fini;
  fcache_fini;
  fpath_fini;
  bootblock.Done;
  eomsg_fini;
  {check failtest}
  if failtest_mode then
    begin
      if ExitCode <> 0 then
        ExitCode := 0
      else
        ExitCode := 1;
    end;
end;

(******************************************************************************)

initialization
  OnlyCompile := false;
  SetLength(Inits,0);
  SetLength(Fins,0);
  fisset := false;
  fname := '';

finalization
  SetLength(Inits,0);
  SetLength(Fins,0);

end.

