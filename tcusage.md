# Usage and Command Line Handling #

## Command Line Structure ##


```
$tcci [options] <file>
```

On command line, 3 types of _Elements_ can be specified.

  * optional short options, starting with '-', followed by an option name and arguments
    * f.e. `-vd` where _-v_ is the option and _d_ is the argument
    * arguments may be separated, f.e. `-v d` is the same as `-vd`
  * optional long options, starting with '--', followed by an option name and separated arguments, f.e. `--file somefile` but not `--filesomefile`
  * template file as last argument

> NOTE: _command line options are case sensitive_

## Commands ##

> NOTE: _command line options are handled in order for mutual (exclusive) options_

  * `-h or --help`: show usable options
  * `-V or --Version`: show program infos (version, date of compilation, compiler version, ...)
  * `-f or --file <template>`: specify template file using argument instead of last option (allows options after file and so on)
  * `-v or --verbosity <level>`: set verbosity level for compiler and vm generated messages, level may be one of:
    * `d or debug` - generic and debug informations, warnings, errors and criticals
    * `i or info` - generic informations, warnings, errors and criticals (default)
    * `w or warning` - show warnings, errors and criticals
    * `e or error` - show errors and criticals
    * `c or critical` - show only critcals (program termination)
    * `s or silent` - show nothing (_may not work for Messages generated before this option is parsed_)
  * `-log or --logfile <log file>`: output compiler and vm generated message in `<log file>` instead of stderr
  * `-oc or --only-compile`: only compile but don't execute the template file
  * `-pct or --write-pct`: write template bytecode, this creates either `*`.pct (template) or `*`.pcu (code unit/use) which may be loaded instead of compiling
  * `--scan-debug`: output scanner infos (needs `-vdebug`)
  * `--pct-debug`: output compiled bytecode (needs `-vdebug`)
  * `--failtest`: change exitcode from <> 0 into 0 and 0 into 1 for normal operation and expected fails, unexpected fails return exitcode 2 (used for automated testing)

## Template File Path ##

A template file may be specified by:
  * Absolute name (`<Drive>:/<Path>/../<File Name>` for Windows and `/<Path>/../<File Name> for *nix`)
  * Relative name to the current working directory
  * A resource identifier of form `<service>://<Path>/../<File Name>` where `<service>` is one of
    * rel - Path and File components specify a Relative name (current working directory relative)
    * pwd - same as rel after startup
    * pkg - Path and File components specify a Relative name in any Package Path, first match will be loaded

f.e.
```
  $tcci -f "rel://somewhere/template.tpl"
```
is the same as (win)
```
  $tcci -f "somewhere\template.tpl"
or
  $tcci -f ".\somewhere\template.tpl"
```