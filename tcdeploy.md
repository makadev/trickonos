# Compilation and Deployment #

## Compiling Trickonos ##

### Prerequisite ###

The following Software must be installed and setup correctly for compilation:
  * [FreePascal](http://www.freepascal.org) 2.4.4+
  * [GNU Make](http://www.gnu.org/software/make) 3.81 (reference version) and GNU Utilities (both may be available/packaged with FreePascal Release and Build snapshots)

### Optional ###

The following Software may be used for extended testing, development and so on (Versions from development machine setup, just as reference):
  * [Lazarus](http://wiki.lazarus.freepascal.org) 0.9.31 - IDE
  * [PasDoc](http://pasdoc.sipsolutions.net) 0.12.1 - Source Documention
  * [Valgrind](http://valgrind.org) 3.6.1 - extended leak checking

## Make Targets (Commands) ##

### Build ###

> NOTE: _creates <trickonos root>/{bin,lib}, <trickonos root>/bin/tcci{,.exe,...} is the command line interpreter_

> NOTE: _local or system global fpc settings may change used flags/switches_

  * Create debug build (using debug symbols, overflow/range error and method check, assertions, no inling, ... have a look at the root makefile)
```
$make debug
```
  * Create release build (using lineinfo [stabs, since smartlinking is deactivated for gdwarf], smart linking, optimizer level 2, inlining, disables range/overflow/io/method checking) **not recommended for unstable versions**
```
$make release
```

### Special Builds ###

  * Create debug build with heaptrc: `$make memdebug`
  * Create debug build for valgrind (_-gp -gv for profiling and memleak checks using callgrind/cachegrind/memcheck_): `$make vgdebug`
  * Create release build for valgrind (_mostly for profiling_): `$make vgrelease`

### Test Targets ###

> NOTE: _all test targets will clean, test cleans and corresponding build before testrun_

  * Create debug build and run tests: `$make test`
  * Create release build and run tests: `$make releasetest`
  * Create debug build with heaptrc and run tests (_won't stop on memleaks_): `$make memtest`
  * Create debug build for valgrind and run tests with memcheck (_slow, won't stop on memleaks_): `$make vgtest`

### Documentation Targets ###

> NOTE: _creates <trickonos root>/apidoc, where pasdoc puts the html files_

  * Create api docs pasdoc (_same as docrelease_): `$make pasdoc`
  * Create api docs pasdoc (_use release switches_): `$make docrelease`
  * Create api docs pasdoc (_use debug switches_): `$make docdebug`

### Others ###

  * Clean (_remove <trickonos root>/{bin,lib}_): `$make clean`
  * Clean All (_run clean, remove generated docs, generated test logs and so on_): `$make cleanall`

### Program Variables (and defaults) ###

  * `FPC = fpc`
  * `PASDOC = pasdoc`
  * `MKDIR = mkdir`
  * `RM = rm`
  * `VALGRIND = valgrind`

## Deployment ##

Currently, there is nothing special about Deploying/Installing the Trickonos Command Line Program tcci. Simply add it to your shell/prompt Environment and/or call it directly.

(reserved for future)