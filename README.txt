This file is Part of the Trickonos Project.
Copyright (C) 2011-2012  Matthias Karbe

--

This is the file README, it gives an overview of the licenses used in this
project, contributor notes, build instructions and additional information.

0. License

  Copyright (C) 2011-2012 Matthias Karbe

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


1.0. Building and Running Trickonos

  !! Trickonos is currently in an early state of development. 
     A lot of core functionality is not implemented or may be weak/broken. !!
  
  DONT INSTALL THIS SOFTWARE IN PRODUCTIVE AND UNPROTECTED ENVIRONMENTS.

1.1. Required and additional Software

  Trickonos comes with (GNU make) makefiles for building and testing, editor
  informations for the Lazarus-IDE, build Instructions for building with
  Freepascal 2.4.4 (or later) and Pasdoc source code documentation.
  
  GNU make and GNU coreutils, utilised by said makefiles, are either
  Part of the FPC binary distribution or can be installed. 
  (see your package manager, query your distributor and/or see 
   http://www.gnu.org/s/make/ and http://www.gnu.org/s/coreutils/ 
   for more Information).
  
  Trickonos is a Program written in Freepascal's Object Pascal dialect, those
  the Freepascal Compiler (FPC), RTL and possibly Packages are needed for 
  building. Freepascal can be aquired from www.freepascal.org, or bundled
  with Lazarus-IDE.
  
  Lazarus-IDE (http://sourceforge.net/projects/lazarus/) may be used for
  development, thought not needed at all for building Trickonos,
  it was mostly developed using Lazarus-IDE and its features. 
  The source root (src/) contains a Lazarus Project File and 
  Project Information which can be used (it uses gnu make for building, 
  not FPC directly!).
  
  A lot of API and Datastructure (Developers) documentation was placed
  in code with the "{DOC>>" Tag. 
  Pasdoc (http://sourceforge.net/projects/pasdoc/) can be used to create HTML
  documentations using these Tagged Source Documentations. Be aware that
  some of these documentations may be out of date, incomplete, 
  non-existing, wrong and a nightmare in spelling/grammar/...

  The following GNU Make commands can be run in the Trickonos root. 
  Additionally to GNU Make (and GNU Core Utils, or compatible replacement)
  Building and Testing needs FPC, API Doc. creation needs PASDOC.
  Binary Pathes may be overriden by specifiyng
  FPC=<fpc binary>, MAKE=<gnu make binary>, PASDOC=<pasdoc binary> on
  the commandline or adding the binaries to the Environment used by GNU Make.
  
1.2. Building Release

  $make release
  The binary will be placed in <trickonos>/bin

1.3. Building Debug 

  $make debug
  The binary will be placed in <trickonos>/bin

1.4. Testing

  $make test
  This will first cleanall, build the debug Version and run the 
  test specified in <trickonos>/test

1.5. Creating Pasdoc source code Information

  $make pasdoc
  This will create <trickonos>/apidoc where index.html is the main Page.

1.6. Cleaning

  $make clean
  This will delete binaries and libs
  
  $make cleanall
  This will delete binaries, libs and generated files like documentation and so
  on.


2.0 Contributor Agreement

  You are free to Contribute to this Project. Simply send a Patchset 
  (With Information about the Patch and Instructions on how to 
   Apply this Patchset) using one of the designated mechanisms for 
  Non-Trival Contributions.
  
  With Contribution to this Project you agree to the following Terms,
  which allow the Project as such to retain free.

    * Your contibutions are free of patent violations or copyright violations, 
      to the best of your knowledge
    * Your contibutions are released under the project's license;
      or are public domain (f.e. build scripts, makefiles, tests, and so on)
    * Your contributions may be relicensed under official future versions of 
      the GPL and/or Forked according to the Terms of the GPL, 
      without your explicit approval
 
  Feel free to add your Name and/or Contact Information, the Date and
  a short Notice about your Contribution(s) in CREDITS.txt.
  Please keep this file clean and readable.

