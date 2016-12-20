#Building Chez Scheme Version 9.4
Copyright 1984-2016 Cisco Systems, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

----------------------------------------------------------------------------

This directory contains the sources for Chez Scheme, plus boot and header
files for various supported machine types.

BASICS

Building and installing Chez Scheme on a recent version of Linux or OS X
is typically as simple as installing the prerequisites listed below and
running (Windows build instructions appear under the heading WINDOWS
later in this file):
```
./configure
sudo make install
```
This should not take more than a minute or so, after which the commands
'scheme' and 'petite' can be used to run Chez Scheme and Petite Chez
Scheme, while 'man scheme' and 'man petite' can be used to view the
manual pages.  Chez Scheme and Petite Chez Scheme are terminal-based
programs, not GUIs.  They both incorporate sophisticated command-line
editing reminiscent of tcsh but with support for expressions that span
multiple lines.

Prerequisites:

* GNU Make
* gcc
* Header files and libraries for ncurses
* Header files and libraries for X windows

DETAILS

The sources for Chez Scheme come in two parts:

* A set of Scheme source files found in the subdirectory s.  Compiling
  these produces the boot files petite.boot and scheme.boot, along with
  two header files, equates.h and scheme.h.

* A set of C source files found in the subdirectory c.  Compiling and
  linking these files produces the executable scheme (scheme.exe under
  Windows).  Compiling the C sources requires the two header files
  produced by compiling the Scheme sources.

Since the Scheme sources can be compiled only by a working version of
Chez Scheme, it's not actually possible to build Chez Scheme from source.
That's why the boot and header files are packaged with the sources.

'./configure' attempts to determine what type of machine it's on and,
if successful, creates several files and directories:

* The directory nanopass containing the Nanopass Infrastructure,
  retrieved from github.

* A make file, Makefile, in the root (top level) directory.

* A "workarea", or subdirectory named for the machine type (e.g.,
  a6le for nonthreaded 64-bit linux).  The workarea is a mirror of
  the root directory, with subdirectories named c, s, and so on.
  Compilation takes place in the workarea.

* Within the workarea, the files Makefile, Mf-install, and Mf-boot.

'./configure' supports various options, among which is --help, which
can be used to list the supported options.

The make file supports several targets:

'make' or 'make build'
  compiles and links the C sources to produce the executable, then
  bootstraps the Scheme sources.  Bootstrapping involves using the
  freshly built scheme executable along with the distributed boot files
  to compile the Scheme sources.  If the new boot files are equivalent
  to the old boot files, the system is bootstrapped.  Otherwise, the new
  boot files are used to create a newer set, and those are compared.
  If this succeeds, the system is bootstrapped.  Otherwise, the make
  fails.  This should not fail unless the distributed boot files are
  out of sync with the sources.

  To run Chez Scheme without installing, you need to tell the executable
  where to find the boot files.  This can be done via command-line
  arguments, e.g.:

    $W/bin/$M/scheme -b $W/boot/$M/petite.boot -b $W/boot/$M/scheme.boot

  or by setting the SCHEMEHEAPDIRS variable to point to the directory
  containing the boot files.  For example, in bash:

    SCHEMEHEAPDIRS=$W/boot/$M $W/bin/$M/scheme

  and in tcsh:

    setenv SCHEMEHEAPDIRS "$W/boot/$M"
    $W/bin/$M/scheme

  In all cases, $W should be replaced with the name of the workarea,
  and $M should be replaced with the machine type.  (Unless the default
  is overridden via an argument to ./configure, $W is the same as $M.)

'sudo make install'
  runs the build plus installs the resulting executable, boot files,
  example files, and manual pages.

'make test'
  runs the build plus runs a set of test programs in various different
  ways, e.g., with different compiler options.  It can take 30 minutes
  or more, depending on the speed of the machine.  It produces voluminous
  output, so it's best to redirect its stdout and stderr to a file.

  NB: A complete run does not imply no errors occurred.  To check for
  errors, look at the file $W/mats/summary, where $W is the name of the
  workarea created by ./configure.  $W/mats/summary should contain one
  line per test run, something like this:

  -------- o=0 --------
  -------- o=3 --------
  -------- o=0 cp0=t --------
  -------- o=3 cp0=t --------
  -------- o=0 spi=t p=t --------
  -------- o=3 spi=t p=t --------
  -------- o=0 eval=interpret --------
  -------- o=3 eval=interpret --------
  -------- o=0 cp0=t eval=interpret --------
  -------- o=3 cp0=t eval=interpret --------
  -------- o=0 ehc=t eoc=f --------
  -------- o=3 ehc=t eval=interpret --------

  If there is anything else in $W/mats/summary, something unexpected
  occurred.

'make bootfiles'
  runs the build plus uses the locally built system to recreate the
  distributed boot and header files for each supported machine type.
  It should be run whenever modifications made to the Scheme sources
  are to be committed to the source-code repository so that up-to-date
  boot and header files can be committed as well.  'make bootfiles'
  can take 5 minutes or more.

  'make bootfiles' builds boot files for each machine type for which
  a subdirectory exists in the top-level boot directory.  to build
  for a supported machine type that isn't built by default, simply
  add the appropriate subdirectory, i.e., 'mkdir boot/$M', where M
  is the machine type, before running 'make bootfiles'.  You can
  also run '(cd $W ; make -f Mf-boot $M.boot)', where W is the name
  of a built work area for the host machine type, to build just the
  boot files for machine-type M.

'make clean'
  removes binaries from the workarea.

'make distclean'
  removes nanopass, Makefile, and all workareas.

WINDOWS

Building Chez Scheme under Windows is currently more complicated than it
should be.  It requires the configure script (and through it, the workarea
script) to be run on a host system that supports a compatible shell,
e.g., bash, and the various command-line tools employed by configure
and workarea, e.g., sed and ln.  For example, the host system could be a
Linux or MacOS X machine.  The release directory must be made available
on a shared filesystem, e.g., samba, to a build machine running Windows.
It is not presently possible to copy the release directory to a Windows
filesystem due to the use of symbolic links.

Prerequisite:

The Microsoft Visual C compiler (express or full) and associated
tools must be available on the Windows build machine, and the PATH,
INCLUDE, and LIB variables must be set up properly for those tools.

Between the configure and build steps (below), the 32-bit or 64-bit
(as appropriate) C runtime library vcruntime$V.dll must be placed into
$W/bin/$M with a symbolic link to or copy of the file in $W/bin, where
$V is the expected C runtime version for the installed Visual C compiler
(e.g., 140 for Visual Studio 2015), $W is the workarea name, and $M is
the machine type.

Configure step (host machine):

To configure the build, run:
```
./configure -m=$M
```
where $M is replaced with one of the Windows machine types, i.e.,
i3nt for 32-bit, a6nt for 64-bit, ti3nt for threaded 32-bit, and
ta6nt for 64-bit threaded.

Build step (Windows build machine):

To build the executable $W\bin\$M\scheme.exe, run:
```
nmake -f Makefile.i3nt 
```
in the directory $W\c, then recompile the Scheme sources via:

bldnt $M

in the directory $W/c, where $W is replaced by the name of the
workarea created by ./configure, and $M is the machine type.

To run Chez Scheme or Petite Chez Scheme from a terminal window,
set ```PATH``` and ```SCHEMEHEAPDIRS```:
```
set PATH=$W\bin\$M;%PATH%
set SCHEMEHEAPDIRS=$W/boot/$M
```
again with $W and $M replaced with the workarea name and machine
type, and start Chez Scheme with the command "scheme" or Petite
Chez with the command "petite".

Chez Scheme and Petite Chez Scheme are terminal-based programs,
not GUIs.  They both incorporate sophisticated command-line editing
reminiscent of tcsh but with support for expressions that span
multiple lines.

Testing under Windows

The iconv tests in mats/io.ms require that a 32-bit or 64-bit (as
appropriate) libiconv-2.dll implementing GNU libiconv be located in
$W/bin/$M with a symbolic link to or copy of the file in $W/bin, where
$W and $M are the workarea name and machine type.  Windows sources for
libiconv can be found at:

http://gnuwin32.sourceforge.net/packages/libiconv.htm

If the dlls are not present, the iconv tests will fail.  No other
tests should be affected.

The tests can be run with a default set of options at a single
optimize level by running the command:

bldnt $M $O

in $W/mats, with $W and $M replaced with the workarea name and
machine type and $O replaced by an optimize level, e.g., 0 or 3.
This produces a set of output (.mo) files in the directory
$W/mats/output-compile-$O-f-f-f.  "make fastreport o=$O" can be
then used to generate a report; this step must be run on a host
system supporting GNU make and the various tools required by the
fastreport target, e.g., grep.  The resulting report file
$W/mats/report-compile-$O-f-f-f should be empty if all tests
succeeded.
