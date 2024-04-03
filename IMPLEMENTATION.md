# Implementation Overview

The majority of the Chez Scheme compiler and libraries are implemented
in Scheme and can be found in the "s" (for Scheme) subdirectory. The
run-time kernel (including the garbage collector, support for
interacting with the operating system, and some of the more
complicated math library support) are implemented in C and can be
found in the "c" directory.

Some key files in "s":

 * "cmacros.ss": object layouts and other global constants, including
   constants that are needed by both the compiler and the kernel

 * "syntax.ss": the macro expander

 * "cpnanopass.ss" and "cpprim.ss": the main compiler, where
   "cpprim.ss" is the part that inlines primitives

 * "cp0.ss", "cptypes.ss", "cpletrec.ss", etc.: source-to-source
   passes that apply before the main compiler

 * "x86_64.ss", "arm64.ss", etc.: backends that are used by
   "cpnanopass.ss" and "cpprim.ss"

 * "ppc32osx.def", "tppc32osx.def", etc., with common combinations
   produced from the "unix.def" and "tunix.def" templates: provides
   platform-specific constants that feed into "cmacros.ss" and selects
   the backend used by "cpnanopass.ss" and "cpprim.ss"

Chez Scheme is a bootstrapped compiler, meaning you need a Chez Scheme
compiler to build a Chez Scheme compiler. The compiler and makefiles
support cross-compilation, so you can work from an already supported
host to cross-compile the boot files and produce the header files for
a new platform. In particular, the `pb` (portable bytecode) machine
type can run on any supported hardware and operating system, so having
`pb` boot files is one way to get started in a new environment.

# Compiled Files and Boot Files

A Scheme file conventionally uses the suffix ".ss" and it's compiled
form uses the suffix ".so". The format of a compiled file is closely
related to the fasl format that is exposed by `fasl-write` and
`fasl-read`, but you can't compile Scheme code to some value that is
written with `fasl-write`. Instead, `compile-file` and related
functions directly generate compiled code in a fasled form that
includes needed linking information (described more below in "Linking").

A boot file, usually with the suffix ".boot", has the same format as a
compiled file, but with an extra header that identifies it as a boot
file and takes care of some singleton objects, such as `#!base-rtd`
and the stub to invoke compiled code.

The vfasl format is used for the same purposes as the fasl format, but
mostly for boot files. It is always platform-specific and its content
is very close to the form that the content will take when loaded into
memory. It can load especially quickly with streamlined linking and
interning of symbols and record types, especially in uncompressed
form. The build scripts do not convert boot files to vfasl format.

# Build System

Chez Scheme assigns a `machine-type` name to each platform it runs on.
The `machine-type` name carries three pieces of information:

 * *whether the system threaded*: `t` indicates that it is, and an
    absence indicates that it's not threaded;

 * *the hardware platform*: `i3` for x86, `a6` for x86_64, `arm32` for
   AArch32, `arm64` for AArch64, and `ppc32` for 32-bit PowerPC; and

 * *the operating system*: `le` for Linux, `nt` for Windows, `osx` for
   Mac OS, etc.

When you run "configure", it looks for boot and header files as the
directory "boot/*machine-type*". (If it doesn't find them, then
configuration cannot continue.) For information on `pb` machine types,
see "Portable Bytecode" below.

The supported machine types are listed in "cmacros.ss" and reflected
by a "boot/*machine-type*" directory for boot and headers files and a
combination of "s/*kind*.def" files to describe the platform. Files
for Unix machine types can be generated from "s/unix.def" or
"s/tunix.def" with variables configured by the "configure" script.

When you run "configure" it creates a new directory in the current
directory to hold the build. The directory where you run "configure"
is the "build directory", while the directory named "*machine-type*"
created by "configure" is the "workarea directory".

Although "configure" generates "Makefile" in the build directory, that
makefile just ensures that a local copy of `zuo` is built, and then it
runs `zuo`. The "configure" script creates "main.zuo" alongside
"Makefile", and that's what `zuo` uses by default. You can run `zuo`
directly instead of `make`, especially if you have `zuo` installed
already. When you run "configure", it stores configuration choices in
a "Mf-config" file in the workarea directory, and it creates a
"main.zuo" that reads "Mf-config" for the build configuration. So,
anything you can do with `make` or `zuo` in the place where
"configure" is run, you can also do with `zuo` in the workarea
directory. When you run "configure" with different arguments to create
different workareas, the top-level "Makefile" is replaced to point to
a different workarea each time, but existing workareas remain set up
for running `zuo`. The "main.zuo" in a workarea directory is anchored
to that directory, so the current directory doesn't have to be the
workarea directory to build there; so, for example, `zuo ta6le` would
build in the "ta6le" workarea (since `zuo` accepts a directory
argument to run "main.zuo" there) independent of where "Makefile"
currently points.

Bootstrap from scratch by using `make re.boot`, which should work even
with a relatively old version of Chez Scheme. Output is written
directly to a "boot" subdirectory.

If you have a working Chez Scheme build for the current sources and
you want to cross-compile to generate *machine-type* boot and header
files, the fastest approach is `zuo . bootquick` *machine-type*. The
output is written to the "boot/*machine-type*" directory.

# Porting to a New Platform

Porting to a new system requires both getting the C runtime compiled
on the new platform and updating the Scheme compiler to generate
machine code for the platform. There are several places where the C
kernel and code generated by the compiler need to work in harmony in
order to get the system to run. For instance, the C kernel needs to
know the type tags, sizes, and field offsets into Scheme objects, so
that the garbage collector in the C kernel can do its job. This is
handled by having the Scheme compiler generate a couple of C headers:
"scheme.h" and "equates.h", that the contain the information about the
Scheme compiler the C kernel needs to do its job.

You can port to a new operating system by imitating the files and
configuration of a similar supported operating system, but building a
new backend for a new processor requires much more understanding of
the compiler and runtime system.

Most of the work of porting to a new architecture is producing a new
"*ISA*.ss" compiler backend, and there will be a "*arch*.def" file to
go with it. For all ports, including a new operating system on an
already-supported architecture, you'll need to update "configure",
"cmacros.ss", and possibly "version.h". If the generic "unix.def"
and/or "tunix.def" templates do not work for the OS--architecture
combination, you'll need to create a new "*machine-type*.def" file or
update the way that "s/machine.zuo" synthesizes a ".def" file from
templates.

Once you have all of the pieces working together, you cross-compile
boot files, then copy them over to the the new machine to start
compiling there.

# Adding Functionality

If new functionality can be implemented in terms of existing
functionality, then you don't have to understand too much about the
compiler internals. Just write Scheme code, and put it in a reasonable
existing "s/*...*.ss" file.

The main catch is that all bindings have to be declared in
"primdata.ss". The declarations are organized by exporting library and
functions versus non-functions. Unless you're also changing a
standard, your addition will go in one of the sets that is declared
with `[libraries]`, meaning that the binding is in the `chezscheme`
library.

When a helper function needs to be in a different source file than the
place where it's used, so it can't just be locally defined, prefix the
helper function name with `$` and register it in the `[flags system
proc]` group in "primdata.ss".

There's usually not much of a bootstrapping problem with new bindings,
since you can add declarations in "primdata.ss" and implement them any
time afterward. If you get into a bad state, however, you can always
bootstrap from a relatively old Chez Scheme using `make re.boot`. In
the rare case that your new functionality is needed to compile Chez
Scheme itself, you'll have to implement a copy of the functionality
(or enough of it) in "s/reboot.ss".

Take care to implement a new functionality as safe, which means
checking arguments fully. Keep in mind that your implementation itself
will be compiled as unsafe. While testing and debugging your
additions, however, you'll probably want to use `zuo . o=0` in the
"*machine-type*/s" workarea space, which compiles in safe mode.

# Writing and Running Tests

A group of tests is written in a ".ms" file in the "mats" directory.
Within a `mat` form after the name for the group of tests, each test
is written as a expression that produces `#t` for success. Use the
`error?` form to wrap an expression that is supposed to raise an
exception in safe mode, but note that the test doesn't describe the
exception specifically, since the expected error message likely
depends on the configuration (e.g. safe versus unsafe); more on that
below.

### Running One Set of Tests (no expected-error checking)

Run tests in a ".ms" file by going to your build's
"*machine-type*/mats" directory, then `zuo .` with a ".mo" target. For
example, use `zuo . 7.mo` to build and run `7.ms`. Unless there are
failures, delete `7.mo` to run `7.ms` again.  Argument variables like
`o` control the way tests are run; for example, use `zuo . 7.mo o=3` to
test in unsafe mode. See the source file "mats/main.zuo" for
information about the configuration options.

A test failure is recorded in a ".mo" file as a line that contains
`Bug`, `Error`, or `invalid memory`. That's why the target for making
a ".mo" file ends by grepping the file. Tests for exceptions produce
the output `Expected error`, but there's not currently a way to check
that the exception tests of an individual ".ms" file produce the
expected error message.

You can make all ".mo" files with just `zuo` or `zuo . each` within
your build's "*machine-type*/mats". You can provide configuration
arguments, too, such as `zuo . o=3` to make all ".mo" files in unsafe
mode. A ".mo" file is rebuilt if configuration arguments are different
that from the previous run.

### Running Tests in One Configuration (with expected-error checking)

Use `zuo . all` to output ".mo" files to a subdirectory that is
partially configuration-specific, such as "compile-0-f-f-f" for
`compile` (as opposed to `interpret`) in safe mode (`0` instead of
`3`), without `suppress-primitive-inlining` enabled (first `f`),
without cp0 enabled (second `f`), and without
`compile-interpret-simple` enabled (last `f`).

The combination of all ".mo" error messages (from both expected
exceptions and test failures) is then compared against a list of
expected errors messages for a configuration using `diff`. The `diff`
result is written to "report-*config*", where *config* is the name of
the configuration. So, an empty "report-*config*" means success.

The set of expected error messages for a given configuration is
generated by starting with either "mats/root-experr-compile-0-f-f-f"
or "mats/root-experr-compile-3-f-f-f" (depending on whether the
configuration is in unsafe or safe mode) and then applying some number
of patches from "mats/patch-*config*". That's why the *config* in
"report-*config*" doesn't identify everything about the configuration;
it only identifies the combinations that can have different error
output.

If you add a new test that's expected to have error output (usually to
check that an exception is correctly raised), then
"mats/root-experr-*config*" and/or "mats/patch-*config*" files need to
change. Modifying those files by hand is not practical. Instead, the
strategy is to make sure that the output diff in "record-*config*" is
correct, and then use `zuo . experr` to generate new expected-error
root and patch files.

The `experr` target can only generated expected-error files based on
tests configurations that you've run. Often, it's enough to just run
`zuo . all` before `zuo . experr`, since expected errors tend to
happen only in safe mode, and they tend not to change among other
configuration options. To make sure that all combinations are covered,
see the next section on running more tests.

### Running Tests for All Configurations

The "mats/main.zuo" script has several sets of configurations
available for convenient testing, in order of increasing length:

 * `zuo . test-one`

 * `zuo . test-some-fast`

 * `zuo . test-some`

 * `zuo . test`

 * `zuo . test-more`

 * `zuo . test-experr`

As its name suggests, the `test` group is a good default set of
configurations. The `test-some` target is mostly a subset of `test`,
and `test-some-fast` further omits interpreter mode. The `test-more`
target includes combinations with slower and more aggressive checking.
The `test-experr` set includes one configuration for every combination
of options that might have different expected errors.

To run *N* configurations in parallel, supply `-j` *N* to `zuo`, as in
`zuo . -j 6 test`, or set the `ZUO_JOBS` environment variable. You can
also use the `test-some`, `test`, and `test-more` targets directly in
your build directory, since that's a shortcut for running them in the
"*machine-type*/mats" directory.

To support parallel tests, targets like `test` write output in a
collection of "output-*config*-*name*" directories within
"*machine-type*/mats", so you can look for "report-*config*" files in
those subdirectories. As its last step, a target like `test` prints a
summary of reports. As long as that output shows only configurations
(i.e., no errors), then all tests passed for all configurations.

If the summary shows only errors that reflect out-of-date expectations
from exception messages, use `zuo . experr` to update
"root-experr-..." and "patch-..." files. The updates files are written
to the source directory, so use your revision control system to make
sure the changes look right. If the only change to a patch file is to
the line-number hints, then it's probably not worth keeping the update
(as long as the line numbers are not too far off).

# Scheme Objects

A Scheme object is represented at run time by a pointer. The low bits
of the pointer indicate the general type of the object, such as "pair"
or "closure". The memory referenced by the pointer may have an
additional tag word to further refine the pointer-tag type.

See also:

> *Don't Stop the BiBOP: Flexible and Efficient Storage Management for
> Dynamically Typed Languages*
> by R. Kent Dybvig, David Eby, and Carl Bruggeman,
> Indiana University TR #400, 1994.
> [PDF](http://www.cs.indiana.edu/ftp/techreports/TR400.pdf)

For example, if "cmacros.ss" says

```scheme
  (define-constant type-pair         #b001)
```

then that means an address with only the lowest bit set among the low
three bits refers to a pair. To get the address where the pair content
is stored, round *up* to the nearest multiple 8 bytes. So, on a 64-bit
machine, add 7 to get to the `car` and add 15 to get to the `cdr`.
Since allocation on a 64-byte machine is 16-byte aligned, the
hexadecimal form of every pair pointer will end in "9".

The `type-typed-object` type,

```scheme
 (define-constant type-typed-object #b111)
```

refers to an object whose first word indicates its type. In the case
of a Scheme record, that first word will be a record-type descriptor
--- that is, a pointer to a record type, which is itself represented
as a record. The based record type, `#!base-rtd` has itself as its
record type. Since the type bits are all ones, on a 64-bit machine,
every object tagged with an additional type word will end in "F" in
hexadecimal, and adding 1 to the pointer produces the address
containing the record content (which starts with the record type, so
add 9 instead to get to the first field in the record).

As another example, a vector is represented as `type-typed-object`
pointer where the first word is a fixnum. That is, a fixnum used as a
type word indicates a vector. The fixnum value is the vector's length
in words/objects, but shifted up by 1 bit, and then the low bit is set
to 1 for an immutable vector.

Most kinds of Scheme values are represented as records, so the layout is
defined by `define-record-type` and similar. For the primitive object
types that are not records (and even a few that are), the layouts are
defined in "cmacros.ss". For example, an `exactnum` (i.e., a complex
number with exact real and imaginary components) is defined as

```scheme
 (define-primitive-structure-disps exactnum type-typed-object
   ([iptr type]
    [ptr real]
    [ptr imag]))
```

The `type-typed-object` in the first line indicates that an `exactnum`
is represented by a pointer that is tagged with `type-typed-object`,
and so we should expect the first field to be a type word. That's why
the first field above is `type`, and it turns out that it will always
contain the value `type-exactnum`. The `iptr` type for `type` means
"a pointer-sized signed integer". The `ptr` type for `real` and `imag`
means "pointer" or "Scheme object".

If you create a new type of object, then several pieces need to be
updated: the garbage collector (in "mkgc.ss" and "gc.c"), the compiler
to implement primitives that generate the kind of objects, the fasl
writer (in "fasl.ss"), the fasl reader (in "fasl.c"), the fasl reader
used by `strip-fasl-file` and `vfasl-convert-file` (in "strip.ss"),
the vfasl writer (in "vfasl.ss"), and the inspector (in "inspect.ss").

# Functions and Calls

Scheme code does not use the C stack, except to the degree that it
interacts with C functions. Instead, the Scheme continuation is a
separate, heap-allocated, linked list of stack segments. Locally, you
can just view the continuation as a stack and assume that overflow
and continuation operations are handled as needed at the boundaries.

See also:
 
> *Representing Control in the Presence of First-Class Continuations*
> by Robert Hieb, R. Kent Dybvig, and Carl Bruggeman,
> Programming Language Design and Implementation, 1990.
> [PDF](https://legacy.cs.indiana.edu/~dyb/pubs/stack.pdf)

> *Compiler and Runtime Support for Continuation Marks*
> by Matthew Flatt and R. Kent Dybvig,
> Programming Language Design and Implementation, 2020.
> [PDF](https://www.cs.utah.edu/plt/publications/pldi20-fd.pdf)

To the degree that the runtime system needs global state, that state
is in the thread context (so, it's thread-local), which we'll
abbreviate as "TC". Some machine register is designated as the `%tc`
register, and it's initialized on entry to Scheme code. For the
definition of TC, see `(define-primitive-structure-disps tc ...)` in
"cmacros.ss".

The first several fields of TC are virtual registers that may be
assigned to machine registers, in which case the TC and registers are
synced on entry and exit from Scheme code, including when calling
kernel functionality from Scheme code. In particular, the SFP (Scheme
frame pointer) virtual register must be assigned to a real register,
because it's the Scheme stack pointer. The TC and SFP registers are
the only two that absolutely must be registers, but AP (allocation
pointer) and TRAP registers are also good candidates on architectures
where plenty of registers are available.

The Scheme stack grows up in the heap, and SFP points to the beginning (i.e., the
low address) of the current stack frame. The first word of a stack
frame is the return address, so a frame looks like this:

```scheme
                ^
                |          (higher addresses)
              future
              frames
          |------------|
          |   var N    |
          |------------|
          |     ...    | ....
          |------------|
          |   var 1    | SFP[1]
          |------------|
          |  ret addr  | SFP[0]
 SFP ->   |------------|
             previous
              frames 
                |          (lower addresses)
                v
```

On entry to a Scheme function, a check ensures that the difference
between SFP and the end of the current stack segment is big enough to
accommodate the (spilled) variables of the called function, plus enough
slop to deal with some primitive operations.

A non-tail call moves SFP past all the live variables of the current
function, installs the return address as a pointer within the current
function, and then jumps to the called function. Function calls and
returns do not use machine "call" and "return" instructions;
everything is just a "jump". ("Call" and "return" instructions are
used for C interactions.) It's the caller's responsibility to reset
SFP back on return, since the caller knows how much it moved SFP
before calling.

The compiler can use a register for the return address instead of
immediately installing it in SFP[0] on a call. That mode is triggered
by giving one of the registers the name `%ret` (as described in
"Machine Registers" below). Currently, however, the called Scheme
function will immediately copy the register into SFP[0], and it will
always return by jumping to SFP[0]. So, until the compiler improves to
deal with leaf functions differently, using a return register can help
only with hand-coded leaf functions that don't immediately move the
return register into SFP[0].

There are two ways that control transitions from C to Scheme: an
initial call through `S_generic_invoke` (see "scheme.c") or via a
foreign callable. Both of those go through `S_call_help` (see
"schlib.c"). The `S_call_help` function calls `S_generic_invoke`
directly. A foreign callable is represented by generated code that
converts arguments and then calls `S_call_help` to run the Scheme
procedure that is wrapped by the callable.

The `S_call_help` function calls the hand-coded `invoke` code (see
"cpnanopass.ss"). The `invoke` code sets things up for the Scheme
world and jumps to the target Scheme function. When control returns
from the called Scheme function back to `invoke`, `invoke` finishes
not with a C return, but by calling `S_return` (see "schlib.c"), which
gets back to `S_call_help` through a longjmp. The indirect return
through longjmp helps the Scheme stack and C stack be independent,
which is part of how Scheme continuations interact with foreign
functions.

For a non-tail call in Scheme, the return address is not right after
the jump instruction for the call. Instead, the return address is a
little later, and there's some data just before that return address
that describes the calling function's stack frame. The GC needs that
information, for example, to know which part of the current Scheme
stack is populated by live variables. The data is represented by
either the `rp-header` or `rp-compact-header` (see "cmacros.ss") shape.
So, when you disassemble code generated by the Chez Scheme compiler,
you may see garbage instructions mingled with the well-formed
instructions, but the garbage will always be jumped over.

# Primitives, Library Entries, and C Entries

Chez Scheme functions are mostly implemented in Chez Scheme, but some
primitives are hand-coded within the compiler, and some primitives are
implemented or supported by C code in the kernel.

For example, the definition of `set-car!` is in "prims.ss" is

```scheme
(define set-car!
  (lambda (p v)
    (#2%set-car! p v)))
```

This turns out not to be a circular definition, because the compiler
recognizes an immediate application of the `set-car!` primitive and
inlines its implementation. The `#2%` prefix instructs the compiler to
inline the safe implementation of `set-car!`, which checks whether its
first argument is a pair. Look for `define-inline 2 set-car!` in
"cpprim.ss" for that part of the compiler. The content of "prims.ss"
is compiled in unsafe mode, so that's why safe mode needs to be
selected explicitly when needed.

What if the argument to `set-car!` is not a pair? The implementation
of inline `set-car!` in "cpprim.ss" includes

```scheme
(build-libcall #t src sexpr set-car! e-pair e-new)
```

which calls a `set-car!` *library function*. That's defined in
"library.ss" by

```scheme
(define-library-entry (set-car! x y) (pair-oops 'set-car! x))
```

That is, the `set-car!` library function always reports an error,
because that's the only reason the library function is called. Some
other library functions implement the slow path for functions where
the compiler inlines only a fast path.

Every library function has to be declared in "cmacros.ss" in the
`declare-library-entries` form. That form declares a vector of
*library entries*, which the linker uses to replace an address stub
(as inserted into machine code via `build-libcall`) with the run-time
address of the library function. The vector is filled in by loading
"library.ss". Since some library functions can refer to others, the
order is important; the linker encounters the forms of "library.ss" one
at a time, and a library entry must be registered before it is
referenced.

Some functions or other pointers, such as the thread-context mutex,
are created by the kernel in C. Those pointers are stored in an array
of *C entries* that is used by the linker. The kernel registers C
entries with `S_install_c_entry` in "prim.c". Machine code that refer
to a C entry is generated in the compiler with `(make-info-literal #f
'entry (lookup-c-entry ....) ....)`. All C entries are also declared
in "cmacros.ss" with `declare-c-entries`.

Adding a new library entry or C entry shifts indices that are
generated by the Scheme compiler. If you change the set of entries,
it's usually easiest to re-bootstrap from sources using
`make re.boot`. To avoid confusion, be sure to change the version
number first (see "Changing the Version Number" below).

Some primitives are implemented directly in the compiler but should
not be inlined. Those functions are implemented with a `$hand-coded`
form. For example, `list` is implemented in "prims.ss" as

```scheme
(define list ($hand-coded 'list-procedure))
```

Look for `list-procedure` in "cpnanopass.ss" to see the
implementation.

Finally, some primitives in "prims.ss" are implemented in the kernel
and simply accessed with `foreign-procedure`. Other parts of the
implementation also use `foreign-procedure` instead of having a
definition in "prims.ss".

If you're looking for math primitives, see "mathprims.ss" instead of
"prims.ss".

# Linking

Before generated code can be run, it must be linked with primitives,
library entries, and C entries as they exist in memory within the
current OS process. Even when code is compiled and then run in the same
OS process, linking is a separate, post-install step (by `c-mkcode` in
"compile.ss"). More typically, compiled code is written to a ".so" or
".boot" fasl file and loaded later. The fasl format is mostly a
generic serialization and deserialization format for Scheme objects,
but writing (via `c-build-fasl` in "compile.ss" plus "fasl.ss") and
fasl reading (via "fasl.c") are asymmetric for code: fasl writing
works only on unlinked code objects, while reading a fasl file produces
linked code objects by linking as it loads. (Utilities in "strip.ss"
can read and re-write file content without linking. Those tools
use a reader and writer that are completely separate from "fasl.ss" and
"fasl.c".) There's currently no support for writing linked code, as
represented by a procedure value, to a fasl stream.

Chez Scheme has its own custom linker and does not use the OS linker.
To support linking, each code object is paired with a relocation
table. Each table entry specifies an offset in the code object, the
value that should be linked at that offset, and the encoding that is
used at the offset. The value to link can be a Scheme object, such as
a bignum, symbol, or list, or an index of a library entry or C entry.
The encoding is machine-specific, and might indicate a literal word in
the code that is loaded by PC-relative addressing or a sequence of
instructions that load a value through moves and shifts. Except for
code that is moved to the "static" GC generation, the relocation table
is preserved with a code object in memory, because it is needed by the
garbage collector to relink when code and linked values are moved in
memory.

When a function directly calls another function compiled at the same
time, the a reference from one function is often directly to the code
object of another function. Predefined functions are typically
referenced by linking to a symbol, and generated code accesses the
function by looking at the function or value slot of the symbol.

# Compilation Pipeline

Compilation

 * starts with an S-expression (possibly with annotations for source
   locations),

 * converts it to a syntax object (see "syntax.ss"),

 * expands macros (see "syntax.ss") and produces an `Lsrc`
   representation in terms of core forms (see `Lsrc` in
   "base-lang.ss"),

 * performs front-end optimizations on that representation (see
   "cp0.ss", "cptypes.ss", etc.),

 * and then compiles to machine code (see "cpnanopass.ss" and
   "cpprim.ss"), which involves many individual passes that convert
   through many different intermediate forms (see "np-language.ss").

It's worth noting that Chez Scheme produces machine code directly,
instead of relying on a system-provided assembler. Chez Scheme also
implements its own linker to connect compiled code to runtime kernel
facilities and shared symbols.
 
See also:

> *Nanopass compiler infrastructure* by Dipanwita Sarkar,
> Indiana University PhD dissertation, 2008.

> *A Nanopass Framework for Commercial Compiler Development*
> by Andrew W. Keep, Indiana University PhD dissertation, 2013.

Note that the core macro expander always converts its input to the
`Lsrc` intermediate form. That intermediate form can be converted back
to an S-expression (see "uncprep.ss", whose name you should parse as
"undo-compilerpass-representation").

In the initial intermediate form, `Lsrc`, all primitive operations are
represented as calls to functions. In later passes in "cpnanopass.ss",
some primitive operations get inlined into a combination of core
forms, some of which are `inline` forms. The `inline` forms eventually
get delivered to a backend for instruction selection. For example, a
use of safe `fx+` is inlined as argument checks that guard an
`(inline + ...)`, and the `(inline + ...)` eventually becomes a
machine-level addition instruction.

# Machine Registers

Each backend file, such as "x86_64.ss" or "arm64.ss", starts with a
description of the machine's registers. It has three parts in
`define-registers`:

```scheme
(define-registers
  (reserved
    <reg>
    ...)
  (allocable
    <reg>
    ...)
  (machine-dependent
    <reg>
    ...))
```

Each `<reg>` has the form

```
    [<name> ... <preserved? / callee-saved?> <num> <type>]
```

 * The `<name>`s in one `<reg>` will all refer to the same register, and
   the first `<name>` is used as the canonical name. By convention, each
   `<name>` starts with `%`. The compiler gives specific meaning to a
   few names listed below, and a backend can use any names otherwise.

 * The information on preserved (i.e, callee-saved) registers helps
   the compiler save registers as needed before some C interactions.

 * The `<num>` value is for the private use of the backend. Typically,
   it corresponds to the register's representation within machine
   instructions.

 * The `<type>` is either `'uptr` or `'fp`, indicating whether the register
   holds a pointer/integer value (i.e., an unsigned integer that is
   the same size as a pointer) or a floating-point value. For
   `allocatable` registers, the different types of registers represent
   different allocation pools.

The `reserved` section describes registers that the compiler needs and
that will be used only for a designated purpose. The registers will
never be allocated to Scheme variables in a compiled function. The
`reserved` section must start with `%tc` and `%sfp`, and it must list
only registers with a recognized name as the canonical name.

The `machine-dependent` section describes additional registers that
also will not be allocated. They are also not saved automatically for
C interactions.

The `allocable` section describes registers that may be mapped to
specific purposes by using a recognized canonical name, but generally
these registers are allocated as needed to hold Scheme variables and
temporaries (including registers with recognized names in situations
where the recognized purpose is not needed). Registers in this
category are automatically saved as needed for C interactions.

The main recognized register names, roughly in order of usefulness as
real machine registers:

 * `%tc` - the first reserved register, must be mapped as reserved

 * `%sfp` - the second reserved register, must be mapped as reserved

 * `%ap` - allocation pointer (for fast bump allocation)

 * `%trap` - counter for when to check signals, including GC signal


 * `%eap` - end of bump-allocation region

 * `%esp` - end of current stack segment


 * `%cp` - used for a procedure about to be called

 * `%ac0` - used for argument count and call results


 * `%ac1` - various scratch and communication purposes

 * `%xp`  - ditto

 * `%yp`  - ditto

Each of the registers maps to a slot in the TC, so they are sometimes
used to communicate between compiled code and the C-implemented
kernel. For example, `S_call_help` expects the function to be called
in AC1 with the argument count in AC0 (as usual). If a recognized name
is not mapped to a register, it exists only as a TC slot.

A few more names are recognized to direct the compiler in different
ways:

 * `%ret` - use a return register instead of just SFP[0]

 * `%reify1`, `%reify2` - a kind of manual allocation of registers for
                          certain hand-coded routines, which otherwise could
                           run out of registers to use

# Variables and Register Allocation

Variables in Scheme code can be allocated either to a register or to
a location in the stack frame, and the same goes for temporaries that
are needed to evaluate subexpressions. Naturally, variables and
temporaries with non-overlapping extents can be mapped to the same
register or frame location. Currently, only variables with the same
type, integer/pointer versus floating-point, can be allocated to the
same frame location.

An early pass in the compiler converts mutable variables to
pair-valued immutable variables, but assignment to variables is still
allowed within the compiler's representation. (The early conversion of
mutable variables ensures that mutation is properly shared for, say,
variables in captured continuations.) That is, even though variables
and temporaries are typically assigned only once, the compiler's
intermediate representation is not a single-assignment form like
SSA.

Each variable or temporary will be allocated to one spot for it's
whole lifetime. So, from the register-allocation perspective, it's
better to use

```scheme
   (set! var1 ...)
   ... var1 ...
   ... code that doesn't use var1 ...
   (set! var2 ...)
   ... var2 ...
```

than to reuse var1 like

```scheme
   (set! var1 ...)
   ... var1 ...
   ... code that doesn't use var1 ...
   (set! var1 ...)
   ... var1 ...
```

Intermediate code in later passes of the compiler can also refer to
registers directly, and those uses are taken into account by the
register allocator.

Overall, the allocator sees several kinds of "variables":

 * real registers;

 * Scheme variables and temporaries as represented by `uvar`s, each of
   which is eventually allocated to a real register or to a frame
   location;

 * unspillable variables, each of which must be allocated to a real
   register; these are introduced by a backend during the
   instruction-selection pass, where an instruction may require a
   register argument; and

 * pre-colored unspillable variables, each of which must be allocated to
   a specific real register; these are introduced by a backend where
   an instruction may require an argument in a specific registers.

The difference between a pre-colored unspillable and just using the
real register is that you declare intent to the register allocator,
and it can sometimes tell you if things go wrong. For example,

```scheme
        (set! %r1 v1)
        (set! must-be-r1 v2)
        ... use %r1 and must-be-r1 ...
```

has clearly gone wrong. In contrast, the register allocator thinks
that

```scheme
        (set! %r1 v1)
        (set! %r1 v2)
        ... use %r1, sometimes expecting v1 and sometimes v2 ...
```

looks fine, and it may optimize away the first assignment. [Note:
Optimized-away assignments are one of the most confusing potential
results of register-use mistakes.]

At the point where the register allocator runs, a Scheme program has
been simplified to a sequence of assignment forms and expression
forms, where the latter are either value-producing and sit on the
right-hand side of an assignment or they have effects and sit by
themselves. The register allocator sees the first assignment to a
variable/register as the beginning of its live range and the last
reference as the end of its live range. In some cases, an instruction
is written with "dummy" arguments just to expose the fact that it
needs those arguments to stay live; for example, a jump instruction
that implements a function-call return conceptually needs to consume
the result-value registers (because those values need to stay live
through the jump), even though the machine-level jump instruction
doesn't refer to the result values. The `kill` dummy instruction can
be used with `set!` to indicate that a variable is trashed, but the
`kill` is discarded after register allocation. It's also possible for
an instruction to produce results in multiple registers. So, besides
using dummy arguments and `kill`, an instruction form can have a
`info-kill*-live*` record attached to it, which lists the `kill*`
variables that the expression effectively assigns and the `live*`
variables that the expression effectively references. (Note: a `set!`
form cannot itself have a `info-kill*-live*` record attached to it,
because the info slot for `set!` be an `info-live` record that records
computed live-variable information.)

As a first pass, the register allocator can look at an intermediate
instruction sequence and determine that there are too many live
variables, so some of them need to be spilled. The register allocator
does that before consulting the backend. So, some of the variables in
the intermediate form will stay as `uvar`s, and some will get
converted to a frame reference of them form SFP[pos]. When the backend
is then asked to select an instruction for an operation that consumes
some variables and delivers a result to some destination variable, it
may not be able to work with one or more of the arguments or
destination in SFP[pos] form; in that case, it will create an
unspillable and assign the SFP[pos] value to the unspillable, then use
the unspillable in a generated instruction sequence. Of course,
introducing unspillables may mean that some of the remaining `uvar`s
no longer fit in registers after all; when that happens, the
register allocator will discard the tentative instruction selection
and try again after spilling for `uvar`s (which will then create even
more unspillables locally, but those will have short lifetimes, so
register allocation will eventually succeed). Long story short, the
backend can assume that a `uvar` wil be replaced later by a register.

When reading the compiler's implementation, `make-tmp` in most passes
creates a `uvar` (that may eventually be spilled to a stack-frame
slot). A `make-tmp` in the instruction-selection pass, however, makes
an unspillable. In earliest passes of the compiler, new temporaries
must be bound with a `let` form (i.e., a `let` in the intermediate
representation) before they can be used; in later passes, a `set!`
initializes a temporary.

In all but the very earliest passes, an `mref` form represents a
memory reference. Typically, a memory reference consists of a
variable and an offset. The general form is two variables and an
offset, all of which are added to obtain an address, because many
machines support indexed memory references of that form. The `%zero`
pseudo-register is used as the second variable in an general `mref`
when only one variable is needed. A variable or memory reference also
has a type, 'uptr or 'fp, in the same way as a register. So, a
variable of a given type may be allocated to a register of that type,
or it may be spilled to a frame location and then referenced through
an `%sfp`-based `mref` using that type. In early passes of the
compiler, `mref`s can be nested and have computed pieces (such as
calculating the offset), but a later pass will introduce temporaries to
flatten `mref`s into just variable/register and immediate-integer
components.

A backend may introduce an unspillable to hold an `mref` value for
various reasons: because the relevant instruction supports only one
register plus an offset instead of two registers, because the offset
is too big, because the offset does not have a required alignment, and
so on.

# Instruction Selection: Compiler to Backend

For each primitive that the compiler will reference via `inline`,
there must be a `declare-primitive` in "np-language.ss". Each
primitive is either an `effect`, a `value` that must be used on the
right-hand side of a `set!` or a `pred` that must be used immediately
in the test position of an `if` --- where `set!` and `if` here refer
to forms in the input intermediate language of the
instruction-selection compiler pass (see `L15c` in "np-languages.ss").
Most primitives potentially correspond to a single machine
instruction, but any of them can expand to any number of instructions.

The `declare-primitive` form binds the name formed by adding a `%`
prefix. So, for example,

```scheme
  (declare-primitive logand value #t)
```

binds `%logand`. The `(%inline name ,arg ...)` macro expands to
`(inline ,null-info ,%name ,arg ...)` macro, so that's why you don't
usually see the `%` written out.

The backend implementation of a primitive is a function that takes as
many arguments as the `inline` form, plus an additional initial
argument for the destination in the case of a `value` primitive on the
right-hand side of a `set!`. The result of the primitive function is a
list of instructions, where an instruction is either a `set!` or `asm`
form in the output intermediate representation of the
instruction-selection pass (see `L15d` in "np-languages.ss"). The
`asm` form in the output language has a function that represents the
instruction; that function again takes the arguments of the `asm`
form, plus an extra leading argument for the destination if it's on
the right-hand side of a `set!` (plus an argument before that for the
machine-code sequence following the instruction, and it returns an
extended machine-code sequence; that is, a machine-code sequence is
built end-to-start).

An instruction procedure typically has a name prefixed with `asm-`.
So, for example, the `%logand` primitive's implementation in the
backend may produces a result that includes a reference to an
`asm-logand` instruction procedure. Or maybe the machine instruction
for logical "and" has a variant that sets condition codes and one that
doesn't, and they're both useful, so `asm-logand` actually takes a
curried boolean to pick; in that case, `%logand` returns an
instruction with `(asm-logand #f)`, which produces a function that
takes the destination and `asm` arguments. Whether an argument to
`asm-logand` is suitable for currying or inclusion as an `asm`
argument depends on whether it makes sense in the `asm` grammar and
whether it needs to be exposed for register allocation.

The compiler may refer to some instructions directly. Of particular
importance are `asm-move` and `asm-fpmove`, which are eventually used
for `set!` forms after the instruction-selection pass. That is, the
output of instruction selection still uses `set!`, and then those are
converted to memory and register-moving instructions later. The
instruction-selection pass must ensure that any surviving `set!`s are
simple enough, though, to map to instructions without further register
allocation. In other words, the backend instruction selector should
only return `set!`s as instructions when they are simple enough, and
it should generate code to simplify the ones that don't start out
simple enough. To give the backend control over `set!`s in the *input*
of instruction selection, those are send to the backend as `%move` and
`%fpmove` primitives (which may simply turn back into `set!s` using
the output language, or they may get simplified). When the compiler
generates additional `set!`s after instruction selection, it generates
only constrained forms, where target or source `mref`s have a single
register and a small, aligned offset.

To organize all of this work, a backend implementation like
"x86_64.ss" or "arm64.ss" must be organized into three parts, which
are implemented by three S-expressions:

 * `define-registers`

 * a module that implements primitives (that convert to instructions),
   installing them with `primitive-handler-set!`

 * a module that implements instructions (that convert to machine
   code), a.k.a. the "assembler", defining the instructions as
   functions

That last module must also implement a few things that apply earlier
than assembling (or even instruction selection), notably including
`asm-foreign-call` and `asm-foreign-callable`. For more on those two,
see "Foreign Function ABI" below.

To summarize the interface between the compiler and backend is:

 * `primitive : L15c.Triv ... -> (listof L15d.Effect)`

 * `instruction : (listof code) L16.Triv ... -> (listof code)`

A `code` is mostly bytes to be emitted, but it also contains
relocation entries and human-readable forms that are printed when
assembly printing is enabled. The `aop-cons*` helper macro (in
"cpnanopass.ss") is like `cons*`, but it skips its first argument if
human-readable forms aren't being kept.

# Instruction Selection: Backend Structure

To further organize the work of instruction selection and assembly,
all of the current backends use a particular internal structure:

 * primitives are defined through a `define-instruction` form that
   helps with pattern matching and automatic conversion/simplification
   of arguments; and

 * instructions are defined as functions that use an `emit` form,
   which in turn dispatches to function that represent actual
   machine-level operations, where the functions for machine-level
   operations typically have names ending in `-op`.

Consider the "arm64.ss" definition of `%logand`, which should accept a
destination (here called "z") and two arguments:

```scheme
  (define-instruction value (logand)
    [(op (z ur) (x ur) (y funkymask))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-logand #f) ,x ,y))]
    [(op (z ur) (x funkymask) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-logand #f) ,y ,x))]
    [(op (z ur) (x ur) (y ur))
     `(set! ,(make-live-info) ,z (asm ,info ,(asm-logand #f) ,x ,y))])
```

The A64 instruction set supports a logical "and" on either two
registers or a register and an immediate, but the immediate value has
to be representable with a funky encoding. The pattern forms above
require that the destination is always a register/variable, and either
of the arguments can be a literal that fits into the funky encoding or
a register/variable. The `define-instruction` macro is parameterized
over patterns like `funkymask` via `coercible?` and `coerce-opnd`
macros, so a backend like "arm64.ss" can support specialized patterns
like `funkymask`.

If a call to this `%logand` function is triggered by a form

```scheme
  `(set! ,info (mref ,var1 ,%zero 8) ,var2 ,7)
```

then the code generated by `define-instruction` will notice that the
first argument is not a register/variable, while 7 does encode as a
mask, so it will arrange to produce the same value as

```
   (let ([u (make-tmp 'u)])
      (list
        (%logand u var2 7)
        `(set! ,(make-live-info) (mref ,var1 ,%zero 8) ,u)))
```

Then, the first case of `%logand` will match, and the result will be
the same as

```
   (let ([u (make-tmp 'u)])
      (list
        `(set! ,(make-live-info) ,u (asm,(asm-logand #f) ,var2 ,7)
        `(set! ,(make-live-info) (mref ,var1 ,%zero 8) ,u))))
```

If the offset 8 were instead a very large number, then auto-conversion
would have to generate an `add` into a second temporary variable.
Otherwise, `asm-move` would not be able to deal with the generated
`set!` to move `u` into the destination. The implementation of
`define-instruction` uses a `mem->mem` helper function to simplify
`mref`s. There's an additional `fpmem` pattern and `fpmem->fpmem`
helper, because the constraints on memory references for
floating-point operations can be different than than the constraints
on memory references to load an integer/pointer (e.g., on "arm32.ss").

Note that `%logand` generates a use of the same `(asm-logand #f)`
instruction for the register--register and the register--immediate
cases. A more explicit distinction could be made in the output of
instruction selection, but delaying the choice is analogous to how
assembly languages often use the same mnemonic for related
instructions. The `asm-move` and `asm-fpmove` must accommodate
register--memory, memory--register, and register--register cases,
because `set!` forms after instruction selection can have those
variants.

The `asm-logand` instruction for "arm64.ss" is implemented as

```
   (lambda (set-cc?)
     (lambda (code* dest src0 src1)
       (Trivit (dest src0 src1)
          (record-case src1
            [(imm) (n) (emit andi set-cc? dest src0 n code*)]
            [else (emit and set-cc? and src0 src1 code*)]))))
```

The `set-cc?` argument corresponds to the `#f` in `(asm-logand #f)`.
The inner lambda represents the instruction --- that is, it's the
function in an `asm` form. The function takes `code*` first, which is
a list of machine codes for all instructions after the `asm-logand`.
The `dest` argument corresponds to the result register, and `src0` and
`src1` are the two arguments.

The `Trivit` form is a bridge between intermediate languages. It takes
variables that are bound already and it rebinds them for the body of
the `Trivit` form. Each rebinding translate the argument from an `L16`
`Triv` record to a list that starts 'reg, 'disp, 'index, 'literal, or
'literal@. (Beware of missing this step, and beware of backends that
sometimes intentionally skip this step because the original is known
to be, say, a register.)

The `emit` form is defined in the "arm64.ss" backend and others, and
it's just a kind of function call that cooperates with `define-op`
declarations. For example, `(define-op andi logical-op arg1 ...)`
binds `andi-op`, and `(emit andi arg2 ...)` turns into `(logical-op
'and arg1 ... arg2 ...)`; that is, `andi-op` first receives the symbol
'andi, then arguments listed at `define-op`, then arguments listed at
`emit`. The last argument is conventionally `code*`, which is the code
list to be extended with new code at its beginning (because the
machine-code list is built end to start). The bounce from `andi-op` to
`logical-op` is because many instructions follow a similar encoding,
such as different bitwise-logical operations like `and` and `or`.
Meanwhile, `logical-op` uses an `emit-code` form, which is also in
"arm64.ss" and other backends, that calls `aop-cons` with a suitable
human-readable addition.

All of that could be done with just plain functions, but the macros
help with boilerplate and arrange some helpful compile-time checking.

# Directives for Linking

Besides actual machine code in the output of the assembly step,
machine-specific linking directives can appear. In the case of
"arm32.ss", the linking options are `arm32-abs` (load an absolute
address), `arm32-call` (call an absolute address while setting the link
register), and a`arm32-jump` (jump to an absolute address). These are
turned into relocation entries associated with compiled code by steps
in "compile.ss". Relocation entries are used when loading and GCing
with update routines implemented in "fasl.c" as described above in
"Linking".

Typically, a linking directive is written just after some code that is
generated as installing a dummy value, and then the update routine in
"fasl.c" writes the non-dummy value when it becomes available later.
Each linking directive must be handled in "compile.ss", and it must
know the position and size of the code (relative to the direction) to
be updated. Overall, there's a close conspiracy among the backend, the
handling in "compile.ss", and the update routine in "fasl.c".

# Foreign Function ABI

Support for foreign procedures and callables in Chez Scheme boils down
to foreign calls and callable stubs for the backend. A backend's
`asm-foreign-call` and `asm-foreign-callable` function receives an
`info-foreign` record, which describes the argument and result types
in relatively primitive forms:

 * double
 * float
 * [signed] integer of {8,16,32,64} bits
 * generic pointer or scheme-object (to treat as a generic pointer)
 * a "&" form, which is a pointer on the Scheme side and by-value on
   the C side, and can be a struct/union; layout info is reported
   by `$ftd-...` helpers

If the result type is a "&" type, then the function expects an extra
first argument on the Scheme side. That extra argument is reflected by
an extra pointer type at the start of the argument list, but the "&"
type is also left for the result type as an indication about that
first argument. In other words, the result type is effectively
duplicated in the result (matching the C view) and an argument
(matching the Scheme view) --- so, overall, the given type matches
neither the C nor Scheme view, but either view can be reconstructed.

The compiler creates wrappers to take care of further conversion
to/from these primitive shapes. You can safely ignore the
foreign-callable support, at first, when porting to a new platform,
but foreign-callable support is needed for generated code to access
runtime kernel functionality.

The `asm-foreign-call` function returns 5 values:

 * `allocate : -> L13.Effect`

   Any needed setup, such as allocating C stack space for arguments.

 * `c-args : (listof (uvar/reg -> L13.Effect))`

   Generate code to convert each argument. The generated code will be
   in reverse order, with the first argument last, because that tends
   to improve register allocation.

   If the result type is "&", then `c-arg`s must include a function to
   accept the pointer that receives the function result (i.e., the
   length of `c-args` should match the length of the argument-type list
   in the given `info-foreign`). The pointer may need to be stashed
   somewhere by the generated code for use after the function returns.

   The use of the src variable for an argument depends on its type:

     - double or float: an 'fp-typed variable
     - integer or pointer: a 'uptr-typed variable that has the integer
     - "&": a 'uptr-typed variable that has a pointer to the argument

 * `c-call : uvar/reg boolean -> L13.Effect`

   Generate code to call the C function whose address is in the given
   register. The boolean if #t if the call can assume that the C
   function is not a varargs function on platforms where varargs
   support is the default.

 * `c-result : uvar/reg -> L13.Effect`

   Similar to the conversions in `c-args`, but for the result, so the
   given argument is a destination variable. This function will not be
   used if the foreign call's result type is void. If the result if a
   floating-point value, the provided destination variable has type
   'fp.

 * `deallocate : -> L13.Effect`

   Any needed teardown, such as deallocating C stack space.

The `asm-foreign-callable` function returns 4 values:

 * `c-init : -> L13.Effect`

   Anything that needs to be done just before transitioning into
   Scheme, such as saving preserved registers that call be used within
   the callable stub.

 * `c-args : (listof (uvar/reg -> L13.Effect))`

   Similar to the `asm-foreign-call` result case, but each function
   should fill a destination variable form platform-specific argument
   registers and stack locations.

   If the result type is "&", then `c-arg`s must include a function to
   produce a pointer that receives the function result. Space for this
   pointer may needed to be allocated (probably on the C stack),
   possibly in a way that can be found on return.

   The use of the destination variable is different than for the
   `asm-foreign-call` in the case of floating-point arguments:

     - double or float: pointer to a flonum to be filled with the value
     - integer or pointer: a 'uptr-typed variable to receive the value
     - "&": a 'uptr-typed variable to receive the pointer

 * `c-result : (uvar/reg -> L13.Effect) or (-> L13.Effect)`

   Similar to the `asm-foreign-call` argument cases, but for a
   floating-point result, the given result register holds pointer to a
   flonum. Also, if the function result is a "&" or void type, then
   `c-result` takes no argument (because the destination pointer was
   already produced or there's no result).

 * `c-return : (-> L13.Effect)`

   Generate the code for a C return, including any teardown needed to
   balance `c-init`.

# Cross Compilation and Compile-Time Constants

When cross compiling, there are two notions of quantities/properties
like the size of pointers or endianness: the host notion and the
target platform's notion. A function like `(native-endianness)` always
reports the host's notion. A constant like `(constant
native-endianness)` refers to the target machine notion.

Cross compilation works by starting with a Chez Scheme that runs on
the host machine and then re-compiling a subset of the Chez Scheme
implementation to run on the host machine but with `constant` values
suitable for the target machine. The recompiled parts are assembled
into an `xpatch` file that can be loaded to replace functions like
`compile-file` and `vfasl-convert-file` with ones that use the
target-machine constants. Loading an `xpatch` file tends to make
compilation or fasl operations for the host machine inaccessible, so a
given Chez Scheme process is only good for targeting one particular
platform.

When working on the compiler or fasl-related tools, take care to use
the right notion of a quantity or property. If you need the host
value, then there must be some function that provides the value. If
you need the target machine's value, then it must be accessed using
`constant`.

# Portable Bytecode

The "portable bytecode" virtual machine uses a 32-bit instruction set
that is interpreted by a loop defined in "c/pb.c", where many of the
instruction implementations are in "c/pb.h". The instruction set is
custom, but inspired by Arm64. Of course, since the instructions are
interpreted, it does not run nearly as fast a native code that Chez
Scheme normally generates, but it runs fast enough to be useful for
bootstrapping a Chez Scheme build from one portable set of boot files.
The pb machine type is also potentially useful in a setting that
disallows code generation or where there's not yet a machine-code
backend for Chez Scheme.

A `machine-type` name for a pb build follows a variant of the normal
conventions:

 * *whether the system threaded*: `t` indicates that it is threaded;

 * `pb`;

 * *word side*: `64`, `32`, or blank for basic; and

 * *endianness*: `l` for little-endian, `b` for big-endian, or blank
    for basic.

Compiled files (including boot files) for a basic pb build work on all
platforms, while compiled files for a non-basic pb build have a
specific word size and endianness for improved performance. Run
"configure" with `--pb` for a basic build, or run "configure" with
`--pbarch` or `-m=<pb-machine-type>` for a non-basic build.

A basic build can work on all platforms because it assumes a 64-bit
representation of Scheme values. On a 32-bit platform, the kernel is
compiled to use a 64-bit integer type for `ptr`, even though the high
half of a `ptr` value will always be zeros. The `TO_VOIDP` and
`TO_PTR` macros used in the kernel tell a C compiler that conversions
between 64-bit `ptr`s and (potentially) 32-bit pointers are
intentional. A basic build also avoids a compile-time assumption of
endianness, turning any such Scheme-level decisions into a run-time
branch. Bytecode instructions are stored as little endian in compiled
code for a basic build; on a big-endian machine, the kernel rewrites
instruction bytes to big-endian form while loading a fasl file, so the
interpreter can decode instructions in native order.

A basic build supports only a limited, hardwired set of foreign
interfaces that are sufficient to access kernel functions. A non-basic
build can support the full foreign interface if the Scheme build is
configured to use libffi. The pb32 variants assume 8-byte alignment in
structs for doubles and 64-bit integer values, which can limit
interoperability with foreign libraries on platforms with a different
alignment convention (such as non-Windows x86, where doubles and
64-bit integers need only 4-byte alignment).

For a non-basic build, fragments of static Scheme code can be turned
into C code to compile and plug back into the kernel. These fragments
are called *pbchunks*.

### pbchunk Builds

The `pbchunk-convert-file` function takes compiled Scheme code (as a
boot or fasl file), generates C code for the chunks, and generates
revised compiled code that contains references to the chunks via
`pb-chunk` instructions. Calling the registration function in the
generated C code registers chunks with the kernel as targets for
`pb-chunk` instructions. Each chunk has a static index, so the revised
compiled Scheme code must be used with exactly the C chunks that are
generated at the same time; when multiple sets of chunks are used
together, each needs to be created with non-overlapping index ranges.

Using

```bash
zuo . bootpbchunk <machine-type>-<tag>
```

creates a "boot/*machine-type*-*tag*" directory that contains adjusted
versions of the boot files in "boot/*machine-type*" plus C code to
implement chunks extracted from the boot files. For example,

```bash
zuo . bootpbchunk tpb64l-pbchunk
```

creates pbchunked boot files for the 64-bit, little-endian pb variant.

If the current machine-type does not match *machine-type*, the
`bootpbchunk` target expects to be able to use a cross compiler, so
create one if needed using

```bash
zuo . bootquick <machine-type>
```

The `bootpbchunk` target recognizes additional arguments to specify
additional boot files. Start with `--petite` to extract pbchunks only
from "petite.boot" (and not the compiler in "scheme.boot"), or start
with `--only` to extract pbchunks only from additional supplied boot
files. For example,

```bash
zuo . bootpbchunk tpb64l-demo --only demo.boot
```

extracts chunks only from "demo.boot" and includes the updated
"demo.boot" alongside the "petite.boot" and "scheme.boot" boot files
in "boot/tpb64l-demo".

To build with the assembled pbchunk configuration, use

```bash
./configure --boot=<machine-type>-<tag> --pbarch
```

which configures a build using prepared "boot/*machine-type*-*tag*"
files. A build configured this way supports only `zuo` for the kernel
build and `zuo . run` to run (the latter assuming that the target
build matches the host platform). A plain `zuo` will not attempt to
rebuild Scheme sources that are part of Chez Scheme.

In the special case of using "boot/*machine-type*-*tag*" to target
WebAssembly via Emscripten, a boot file added via `ARGS` will be
included as a preload automatically and should not be listed again
later with `--emboot`.

### Internal pbchunk Protocol

A `pb-chunk` instruction's payload is two integers: a 16-bit *index*
and an 8-bit *subindex*. The *index* selects a registered C chunk
function. The *subindex* is passed as the third argument to that
function. Meanwhile, the first two arguments to the chunk C function
are the machine state *ms* that lives in a thread context and the
address *ip* of the `pb-chunk` instruction. The pb virtual registers
are accessed via *ms*. The *ip* argument is useful for constructing
relative addresses, such as the address of code that contains a
relocatable reference. A C chunk function returns the address of pb
code to jump to. A chunk function might return an address of Scheme
function code to call that function, or it might return the address of
code to go back to running in interpreted mode for the same code
object where it started; that is, general jumps and bailing out of
chunk mode are implemented in the same way.

# Changing the Version Number

To change the version number, edit the `version` definition in
"cmacros.ss", and re-bootstrap from scratch using `make re.boot`.

To update the "boot/pb" files that are normally used to build Chez
Scheme without an existing Chez Scheme, use `./configure --pb` before
running `make re.boot`.
