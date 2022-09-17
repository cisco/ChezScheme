Chez Scheme is both a programming language and an implementation of
that language, with supporting tools and documentation.
This variant of Chez Scheme is extended to support the implementation
of [Racket](https://racket-lang.org/), and the main additions are
listed at the end of this README.

Supported platforms (bytecode interpreter may work for others):

 * Windows: x86, x86_64, AArch64
 * Mac OS: x86, x86_64, AArch64, PowerPC32
 * Linux: x86, x86_64, ARMv6, AArch64, RV64G, PowerPC32
 * FreeBSD: x86, x86_64, ARMv6, AArch64, PowerPC32
 * OpenBSD: x86, x86_64, ARMv6, AArch64, PowerPC32
 * NetBSD: x86, x86_64, ARMv6, AArch64, PowerPC32
 * Solaris: x86, x86_64
 * GNU/Hurd: x86
 * Android: ARMv7, AArch64
 * iOS: AArch64
 * WebAssembly via Emscripten (bytecode interpreter only)

As a superset of the language described in the
[Revised<sup>6</sup> Report on the Algorithmic Language Scheme](http://www.r6rs.org)
(R6RS), Chez Scheme supports all standard features of Scheme,
including first-class procedures, proper treatment of tail calls,
continuations, user-defined records, libraries, exceptions, and
hygienic macro expansion.

Chez Scheme also includes extensive support for interfacing with C
and other languages, support for multiple threads possibly running
on multiple cores, non-blocking I/O, and many other features.

The Chez Scheme implementation consists of a compiler, run-time
system, and programming environment.
Although an interpreter is available, all code is compiled by
default.
Source code is compiled on-the-fly when loaded from a source file
or entered via the shell.
A source file can also be precompiled into a stored binary form and
automatically recompiled when its dependencies change.
Whether compiling on the fly or precompiling, the compiler produces
optimized machine code, with some optimization across separately
compiled library boundaries.
The compiler can also be directed to perform whole-program compilation,
which does full cross-library optimization and also reduces a
program and the libraries upon which it depends to a single binary.

The run-time system interfaces with the operating system and supports,
among other things, binary and textual (Unicode) I/O, automatic
storage management (dynamic memory allocation and generational
garbage collection), library management, and exception handling.
By default, the compiler is included in the run-time system, allowing
programs to be generated and compiled at run time, and storage for
dynamically compiled code, just like any other dynamically allocated
storage, is automatically reclaimed by the garbage collector.

The programming environment includes a source-level debugger, a
mechanism for producing HTML displays of profile counts and program
"hot spots" when profiling is enabled during compilation, tools for
inspecting memory usage, and an interactive shell interface (the
expression editor, or "expeditor" for short) that supports multi-line
expression editing.

The R6RS core of the Chez Scheme language is described in
[The Scheme Programming Language](http://www.scheme.com/tspl4/),
which also includes an introduction to Scheme and a set of example programs.
Chez Scheme's additional language, run-time system, and
programming environment features are described in the
[Chez Scheme User's Guide](http://cisco.github.io/ChezScheme/csug9.5/csug.html).
The latter includes a shared index and a shared summary of forms,
with links where appropriate to the former, so it is often the best
starting point.

Get started with Chez Scheme by [Building Chez Scheme](BUILDING).

For more information about the implementation and a guide to modifying
Chez Scheme, see [implementation notes](IMPLEMENTATION.md).

For more information on Chez Scheme, see the [Chez Scheme Project Page](https://cisco.github.io/ChezScheme/).

Main additions to Chez Scheme in the Racket variant:

 * AArch64 and RV64G (RISC-V) code generation

 * Unboxed floating-point arithmetic, flvectors, and faster
   multiplication and division for large exact numbers

 * Type reconstruction during optimization (especially for safe code)

 * Continuation marks

 * Parallel garbage collection, in-place garbage collection for
   old-generation objects (instead of always copying), and
   reachability-based memory accounting

 * Ordered finalization, immobile (but collectable) objects,
   weak/ephemeron generic hash tables, and reference bytevectors

 * Portable bytecode (pb) mode, which is mainly useful for
   bootstrapping a build on any platform, but can also be used on
   platforms without native-code generation; can be compiled via
   Emscripten, linked with libffi, and/or used with bytecode partially
   compiled to C

 * Easier bootstrapping via old versions of Chez Scheme
