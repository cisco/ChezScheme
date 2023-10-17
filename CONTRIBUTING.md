# Contributing to Chez Scheme

Chez Scheme is a work in progress, and we invite contributions from
anyone who is interested in putting forth the necessary effort.
One or more of the committers will review pull requests for
compatibility with the principles and guidance given below.  Details
on pull-request processing are given in [the governance
document](CHARTER.md).

Our core principles are pretty simple: we try to make Chez Scheme
reliable and efficient.

Reliability means behaving as designed and documented.  We have no
"known bugs" list, preferring instead careful design, coding, and
testing practices that prevent most bugs and fixing bugs that do
occur as they are discovered.  A Scheme program run using Chez
Scheme can crash due to bugs in the program, but it should not crash
due to bugs in the implementation.

Efficiency means performing at a high level, consuming minimal cpu
time and memory.  Performance should be a continuous function, with
no cliffs or surprises, and should scale well as program or problem
size grow.  Performance should be balanced across features, not
good in one area and bad in another.  Compile time is important as
well as run time, so compiler optimizations are generally expected
to pay their own way, i.e., indirectly benefit compiler performance
enough to cover the direct cost of the optimization.

We attempt to achieve the core principles through careful control
over growth, testing, and documentation.

Like the Scheme language itself, a good implementation is not built
by piling up features but by providing enabling building blocks.
So when asked to add a new feature, we first look for a way to
achieve the same effect with existing functionality or smaller
extensions that are more generally applicable.

Chez Scheme is tested in two ways: implicitly by bootstrapping
itself and explicitly via a suite of tests.  The suite of tests is
about as large as the code base for the implementation, and it is
often the case that more lines of test code than implementation
code are added to support a new feature.  We also benchmark the
system whenever we make a change that might materially affect
performance.

This project also includes documentation for Chez Scheme in the
form of a manual page, a user's guide, and release notes, and we
try to set high standards for this documentation.  A feature isn't
fully implemented until it has been documented.  Writing documentation
often exposes unnecessary complexity in the design and bugs in the
implementation, particularly in corner cases.

Consistent with these principles, we naturally want Chez Scheme to
evolve in various useful ways to, among other things:

* increase utility
* improve user friendliness
* support new standards
* run on new platforms

Backward compatibility should be maintained whenever feasible but
must sometimes take a back seat to progress in a system whose
lifetime is measured in decades.

Please keep in mind the following guidance when preparing contributions:

* Appropriate tests and documentation changes should be included
  with all code changes.

* Coding structure (including indentation) should be consistent
  with the existing code base.  This implies that contributors should
  study the existing code before contributing.

* Spend the time required to make the code as clean, clear, and
  efficient as possible.  All other things equal, shorter code is
  preferable to longer code.  Although some people believe more klocs
  equals more value, code quality is in fact inversely proportional
  to code size.

* Don't forget to update the release notes.

* Some contributions may be more appropriately published as projects
  of their own.  If you are contributing a significant extension built
  using Chez Scheme, consider whether your contribution is such an
  independent project.  An example of such a project is the [Nanopass
  Framework](http://github.com/nanopass/nanopass-framework-scheme)
  which is both used by Chez Scheme and was initially written using
  Chez Scheme, but evolves separately.

Before investing significant effort preparing a contribution,
consider running the idea by one of the committers for additional
guidance and advice.
