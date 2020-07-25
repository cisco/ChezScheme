This directory constains enough of a Chez Scheme simulation to load
the Chez Scheme compiler purely from source into Racket and apply the
compiler to itself, thus bootstrapping Chez Scheme. (So, using an
existing Racket v7.1 or later, but without using an existing Chez
Scheme.)

The "make-boot.rkt" programs builds Chez Scheme ".boot" and ".h" files
from source. The output is written to "<machine>/boot/<machine>" in a
Chez Scheme source directory. Build boot files that way before
`configure` and `make` to bootstrap the build.

The Chez Scheme simulation hasn't been made especially fast, so expect
the bootstrap process to take 10 times as long as using an existing
Chez Scheme.

While the similation of Chez Scheme should be robust to many Chez
Scheme changes, it does rely on details of the Chez Scheme
implementation and source, So, the simulation will have to be updated
to accommodate some Chez Scheme changes.
