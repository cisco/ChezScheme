#!/bin/bash -v

cd ..

case $TARGET_MACHINE in
  *i3le|ti3le) INST=i386;;
  *a6le|ta6le) INST=x86_64;;
  *)
esac
curl -L -o installer.sh http://www.cs.utah.edu/plt/snapshots/current/installers/min-racket-current-${INST}-linux-precise.sh
sh installer.sh --in-place --dest $HOME/racket/

$HOME/racket/bin/racket -v
$HOME/racket/bin/raco pkg install -i --auto -D cs-bootstrap

export MACH=$TARGET_MACHINE
cd ChezScheme
$HOME/racket/bin/racket -l cs-bootstrap

