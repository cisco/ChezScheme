#lang racket/base

(provide (all-defined-out))

(struct syntax-object (e ctx) #:prefab #:mutable
  #:reflection-name '|{syntax-object bdehkef6almh6ypb-a}|)

(struct top-ribcage (x y) #:prefab #:mutable
  #:reflection-name '|{top-ribcage fxdfzth2q3h88vd-a}|)

(struct fixed-ribcage (x y z) #:prefab #:mutable
  #:reflection-name '|{fixed-ribcage cqxefau3fa3vz4m0-0}|)

(struct extensible-ribcage (chunks) #:prefab #:mutable
  #:reflection-name '|{extensible-ribcage cqxefau3fa3vz4m0-1}|)

(struct local-label (binding level) #:prefab #:mutable)

(struct rec-cons-desc (rtd parent-rcd protocol) #:prefab #:mutable
  #:reflection-name '|{rcd qh0yzh5qyrxmz2l-a}|)

(struct primref2 (name flags arity) #:prefab #:mutable
  #:reflection-name '|{primref a0xltlrcpeygsahopkplcn-2}|)

(struct primref3 (name flags arity signatures) #:prefab #:mutable
  #:reflection-name '|{primref a0xltlrcpeygsahopkplcn-3}|)
