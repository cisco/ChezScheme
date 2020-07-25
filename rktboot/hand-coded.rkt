#lang racket/base

(provide $hand-coded)

(define ($hand-coded sym)
  (case sym
    [($install-library-entry-procedure)
     (lambda (key val)
       (hash-set! library-entries key val))]
    [($foreign-entry-procedure) void]
    [(callcc call1cc) call/cc]
    [(scan-remembered-set
      get-room
      call-error
      dooverflood
      dooverflow
      dorest0 dorest1 dorest2 dorest3 dorest4 dorest5 doargerr
      dounderflow nuate reify-cc
      dofargint32 dofretint32 dofretuns32 dofargint64 dofretint64
      dofretuns64 dofretu8* dofretu16* dofretu32* domvleterr
      values-error $shift-attachment)
     void]
    [(bytevector=?) equal?]
    [($wrapper-apply wrapper-apply arity-wrapper-apply) void]
    [(nonprocedure-code) (lambda args (error "not a procedure"))]
    [else
     (error '$hand-coded "missing ~s" sym)]))

(define library-entries (make-hasheqv))
