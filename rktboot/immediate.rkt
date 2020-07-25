#lang racket/base

(define-syntax-rule (immediate name name?)
  (begin
    (provide (rename-out [value name])
             name?)
    
    ;; mutable preserves `eq?` in datum->syntax->datum conversion
    (struct name ([v #:mutable]) #:prefab)
    
    (define value (name #f))))

(immediate base-rtd base-rtd?)
(immediate bwp bwp?)
(immediate black-hole black-hole?)
(immediate $unbound-object $unbound-object?)
