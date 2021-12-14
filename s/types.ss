;;; types.ss
;;; Copyright 1984-2017 Cisco Systems, Inc.
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;; http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; annotation fields are mutable so that the reader can handle
;;; cycles involving annotated data
(define-record-type annotation
  (fields (mutable expression) (mutable source) (mutable stripped) (mutable flags))
  (nongenerative #{annotation bc6o9md359cw9ds5-b})
  (sealed #t)
  (protocol
    (lambda (new)
      (case-lambda
        [(expression source stripped) (new expression source stripped (fxlogor (constant annotation-debug) (constant annotation-profile)))]
        [(expression source stripped flags) (new expression source stripped flags)]))))

(define-record-type source
  (fields (immutable sfd) (immutable bfp) (immutable efp))
  (nongenerative #{source gbwctw0mahurbuiegp7uq3-0}))

(define-record-type source-2d
  (parent source)
  (fields (immutable line) (immutable column))
  (nongenerative #{source-2d gbwctw0mahurbuiegp7uq3-2})
  (sealed #t))

(define-record-type source-file-descriptor
  (fields (immutable name) (immutable length) (immutable crc))
  (nongenerative #{source-file-descriptor bdbv4s3hk5ja7rql-a})
  (sealed #t))

(define-record-type syntax-object
  (fields (immutable expression) (immutable wrap))
  (nongenerative #{syntax-object bdehkef6almh6ypb-a})
  (sealed #t))

(define-syntax syntax-object-rtd
  (identifier-syntax (type-descriptor syntax-object)))

(define-record-type code-info
  (fields
    (immutable src)
    (immutable realm)   ; symbol or #f
    (immutable sexpr)
    (immutable free)    ; vector of elts, elt = symbols #f
    (immutable live)    ; vector of pairs each mapping a symbol or pair of symbols to an index
    (immutable rpis))   ; vector of rp-infos
  (nongenerative #{code-info let7gjkji5i3z5a2rnityfvf6-0})
  (sealed #t))

(define-record-type rp-info
  (fields (immutable offset) (immutable src) (immutable sexpr) (immutable mask))
  (nongenerative #{rp-info gr886ae7iuw4wt9ft4vxym-1})
  (sealed #t))

(define cpsymbol '#{closure cedozdf6uqtmcjjt-2})

; block profiling record
(define-record-type rblock
  (fields (immutable srecs) (immutable op))
  (nongenerative #{rblock bt50nyec0orotoeb-0})
  (sealed #t))

(define-record-type static-closure-info
  (nongenerative #{static-closure-info cv84l52b3ghjjuqa-0})
  (sealed #t)
  (fields
    (mutable raw-closure-count)
    (mutable raw-free-var-count)
    (mutable wk-borrowed-count)
    (mutable wk-empty-count)
    (mutable wk-single-count)
    (mutable wk-pair-count)
    (mutable wk-vector-count)
    (mutable wk-vector-free-var-count)
    (mutable nwk-empty-count)
    (mutable nwk-closure-count)
    (mutable nwk-closure-free-var-count))
  (protocol
    (lambda (new)
      (lambda ()
        (new 0 0 0 0 0 0 0 0 0 0 0)))))

#;(define-record-type profile-counter
    (fields (mutable uptr count)) ; sadly, can't specify the type
    (nongenerative #{profile-counter b5vnnom9h4o4uny0-2})
    (sealed #t)
    (protocol (lambda (new) (lambda () (new 0)))))
(let-syntax ([a (lambda (x)
                  (syntax-case x ()
                    [(_ profile-counter? make-profile-counter profile-counter-count profile-counter-count-set!)
                     (let ([rtd ($make-record-type #!base-rtd #f
                                  '#{profile-counter b5vnnom9h4o4uny0-2}
                                  '((mutable uptr count))
                                  #t #f)])
                       #`(begin
                           (define make-profile-counter (record-constructor '#,rtd))
                           (define profile-counter? (record-predicate '#,rtd))
                           (define profile-counter-count (record-accessor '#,rtd 0))
                           (define profile-counter-count-set! (record-mutator '#,rtd 0))))]))])
  (a profile-counter? make-profile-counter profile-counter-count profile-counter-count-set!))


(define-record-type winder
  (fields (immutable in) (immutable out) (immutable attachments))
  (nongenerative #{winder qnbz1n5f3x1ldovscan3nu-0}))

(define-record-type critical-winder
  (parent winder)
  (sealed #t)
  (nongenerative #{critical-winder qnbz1n5f3x1ldovscan3nu-2}))
