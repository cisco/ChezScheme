;;; enum.ss
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

;; NOTES:
;; This implementation assume the universe is small
;;  and the algorithms used by this implementation may be
;;  up to linear in the universe
;;
;; This code is a good candidate for partial-static-structure optimization
;;  Right now the define-enumeration macro is doing optimizations
;;  that could be automatically performed by PSS if PSS worked on enums
;;
;; The R6RS standard is unclear whether the function returned by enum-set-indexer
;;  should throw an error if its argument is not a symbol.  We have chosen to
;;  not include that check, but if the standard is updated, this may need to be changed.

(let ()

;;;;;;;;
#| Low-level enum-set definition and operations
   The structure is as follows:

-------------------------------------------------------------------------------
The following records are created once:

enum-base-rtd:
+-----------------+--------------------+--------------------------------+-----+
| rtd:#!base-rtd  | parent:#!base-rtd  | fields:(index->sym sym->index) | ... |
+-----------------+--------------------+--------------------------------+-----+

enum-parent-rtd:
+-----------------+--------------------+--------------------------------+-----+
| rtd:#!base-rtd  | parent: #f         | fields:(members)               | ... |
+-----------------+--------------------+--------------------------------+-----+

-------------------------------------------------------------------------------
The following record is created per enum-type and it stored the mappings
between symbols and their corresponding bits in the bit mask:

this-enum-rtd:
+-------------------+------------------------+-----------+-----
| rtd:enum-base-rtd | parent:enum-parent-rtd | fields:() | ... 
+-------------------+------------------------+-----------+-----
 ----+------------+------------+
  ...| index->sym | sym->index |
 ----+------------+------------+

-------------------------------------------------------------------------------
The following record is created per enum-set:

an-enum-set:
+-------------------+--------------------------------+
| rtd:this-enum-rtd | members: 17 (integer bit mask) |
+-------------------+--------------------------------+

|#

  (define enum-base-rtd
    (make-record-type              ; not sealed, not opaque
      #!base-rtd                   ; undocumented #!base-rtd
      '#{enum b9s78zmm79qs7j22-a}  ; make enum-base-rtd type nongenerative
      '((immutable sym->index)     ; static (per enumeration type) fields
        (immutable index->sym))))
  (define enum-parent-rtd          ; not sealed, not opaque, nongenerative
    (make-record-type
      '#{enum-parent dwwi4y1kribh7mif58yoxe-0}
      '((immutable members))))

  (define get-sym->index (csv7:record-field-accessor enum-base-rtd 'sym->index))
  (define get-index->sym (csv7:record-field-accessor enum-base-rtd 'index->sym))
  (define get-members    (csv7:record-field-accessor enum-parent-rtd 'members))
  (define members-universe -1) ;; All bits set

;;;;;;;;

  ;; Make a new enum-set using the rtd and the new set of members
  (define (make-enum-set enum-set-rtd members)
    #;((record-constructor enum-set-rtd) members)
   ; breaking the abstraction to avoid significant efficiency hit
    ($record enum-set-rtd members))

  ;; Perform type check for enum-set and return its RTD
  (define (enum-set-rtd who enum-set)
    (or (and (record? enum-set)
             (let ([rtd (record-rtd enum-set)])
               (and (eq? (record-rtd rtd) enum-base-rtd)
                    rtd)))
        ($oops who "~s is not an enumeration" enum-set)))

  (define (assert-symbol-list who symbol-list)
    (unless (and (list? symbol-list)
                 (for-all symbol? symbol-list))
      ($oops who "~s is not a list of symbols" symbol-list)))
  (define (assert-symbol who symbol)
    (unless (symbol? symbol)
      ($oops who "~s is not a symbol" symbol)))

  (define rtd&list->enum-set
    (lambda (who rtd symbol-list)
      (let ([sym->index (get-sym->index rtd)])
        (let loop ([members 0]
                   [symbol-list symbol-list])
          (if (null? symbol-list)
              (make-enum-set rtd members)
              (let ([index (symbol-hashtable-ref sym->index (car symbol-list) #f)])
                (if (not index)
                    (if who
                        ($oops who "universe does not include specified symbol ~s"
                               (car symbol-list))
                        (loop members (cdr symbol-list)))
                    (loop (logbit1 index members) (cdr symbol-list)))))))))

  (define $enum-set->list
    (lambda (who enum-set)
      (let ([rtd (enum-set-rtd who enum-set)])
        (let ([index->sym (get-index->sym rtd)]
              [members    (get-members    enum-set)])
          (let loop ([i (fx1- (vector-length index->sym))]
                     [lst '()])
            (if (fx< i 0)
                lst
                (loop (fx1- i)
                      (if (logbit? i members)
                          (cons (vector-ref index->sym i) lst)
                          lst))))))))

  (record-writer enum-parent-rtd (lambda (x p wr) (display "#<enum-set>" p)))

;;;;;;;;
;; Constructor

  (let ()
    ;; Takes lst and assigns indexes to each element of lst
    ;; lst :: symbol-list
    ;; index :: fixnum
    ;; symbol->index :: hashtable from symbols to fixnum
    ;; rev-lst :: symbol-list (stored in reverse order)
    ;;
    ;; Result :: (values fixnum (vector of symbols))
    (define (make-symbol->index lst index symbol->index rev-lst)
      (cond
        [(null? lst)
         (let ([index->symbol (make-vector index)])
           (let loop ([i (fx1- index)]
                      [rev-lst rev-lst])
             (unless (null? rev-lst) ;; or (< i 0)
               (vector-set! index->symbol i (car rev-lst))
               (loop (fx1- i) (cdr rev-lst))))
           (values index index->symbol))]
        [(symbol-hashtable-contains? symbol->index (car lst))
         (make-symbol->index (cdr lst) index symbol->index rev-lst)]
        [else
         (symbol-hashtable-set! symbol->index (car lst) index)
         (make-symbol->index (cdr lst) (fx1+ index) symbol->index (cons (car lst) rev-lst))]))

    (set! make-enumeration
      (lambda (symbol-list)
        (assert-symbol-list 'make-enumeration symbol-list)
        (let ([sym->index (make-hashtable symbol-hash eq?)])
          (let-values ([(index index->sym) (make-symbol->index symbol-list 0 sym->index '())])
            (let ([this-enum-rtd
                   ($make-record-type
                     enum-base-rtd enum-parent-rtd "enum-type"
                     '() ; no fields to add
                     #t ; sealed
                     #f ; not opaque
                     sym->index
                     index->sym)])
              (make-enum-set this-enum-rtd members-universe)))))))

;;;;;;;;;
;; Misc functions

  (set! $enum-set-members get-members)

  (set! enum-set-universe
    (lambda (enum-set)
      (make-enum-set (enum-set-rtd 'enum-set-universe enum-set) -1)))
  (set! enum-set-indexer
    (lambda (enum-set)
      (let ([sym->index (get-sym->index (enum-set-rtd 'enum-set-indexer enum-set))])
        (lambda (x)
          (assert-symbol 'enum-set-indexer x)
          (symbol-hashtable-ref sym->index x #f)))))

  (set! enum-set-constructor
    (lambda (enum-set)
      (let ([rtd (enum-set-rtd 'enum-set-constructor enum-set)])
        (lambda (symbol-list)
          (assert-symbol-list 'enum-set-constructor symbol-list)
          (rtd&list->enum-set 'enum-set-constructor rtd symbol-list)))))

  (set! enum-set->list
    (lambda (enum-set)
      ($enum-set->list 'enum-set->list enum-set)))

;;;;;;;;;
;; Predicates

  (set! enum-set?
    (lambda (enum-set)
      (and (record? enum-set)
           (let ([rtd (record-rtd enum-set)])
             (eq? (record-rtd rtd) enum-base-rtd)))))

  (let ()
    (define (enum-set-subset-aux? enum-set1 enum-set2 rtd1 rtd2)
      (let ([index->sym1 (get-index->sym rtd1)]
            [members1    (get-members enum-set1)]
            [sym->index2 (get-sym->index rtd2)]
            [members2    (get-members enum-set2)])
        (let loop ([index1 0])
          (or (fx= index1 (vector-length index->sym1))
              (let ([index2 (symbol-hashtable-ref
                              sym->index2
                              (vector-ref index->sym1 index1) #f)])
                (and index2
                     (or (not (logbit? index1 members1))
                         (logbit? index2 members2))
                     (loop (fx1+ index1))))))))

    (set! enum-set-member?
      (lambda (symbol enum-set)
        (assert-symbol 'enum-set-member? symbol)
        (let ([sym->index (get-sym->index
                            (enum-set-rtd 'enum-set-member? enum-set))])
          (let ([index (symbol-hashtable-ref sym->index symbol #f)])
            (and index
                 (logbit? index (get-members enum-set)))))))

    (set! enum-set-subset?
      (lambda (enum-set1 enum-set2)
        (let ([rtd1 (enum-set-rtd 'enum-set-subset? enum-set1)]
              [rtd2 (enum-set-rtd 'enum-set-subset? enum-set2)])
          (if (eq? rtd1 rtd2)
              (let ([members2 (get-members enum-set2)])
                (= members2 (logor (get-members enum-set1) members2)))
              (enum-set-subset-aux? enum-set1 enum-set2 rtd1 rtd2)))))

    (set! enum-set=?
      (lambda (enum-set1 enum-set2)
        (let ([rtd1 (enum-set-rtd 'enum-set=? enum-set1)]
              [rtd2 (enum-set-rtd 'enum-set=? enum-set2)])
          (if (eq? rtd1 rtd2)
              (= (get-members enum-set1) (get-members enum-set2))
              (and (enum-set-subset-aux? enum-set1 enum-set2 rtd1 rtd2)
                   (enum-set-subset-aux? enum-set2 enum-set1 rtd2 rtd1))))))
    )

;;;;;;;;
;; Set-like functions

  (let ()
    (define-syntax enum-bin-op
      (syntax-rules ()
        [(_ name (members1 members2) members-expr)
         (set! name
           (lambda (enum-set1 enum-set2)
             (let ([rtd1 (enum-set-rtd 'name enum-set1)]
                   [rtd2 (enum-set-rtd 'name enum-set2)])
               (unless (eq? rtd1 rtd2)
                 ($oops 'name "~s and ~s have different enumeration types"
                        enum-set1 enum-set2))
               (make-enum-set rtd1 (let ([members1 (get-members enum-set1)]
                                         [members2 (get-members enum-set2)])
                                     members-expr)))))]))

    (enum-bin-op enum-set-union        (members1 members2) (logor  members1 members2))
    (enum-bin-op enum-set-intersection (members1 members2) (logand members1 members2))
    (enum-bin-op enum-set-difference   (members1 members2) (logand members1 (lognot members2))) 
   )

;;;;;;;;
;; Other functions

  (set! enum-set-complement
    (lambda (enum-set)
      (let ([rtd (enum-set-rtd 'enum-set-complement enum-set)])
        (make-enum-set rtd (lognot (get-members enum-set))))))

  (set! enum-set-projection
    (lambda (enum-set1 enum-set2)
      (rtd&list->enum-set #f
        (enum-set-rtd 'enum-set-projection enum-set2)
        ($enum-set->list 'enum-set-projection enum-set1))))
  )
