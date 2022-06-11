;;; Copyright (C) 2008  Abdulaziz Ghuloum, R. Kent Dybvig
;;; Copyright (C) 2006,2007  Abdulaziz Ghuloum
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

(import (scheme) (unicode-data))

; dropping support for s16 inner vectors for now
(include "extract-common.ss")

(define code-point-limit #x110000) ; as of Unicode 5.1
#;(define table-limit #x30000)
(define table-limit code-point-limit)
(define-table (make-table table-ref table-set! table-ref-code)
  (make-vector vector-ref vector-set!)
  table-limit #x40 #x40)

(define-record-type chardata
  (fields (immutable ucchar)
          (immutable lcchar)
          (immutable tcchar)
          (mutable fcchar)
          (mutable ucstr)
          (mutable lcstr)
          (mutable tcstr)
          (mutable fcstr)
          (immutable decomp-canon)
          (immutable decomp-compat))
  (protocol
    (lambda (new)
      (lambda (ucchar lcchar tcchar decomp-canon decomp-compat)
        (new ucchar lcchar tcchar 0 ucchar lcchar tcchar 0
             decomp-canon decomp-compat)))))

(define (find-cdrec idx ls)
  (cond
    [(assq idx ls) => cdr]
    [else (error 'find-cdrec "~s is missing" idx)]))

(define data-case
  (lambda (fields)
    (let ([n (hex->num (car fields))]
          [uc (list-ref fields 12)]
          [lc (list-ref fields 13)]
          [tc (list-ref fields 14)])
      (define (f x) (if (string=? x "") 0 (- (hex->num x) n)))
      (cons n (make-chardata (f uc) (f lc) (f tc)
                (parse-decomp n (list-ref fields 5) #f)
                (parse-decomp n (list-ref fields 5) #t))))))

(define (split str)
  (remove ""
    (let f ([i 0] [n (string-length str)])
      (cond
        [(= i n) (list (substring str 0 n))]
        [(char=? (string-ref str i) #\space)
         (cons (substring str 0 i) 
               (split (substring str (+ i 1) n)))]
        [else (f (add1 i) n)]))))

(define (improperize ls)
  (cond
    [(null? (cdr ls)) (car ls)]
    [else (cons (car ls) (improperize (cdr ls)))]))

(define (c*->off c* n)
  (if (= (length c*) 1)
      (- (car c*) n)
      (improperize (map integer->char c*))))

(define (codes->off str n)
  (c*->off (map hex->num (split str)) n))

;;; decomposition field looks like:
;;;    hex-value*
;;;    <tag> hex-value*
;;; latter appear to be for compatibility decomposition only
(define (parse-decomp n str compat?)
  (let f ([ls (split str)])
    (cond
      [(null? ls) 0]
      [(char=? (string-ref (car ls) 0) #\<)
       (if compat? (c*->off (map hex->num (cdr ls)) n) 0)]
      [else (c*->off (map hex->num ls) n)])))

(define (insert-foldcase-data! ls data)
  (for-each 
    (lambda (fields)
      (let ([n (hex->num (car fields))])
        (let ([cdrec (find-cdrec n ls)]
              [offset (codes->off (caddr fields) n)])
          (chardata-fcchar-set! cdrec offset)
          (chardata-fcstr-set! cdrec offset))))
    (filter (lambda (fields) (equal? (cadr fields) "C")) data))
  (for-each 
    (lambda (fields)
      (let ([n (hex->num (car fields))])
        (chardata-fcstr-set!
          (find-cdrec n ls)
          (codes->off (caddr fields) n))))
    (filter (lambda (fields) (equal? (cadr fields) "F")) data)))

(define (insert-specialcase-data! ls data)
  (for-each
    (lambda (fields)
      (let ([n (hex->num (car fields))])
        (let ([cdrec (find-cdrec n ls)])
          (chardata-lcstr-set! cdrec (codes->off (list-ref fields 1) n))
          (chardata-tcstr-set! cdrec (codes->off (list-ref fields 2) n))
          (chardata-ucstr-set! cdrec (codes->off (list-ref fields 3) n)))))
    (filter
      (lambda (fields) (= 0 (string-length (list-ref fields 4))))
      data)))

(define verify-identity!
  (lambda (n cdrec)
    (define (zeros? . args) (andmap (lambda (x) (eqv? x 0)) args))
    (unless (zeros? (chardata-ucchar cdrec)
                    (chardata-lcchar cdrec)
                    (chardata-tcchar cdrec)
                    (chardata-fcchar cdrec)
                    (chardata-ucstr cdrec)
                    (chardata-lcstr cdrec)
                    (chardata-tcstr cdrec)
                    (chardata-fcstr cdrec)
                    (chardata-decomp-canon cdrec)
                    (chardata-decomp-compat cdrec))
      (error 'verify-identity "failed for ~x, ~s" n cdrec))))

(define build-uncommonized-table
  (lambda (acc ls)
    (let ([table (make-table 0)])
      (for-each 
        (lambda (x)
          (let ([n (car x)] [cdrec (cdr x)])
            (unless (< n code-point-limit)
              (error 'build-table
                "code point value ~s is at or above declared limit ~s"
                n code-point-limit))
            (if (>= n table-limit)
                (verify-identity! n cdrec)
                (table-set! table n (acc cdrec)))))
        ls)
      table)))

(define build-table
  (lambda (acc ls)
    (commonize* (build-uncommonized-table acc ls))))

(define (get-composition-pairs decomp-canon-table)
  (define ($str-decomp-canon c) 
    (define (strop tbl c)
      (let ([n (char->integer c)])
        (if (and (fx< table-limit code-point-limit)
                 (fx>= n table-limit))
            c
            (let ([x (table-ref tbl n)])
              (if (fixnum? x)
                  (integer->char (fx+ x n))
                  x)))))
    (strop decomp-canon-table c))
  (let ([exclusions 
         (map hex->num
           (map car (get-unicode-data
                      "UNIDATA/CompositionExclusions.txt")))]
        [from* '()]
        [to* '()])
    (define (enter i)
      (unless (memv i exclusions)
        (let* ([c (integer->char i)]  [c* ($str-decomp-canon c)])
          (when (pair? c*)
            (set! from* (cons c* from*))
            (set! to* (cons c to*))))))
    (do ([i 0 (fx+ i 1)]) ((fx= i #xD800)) (enter i))
    (do ([i #xE000 (fx+ i 1)]) ((fx= i code-point-limit)) (enter i))
    (commonize* (cons (list->vector from*) (list->vector to*)))))

(let ([ls (map data-case (get-unicode-data "UNIDATA/UnicodeData.txt"))])
  (insert-foldcase-data! ls (get-unicode-data "UNIDATA/CaseFolding.txt"))
  (insert-specialcase-data! ls (get-unicode-data "UNIDATA/SpecialCasing.txt"))
 ; insert final sigma flag for char-downcase conversion
  (chardata-lcstr-set! (find-cdrec #x3a3 ls) 'sigma)
  (with-output-to-file* "unicode-char-cases.ss"
    (lambda ()
      (parameterize ([print-graph #t] [print-vector-length #f] [print-unicode #f])
        (pretty-print
          `(module ($char-upcase $char-downcase $char-titlecase $char-foldcase
                    $str-upcase $str-downcase $str-titlecase $str-foldcase
                    $str-decomp-canon $str-decomp-compat
                    $composition-pairs)
             (define char-upcase-table ',(build-table chardata-ucchar ls))
             (define char-downcase-table ',(build-table chardata-lcchar ls))
             (define char-titlecase-table ',(build-table chardata-tcchar ls))
             (define char-foldcase-table ',(build-table chardata-fcchar ls))
             (define string-upcase-table ',(build-table chardata-ucstr ls))
             (define string-downcase-table ',(build-table chardata-lcstr ls))
             (define string-titlecase-table ',(build-table chardata-tcstr ls))
             (define string-foldcase-table ',(build-table chardata-fcstr ls))
             (define decomp-canon-table ',(build-table chardata-decomp-canon ls))
             (define decomp-compat-table ',(build-table chardata-decomp-compat ls))
             (define table-limit ,table-limit)
             (define code-point-limit ,code-point-limit)
             (define table-ref ,table-ref-code)
             (define (charop tbl c)
               (let ([n (char->integer c)])
                 (if (and (fx< table-limit code-point-limit)
                          (fx>= n table-limit))
                     c
                     (integer->char (fx+ (table-ref tbl n) n)))))
             (define (strop tbl c)
               (let ([n (char->integer c)])
                 (if (and (fx< table-limit code-point-limit)
                          (fx>= n table-limit))
                     c
                     (let ([x (table-ref tbl n)])
                       (if (fixnum? x)
                           (integer->char (fx+ x n))
                           x)))))
             (define ($char-upcase c) (charop char-upcase-table c))
             (define ($char-downcase c) (charop char-downcase-table c))
             (define ($char-titlecase c) (charop char-titlecase-table c))
             (define ($char-foldcase c) (charop char-foldcase-table c))
             (define ($str-upcase c) (strop string-upcase-table c))
             (define ($str-downcase c) (strop string-downcase-table c))
             (define ($str-titlecase c) (strop string-titlecase-table c))
             (define ($str-foldcase c) (strop string-foldcase-table c))
             (define ($str-decomp-canon c) (strop decomp-canon-table c))
             (define ($str-decomp-compat c) (strop decomp-compat-table c))
             (define ($composition-pairs)
               ',(get-composition-pairs
                   (build-uncommonized-table chardata-decomp-canon ls)))))))))

(printf "Happy Happy Joy Joy ~a\n" (sizeof cache))
