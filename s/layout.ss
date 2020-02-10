;;; layout.ss
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

(define compute-field-offsets
 ; type-disp is the offset from the ptr to the object's true address
 ; ls is a list of field descriptors
  (case-lambda
   [(who type-disp n mpm)
    ;; Simple case: all pointers, mutability mask given
    (values -1 mpm n (fx* n (constant ptr-bytes)))]
   [(who type-disp ls)
    (define parse-field
      (lambda (f)
        (define supported-type
          (lambda (x)
            (let ([x (filter-foreign-type x)])
              (and (memq x (record-datatype list)) x))))
        (define (err) ($oops who "invalid field specifier ~s" f))
        (define (s0 f)
          (cond
            [(symbol? f) (values f #t 'scheme-object 'scheme-object 1)]
            [(list? f) (s1 f)]
            [else (err)]))
        (define (s1 f)
          (cond
            [(null? f) (err)]
            [(null? (cdr f))
             (if (symbol? (car f))
                 (values (car f) #t 'scheme-object 'scheme-object 1)
                 (err))]
            [(eq? (car f) 'immutable) (s2 (cdr f) #f)]
            [(eq? (car f) 'mutable) (s2 (cdr f) #t)]
            [else (s2 f #t)]))
        (define (s2 f mutable?)
          (cond
            [(null? f) (err)]
            [(null? (cdr f))
             (if (symbol? (car f))
                 (values (car f) mutable? 'scheme-object 'scheme-object 1)
                 (err))]
            [(supported-type (car f)) =>
             (lambda (real-type) (s3 (cdr f) mutable? (car f) real-type))]
            [else (s3 f mutable? 'scheme-object 'scheme-object)]))
        (define (s3 f mutable? type real-type)
          (cond
            [(null? f) (err)]
            [(symbol? (car f)) (s4 (cdr f) mutable? type real-type (car f))]
            [else (err)]))
        (define (s4 f mutable? type real-type name)
          (cond
            [(null? f) (values name mutable? type real-type 1)]
            [(and (integer? (car f)) (nonnegative? (car f)))
             (values name mutable? type real-type (car f))]
            [else (err)]))
        (s0 f)))
    (define type->bytes
      (lambda (ty)
        (define-syntax ->bytes
          (syntax-rules () ((_ type bytes pred) bytes)))
        (record-datatype cases ty ->bytes
          ($oops who "unrecognized type ~s" ty))))
    (define get-max-alignment
      (lambda (ty)
        (case ty
          [(single-float double-float) (constant max-float-alignment)]
          [else (constant max-integer-alignment)])))
    (define align
      (lambda (n bytes type)
        (let ([k (gcd (get-max-alignment type) bytes)])
          (logand (+ n (fx- k 1)) (fx- k)))))
    (with-values
      (let f ((ls ls) (byte 0))
        (if (null? ls)
            (values 0 0 '() byte) ; pm, mpm, flds, size
            (with-values (parse-field (car ls))
              (lambda (name mutable? type real-type len)
                (let* ((bytes (type->bytes real-type))
                      ; align even if len is zero to give element its
                      ; proper alignment, since zero at the end can mean
                      ; variable-length
                       (byte (align byte bytes real-type)))
                  (with-values (f (cdr ls) (+ byte (* bytes len)))
                    (lambda (pm mpm flds size)
                      (let ((flds (cons (make-fld name mutable? type (+ type-disp byte)) flds)))
                        (if (eq? real-type 'scheme-object)
                            (let ((m (ash (- (ash 1 len) 1)
                                          (fxsrl byte (constant log2-ptr-bytes)))))
                              (values
                                (+ pm m)
                                (if mutable? (+ mpm m) mpm)
                                flds
                                size))
                            (values pm mpm flds size))))))))))
      (lambda (pm mpm flds size)
        (define sanitize-mask
          ; if bits are set for each word, return mask of -1
          ; to give gc a quick test for pure vs. impure
          (lambda (m size)
            (if (= (- (ash 1 (quotient (+ size -1 (constant ptr-bytes)) (constant ptr-bytes))) 1) m)
                -1
                m)))
        (values (sanitize-mask pm size) mpm flds size)))]))
