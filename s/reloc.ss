;;; reloc.ss
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

(begin
(define $reloc
  (lambda (type item-offset code-offset)
    (make-reloc type item-offset code-offset
      (or (> item-offset (constant reloc-item-offset-mask))
          (> code-offset (constant reloc-code-offset-mask))))))

(define $make-relocation-table!
  (let ()
    (define link-code-object!
      (foreign-procedure "(cs)link_code_object"
        (scheme-object scheme-object)
        void))
    (define set-reloc!
      (foreign-procedure "(cs)s_set_reloc"
        (scheme-object scheme-object scheme-object)
        scheme-object))
    (define make-reloc-table
      (foreign-procedure "(cs)s_make_reloc_table"
        (scheme-object scheme-object)
        scheme-object))
    (lambda (p r* objs)
      (make-reloc-table p (fold-left (lambda (m r) (fx+ m (if (reloc-long? r) 3 1))) 0 r*))
      (let mkc1 ([r* r*] [n 0])
        (if (null? r*)
            (link-code-object! p objs)
            (let ([r (car r*)] [r* (cdr r*)])
              (if (reloc-long? r)
                  (begin
                    (set-reloc! p n
                      (logor
                        (bitwise-arithmetic-shift-left (reloc-type r) (constant reloc-type-offset))
                        (constant reloc-extended-format)))
                    (set-reloc! p (fx+ n 1) (reloc-item-offset r))
                    (set-reloc! p (fx+ n 2) (reloc-code-offset r))
                    (mkc1 r* (fx+ n 3)))
                  (begin
                    (set-reloc! p n 
                      (logor
                        (bitwise-arithmetic-shift-left (reloc-type r) (constant reloc-type-offset))
                        (bitwise-arithmetic-shift-left (reloc-code-offset r) (constant reloc-code-offset-offset))
                        (bitwise-arithmetic-shift-left (reloc-item-offset r) (constant reloc-item-offset-offset))))
                    (mkc1 r* (fx+ n 1))))))))))

(let ()
(set! $make-cinst
  (lambda (build-sinst vtable)
    (critical-section
      (let ([p ($make-code-object
                 (if (eq? (subset-mode) 'system) (constant code-flag-system) 0)
                 0
                 "cinst"
                 (* 2 (constant ptr-bytes))
                 #f
                 '())])
        ($make-relocation-table! p
          (list ($reloc (constant reloc-abs) 0 (constant ptr-bytes))
                ($reloc (constant reloc-abs) (constant code-data-disp)
                  (constant code-data-disp)))
          (list vtable (build-sinst p)))
        p))))

(set! $make-vtable
  (lambda (foreign-entries)
    (let ([n (length foreign-entries)])
      (let loop ([i n] [r '()] [offset (constant code-data-disp)])
        (if (fx= i 0)
            (critical-section
              (let ([p ($make-code-object
                         (if (eq? (subset-mode) 'system) (constant code-flag-system) 0)
                         0
                         "vtable"
                         (* n (constant ptr-bytes))
                         #f
                         '())])
                ($make-relocation-table! p r foreign-entries)
                p))
            (loop (fx- i 1)
                  (cons ($reloc
                          (constant reloc-abs)
                          (constant code-data-disp)
                          offset)
                        r)
                  (constant ptr-bytes)))))))
)
)
