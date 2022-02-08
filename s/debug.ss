;;; debug.ss
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

;; Beware of setting any parameters here that are defined in
;; "back.ss", because those will not be set by the host "setup.so"
;; when loading a cross-compiltion "xpatch" later for a cross build

(current-eval interpret)

(subset-mode 'system)

(generate-inspector-information #f)

(eval-syntax-expanders-when '(load eval))

(disable-unbound-warning compile-with-asm compile-with-setup-closure-counts compile-with-closure-counts)

(require-nongenerative-clause #t)

(define compile-with-asm
  (lambda (ss so mach)
    (let ([file (format "~a.asm" (path-root so))])
      (parameterize ([#%$assembly-output (open-output-file file '(buffered replace))])
        (compile-file ss so mach)
        (close-output-port (#%$assembly-output))))))

#;(define compile-with-closure-counts
  (lambda (ss* so* mach)
    (time (for-each (lambda (x y)
                      (collect 2)
                      (compile-file (symbol->string x) (symbol->string y) mach))
            ss* so*))))

#;(module (compile-with-setup-closure-counts compile-with-closure-counts)
  (module (csv-cell csv-row csv-row* csv-rowv)
    (define ->string
      (lambda (x)
        (cond
          [(string? x) x]
          [(symbol? x) (symbol->string x)]
          [(char? x) (list->string (list x))]
          [(number? x) (number->string x)]
          [(identifier? x) (symbol->string (syntax->datum x))]
          [else (format "~s" x)])))

    (define needs-double-quote?
      (lambda (str)
        (let ([len (string-length str)])
          (let f ([i 0])
            (and (< i len)
                 (let ([c (string-ref str i)])
                   (or (char=? c #\,) (char=? c #\") (f (fx+ i 1)))))))))

    (define double-double-quote
      (lambda (str)
        (let ([len (string-length str)])
          (let f ([i 0] [new-len 0])
            (if (fx= i len)
                (make-string new-len)
                (let ([c (string-ref str i)])
                  (if (char=? c #\")
                      (let ([new-str (f (fx+ i 1) (fx+ new-len 2))])
                        (string-set! new-str new-len #\")
                        (string-set! new-str (fx+ new-len 1) #\")
                        new-str)
                      (let ([new-str (f (fx+ i 1) (fx+ new-len 1))])
                        (string-set! new-str new-len c)
                        new-str))))))))

    (define csv-cell
      (lambda (op x)
        (let ([str (->string x)])
          (if (needs-double-quote? str)
              (fprintf op "\"~a\"" (double-double-quote str))
              (display str op)))))

    (define csv-row
      (lambda (op xs)
        (let f ([xs xs])
          (if (null? xs)
              (begin (newline op) (newline))
              (let ([x (car xs)] [xs (cdr xs)])
                (csv-cell (current-output-port) x)
                (csv-cell op x)
                (unless (null? xs) (display ","))
                (unless (null? xs) (display "," op))
                (f xs))))))

    (define csv-rowv
      (lambda (op . xs)
        (let f ([xs xs])
          (if (null? xs)
              (newline op)
              (let ([x (car xs)] [xs (cdr xs)])
                (cond
                  [(vector? x)
                   (let ([len (vector-length x)])
                     (do ([i 0 (fx+ i 1)])
                       ((= i len))
                       (csv-cell op (vector-ref x i))
                       (unless (= (fx+ i 1) len) (display "," op)))
                     (newline op))]
                  [else
                    (csv-cell op x)
                    (unless (null? xs) (display "," op))
                    (f xs)]))))))

    (define csv-row*
      (lambda (op . xs)
        (csv-row op xs))))

  (define compile-with-setup-closure-counts
    (lambda (opts ss* so* mach with-header?)
      (include "types.ss")
      (assert (or (eq? opts 'all) (equal? opts '(all))))
      (let ([ci (make-static-closure-info)])
        (time (for-each (lambda (x y)
                          (collect 2)
                          (parameterize ([#%$track-static-closure-counts ci]
                                         [#%$track-dynamic-closure-counts #t])
                            (compile-file (symbol->string x) (symbol->string y) mach)))
                ss* so*))
          (let ([v (#%$dynamic-closure-counts)])
            (call-with-output-file "static-compiler.csv"
              (lambda (op)
                (let* ([final-cl-count (+ (static-closure-info-wk-pair-count ci)
                                          (static-closure-info-wk-vector-count ci)
                                          (static-closure-info-nwk-closure-count ci))]
                       [final-fv-count (+ (* (static-closure-info-wk-pair-count ci) 2)
                                          (static-closure-info-wk-vector-free-var-count ci)
                                          (static-closure-info-nwk-closure-free-var-count ci))]
                       [orig-var/closure (if (zero? (static-closure-info-raw-closure-count ci))
                                             (quote n/a)
                                             (inexact (/ (static-closure-info-raw-free-var-count ci)
                                                         (static-closure-info-raw-closure-count ci))))]
                       [final-var/closure (if (zero? final-cl-count)
                                              (quote n/a)
                                              (inexact (/ final-fv-count final-cl-count)))]
                       [wk-var/vector (if (zero? (static-closure-info-wk-vector-count ci))
                                          (quote n/a)
                                          (inexact (/ (static-closure-info-wk-vector-free-var-count ci)
                                                      (static-closure-info-wk-vector-count ci))))]
                       [nwk-var/closure (if (zero? (static-closure-info-nwk-closure-count ci))
                                            (quote n/a)
                                            (inexact (/ (static-closure-info-nwk-closure-free-var-count ci)
                                                        (static-closure-info-nwk-closure-count ci))))])
                  (when with-header? 
                    (csv-row* op "Opts" "Orig. Closure Count" "Orig. Total Free Vars" "Orig. Avg. Free Var/Closure"
                      "Final Closure Count" "Final Total Free Vars" "Final Avg. Free Var/Closure"
                      "WK Borrowed" "WK Empty" "WK Single" "WK Pair" "WK Vector" "WK Vector Total Vars" "WK Vector Vars/Vector"
                      "NWK Empty" "NWK Closure" "NWK Closure Total Vars" "NWK Closure Vars/Closure"
                      "% Closures Eliminated" "% Size Reduction"))
                  #|
                  (printf "compiler closure elimination\n")
                  (printf "  original closures:       ~d\n" (static-closure-info-raw-closure-count ci))
                  (printf "  original free var total: ~d\n" (static-closure-info-raw-free-var-count ci))
                  (printf "  fv/closure:              ~s\n" orig-var/closure)
                  (printf "  final closure count:     ~d\n" final-cl-count)
                  (printf "  final free var total:    ~d\n" final-fv-count)
                  (printf "  fv/closure:              ~s\n" final-var/closure)
                  (printf "    wk empty:              ~d\n" (static-closure-info-wk-empty-count ci))
                  (printf "    wk borrowed:           ~d\n" (static-closure-info-wk-borrowed-count ci))
                  (printf "    wk single:             ~d\n" (static-closure-info-wk-single-count ci))
                  (printf "    wk pair:               ~d\n" (static-closure-info-wk-pair-count ci))
                  (printf "    wk vector:             ~d\n" (static-closure-info-wk-vector-count ci))
                  (printf "    wk vector free var:    ~d\n" (static-closure-info-wk-vector-free-var-count ci))
                  (printf "    fv/vector:             ~s\n" wk-var/vector)
                  (printf "    nwk empty:             ~s\n" (static-closure-info-nwk-empty-count ci))
                  (printf "    nwk closure:           ~s\n" (static-closure-info-nwk-closure-count ci))
                  (printf "    nwk closure free var:  ~s\n" (static-closure-info-nwk-closure-free-var-count ci))
                  (printf "    fv/closure:            ~s\n" nwk-var/closure)
                  (printf "  % closures eliminated:   ~s\n"
                    (inexact (/ (* (- (static-closure-info-raw-closure-count ci) final-cl-count) 100)
                                (static-closure-info-raw-closure-count ci))))
                  (printf "  % free-vars eliminated:  ~s\n"
                    (inexact (/ (* (- (static-closure-info-raw-free-var-count ci) final-fv-count) 100)
                                (static-closure-info-raw-free-var-count ci))))
                  |#
                  (printf "printing static row!!!\n")
                  (csv-row* op opts (static-closure-info-raw-closure-count ci)
                    (static-closure-info-raw-free-var-count ci) orig-var/closure
                    final-cl-count final-fv-count final-var/closure
                    (static-closure-info-wk-borrowed-count ci)
                    (static-closure-info-wk-empty-count ci)
                    (static-closure-info-wk-single-count ci)
                    (static-closure-info-wk-pair-count ci)
                    (static-closure-info-wk-vector-count ci)
                    (static-closure-info-wk-vector-free-var-count ci)
                    wk-var/vector
                    (static-closure-info-nwk-empty-count ci)
                    (static-closure-info-nwk-closure-count ci)
                    (static-closure-info-nwk-closure-free-var-count ci)
                    nwk-var/closure
                    (inexact (/ (* (- (static-closure-info-raw-closure-count ci) final-cl-count) 100)
                                (static-closure-info-raw-closure-count ci)))
                    (inexact (/ (* (- (static-closure-info-raw-free-var-count ci) final-fv-count) 100)
                                (static-closure-info-raw-free-var-count ci))))))
              (if with-header? 'replace 'append))))))

  (define compile-with-closure-counts
    (lambda (opts ss* so* mach with-header?)
      (assert (or (eq? opts 'all) (equal? opts '(all))))
      (#%$clear-dynamic-closure-counts)
      (time (for-each (lambda (x y)
                        (collect 2)
                        (parameterize ([#%$track-dynamic-closure-counts #t]) ; true, but could be false
                          (compile-file (symbol->string x) (symbol->string y) mach)))
              ss* so*))
      (let ([v (#%$dynamic-closure-counts)])
        (call-with-output-file "dynamic-compiler.csv"
          (lambda (op)

            (when with-header?
              (csv-row* op "Name"
                "Raw ref count" "Ref count" "% Ref Elim"
                "Raw create count" "Pair create count" "Vector create count" "Closure create count"
                "Total create count" "% Create Elim"
                "Raw alloc" "Vector alloc" "Closure alloc" "Total alloc" "% Alloc Elim"
                "Padded closure alloc count" "Padded vector alloc count"))
            (let* ([%ref-elim (if (zero? (vector-ref v 0))
                                  'n/a
                                  (* (/ (- (vector-ref v 0) (vector-ref v 3))
                                        (vector-ref v 0))
                                     100.0))]
                   [total-create (+ (vector-ref v 4) (vector-ref v 5) (vector-ref v 8))]
                   [%create-elim (if (zero? (vector-ref v 1))
                                     'n/a
                                     (* (/ (- (vector-ref v 1) total-create) (vector-ref v 1))
                                        100.0))]
                   [total-alloc (+ (* 2 (vector-ref v 4)) (vector-ref v 6) (vector-ref v 9))]
                   [%alloc-elim (if (zero? (vector-ref v 2))
                                    'n/a
                                    (* (/ (- (vector-ref v 2) total-alloc)
                                          (vector-ref v 2))
                                       100.0))])
              #|
              (printf "compiler dynamic closure counts:\n")
              (printf "  original references:         ~d\n" (vector-ref v 0))
              (printf "  original closure creations:  ~d\n" (vector-ref v 1))
              (printf "  original closure allocation: ~d\n" (vector-ref v 2))
              (printf "  final references:            ~d\n" (vector-ref v 3))
              (printf "  % eliminated:                ~s\n" %ref-elim)
              (printf "  pairs created:               ~d\n" (vector-ref v 4))
              (printf "  vectors created:             ~d\n" (vector-ref v 5))
              (printf "  closures created:            ~d\n" (vector-ref v 8))
              (printf "  total creation:              ~d\n" total-create)
              (printf "  % eliminated:                ~s\n" %create-elim)
              (printf "  vector allocation:           ~d\n" (vector-ref v 6))
              (printf "  closure allocation:          ~d\n" (vector-ref v 9))
              (printf "  total allocation:            ~d\n" total-alloc)
              (printf "  % eliminated:                ~s\n" %alloc-elim)
              (printf "  padded vector allocation:    ~d\n" (vector-ref v 7))
              (printf "  padded closure allocation:   ~d\n" (vector-ref v 10))
              |#
              (printf "printing dynamic row!!!\n")
              (csv-row* op opts
                (vector-ref v 0) (vector-ref v 3) %ref-elim
                (vector-ref v 1) (vector-ref v 4) (vector-ref v 5) (vector-ref v 8)
                total-create %create-elim
                (vector-ref v 2) (vector-ref v 6) (vector-ref v 9) total-alloc %alloc-elim
                (vector-ref v 7) (vector-ref v 10))))
            (if with-header? 'replace 'append))))))

