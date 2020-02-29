;;; trace.ss
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

(let ()

(define tracer-list '())
(define-threaded trace-level 0)
(define-threaded trace-continuation #f)

(define-record-type tracer
  (fields (mutable id) (mutable old) (mutable new))
  (nongenerative)
  (sealed #t))

(define bars
   (lambda (i p)
      (letrec ([bars1
                (lambda (i p)
                   (unless (fx<= i 0)
                      (write-char #\space p)
                      (bars2 (fx- i 1) p)))]
               [bars2
                (lambda (i p)
                   (unless (fx<= i 0)
                      (write-char #\| p)
                      (bars1 (fx- i 1) p)))])
         (bars2 i p))))

(define trace-display
 (let ([last-trace-level 0])
   (lambda (print x)
      (let ([p (trace-output-port)])
         (if (> trace-level 10)
             (let ([s (number->string (- trace-level 1))])
                (bars (fx- 9 (string-length s)) p)
                (write-char #\[ p)
                (display s p)
                (write-char #\] p))
             (bars trace-level p))
         (set! last-trace-level trace-level)
         (parameterize ([pretty-initial-indent (fxmin trace-level 11)]
                        [pretty-line-length 80]
                        [pretty-one-line-limit 80])
            (print x p))))))

(define pretty-print-multiple
  (lambda (ls p)
    (let ([indent (pretty-initial-indent)])
      (let f ([x (car ls)] [ls (cdr ls)])
        ((trace-print) x p)
        (unless (null? ls)
          (let f ([n indent])
            (write-char #\space p)
            (unless (fx= n 1) (f (fx- n 1))))
          (f (car ls) (cdr ls)))))))

(define prune-tracer-list
   (lambda ()
      (set! tracer-list
         (let prune ([ls tracer-list])
            (if (null? ls)
                '()
                (let ((t (car ls)))
                  (if (and (top-level-bound? (tracer-id t))
                           (eq? (tracer-new t)
                                (top-level-value (tracer-id t))))
                      (cons t (prune (cdr ls)))
                      (prune (cdr ls)))))))))

(set! $trace
   (lambda ids
      (prune-tracer-list)
      (for-each (lambda (id) 
                   (unless (symbol? id)
                      ($oops 'trace "~s is not a symbol" id))
                   (unless (top-level-bound? id)
                      ($oops 'trace "~:s is not bound" id))
                   (unless (procedure? (top-level-value id))
                      ($oops 'trace
                             "the top-level value of ~s is not a procedure"
                             id)))
                ids)
      (if (null? ids)
          (map tracer-id tracer-list)
          (map (lambda (id)
                  (unless (memq id (map tracer-id tracer-list))
                     (let ([old (top-level-value id)])
                        (let ([new ($trace-closure id old)])
                           (if (top-level-mutable? id)
                               (set-top-level-value! id new)
                               (begin
                                 (define-top-level-value id new)
                                 (warningf 'trace "redefining ~s; existing references will not be traced" id)))
                           (set! tracer-list
                                 (cons (make-tracer id old new)
                                       tracer-list)))))
                  id)
               ids))))

(set! $untrace
   (lambda ids
      (prune-tracer-list)
      (let f ([ls tracer-list] [gone '()] [keep '()])
         (if (null? ls)
             (begin (set! tracer-list keep)
                    gone)
             (let* ([x (car ls)] [id (tracer-id x)])
                (if (or (null? ids) (memq id ids))
                    (begin (set-top-level-value! id (tracer-old x))
                           (f (cdr ls) (cons id gone) keep))
                    (f (cdr ls) gone (cons x keep))))))))

(set! $trace-closure
   (lambda (name closure)
      (unless (procedure? closure)
         ($oops 'trace "~s is not a procedure" closure))
      (lambda args
         (call/1cc
            (lambda (k)
               (if (eq? k trace-continuation)
                   (begin (trace-display (trace-print) (cons name args))
                          (apply closure args))
                   (fluid-let ([trace-level (+ 1 trace-level)]
                               [trace-continuation trace-continuation])
                      (trace-display (trace-print) (cons name args))
                      (call-with-values
                        (lambda ()
                          (call/1cc
                            (lambda (k)
                              (set! trace-continuation k)
                              (apply closure args))))
                        (case-lambda
                          [(x) (trace-display (trace-print) x) x]
                          [() (trace-display
                                (lambda (x p) (display x p) (newline p))
                                "*** no values ***")
                              (values)]
                          [args (trace-display pretty-print-multiple args)
                                (apply values args)])))))))))

) ;let
