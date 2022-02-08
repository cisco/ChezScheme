;;; back.ss
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
(define-who trace-output-port
   ($make-thread-parameter
      (console-output-port)
      (lambda (x)
         (unless (and (output-port? x) (textual-port? x))
            ($oops who "~s is not a textual output port" x))
         x)))

(define-who trace-print
  ($make-thread-parameter
    pretty-print
    (lambda (x)
      (unless (procedure? x)
        ($oops who "~s is not a procedure" x))
      x)))
    
(define suppress-greeting (make-parameter #f (lambda (x) (and x #t))))

(define-who collect-maximum-generation
  (let ([$get-maximum-generation (foreign-procedure "(cs)maxgen" () int)]
        [$set-maximum-generation! (foreign-procedure "(cs)set_maxgen" (int) void)])
    (case-lambda
      [() ($get-maximum-generation)]
      [(g)
       (unless (and (fixnum? g) (fx>= g 0)) ($oops who "invalid generation ~s" g))
       (when (fx= g 0) ($oops who "new maximum generation must be at least 1"))
       (let ([limit (fx- (constant static-generation) 1)])
         (when (fx> g limit) ($oops who "~s exceeds maximum supported value ~s" g limit)))
       ($set-maximum-generation! g)])))

(define-who release-minimum-generation
  (let ([$get-release-minimum-generation (foreign-procedure "(cs)minfreegen" () int)]
        [$set-release-minimum-generation! (foreign-procedure "(cs)set_minfreegen" (int) void)])
    (case-lambda
      [() ($get-release-minimum-generation)]
      [(g)
       (unless (and (fixnum? g) (fx>= g 0)) ($oops who "invalid generation ~s" g))
       (unless (fx<= g (collect-maximum-generation))
         ($oops who "new release minimum generation must not be be greater than collect-maximum-generation"))
       ($set-release-minimum-generation! g)])))

(define-who in-place-minimum-generation
  (let ([$get-mark-minimum-generation (foreign-procedure "(cs)minmarkgen" () int)]
        [$set-mark-minimum-generation! (foreign-procedure "(cs)set_minmarkgen" (int) void)])
    (case-lambda
      [() ($get-mark-minimum-generation)]
      [(g)
       (unless (and (fixnum? g) (fx>= g 0)) ($oops who "invalid generation ~s" g))
       (let ([limit (fx- (constant static-generation) 1)])
         (when (fx> g limit) ($oops who "~s exceeds maximum supported value ~s" g limit)))
       ($set-mark-minimum-generation! g)])))

(define-who enable-object-counts
  (let ([$get-enable-object-counts (foreign-procedure "(cs)enable_object_counts" () boolean)]
        [$set-enable-object-counts (foreign-procedure "(cs)set_enable_object_counts" (boolean) void)])
    (case-lambda
      [() ($get-enable-object-counts)]
      [(b) ($set-enable-object-counts b)])))

(define-who enable-object-backreferences
  (let ([$get-enable-object-backreferences (foreign-procedure "(cs)enable_object_backreferences" () boolean)]
        [$set-enable-object-backreferences (foreign-procedure "(cs)set_enable_object_backreferences" (boolean) void)])
    (case-lambda
      [() ($get-enable-object-backreferences)]
      [(b) ($set-enable-object-backreferences b)])))

(define-who collect-trip-bytes
   (make-parameter
      (constant default-collect-trip-bytes)
      (lambda (x)
         (unless (and (fixnum? x) (fx< 0 x))
            ($oops who "~s is not a positive fixnum" x))
            ($set-collect-trip-bytes x)
            x)))

(define-who heap-reserve-ratio
  (case-lambda
    [() $heap-reserve-ratio]
    [(x) (unless (number? x)
           ($oops who "~s is not a number" x))
         (let ([y (inexact x)])
           (unless (and (flonum? y) (>= y 0))
             ($oops who "invalid heap reserve ratio ~s" x))
           (set! $heap-reserve-ratio y))]))

(define-who $assembly-output
  ($make-thread-parameter #f
    (lambda (x)
      (cond
        [(or (not x) (and (output-port? x) (textual-port? x))) x]
        [(eq? x #t) (current-output-port)]
        [else ($oops who "~s is not a textual output port or #f" x)]))))

(define-who expand-output
  ($make-thread-parameter #f
    (lambda (x)
      (unless (or (not x) (and (output-port? x) (textual-port? x)))
        ($oops who "~s is not a textual output port or #f" x))
      x)))

(define-who expand/optimize-output
  ($make-thread-parameter #f
    (lambda (x)
      (unless (or (not x) (and (output-port? x) (textual-port? x)))
        ($oops who "~s is not a textual output port or #f" x))
      x)))

(define generate-wpo-files
  ($make-thread-parameter #f
    (lambda (x)
      (and x #t))))

(define-who generate-covin-files
  ($make-thread-parameter #f
    (lambda (x)
      (and x #t))))

(define $enable-check-prelex-flags
  ($make-thread-parameter #f
    (lambda (x)
      (and x #t))))

(define-who run-cp0
  ($make-thread-parameter
    (default-run-cp0)
    (lambda (x)
      (unless (procedure? x)
        ($oops who "~s is not a procedure" x))
      x)))

(define fasl-compressed
  ($make-thread-parameter #t (lambda (x) (and x #t))))

(define compile-file-message
  ($make-thread-parameter #t (lambda (x) (and x #t))))

(define compile-imported-libraries
  ($make-thread-parameter #f (lambda (x) (and x #t))))

(define-who compile-library-handler
  ($make-thread-parameter
    (lambda (ifn ofn) (compile-library ifn ofn))
    (lambda (x)
      (unless (procedure? x) ($oops who "~s is not a procedure" x))
      x)))

(define-who compile-program-handler
  ($make-thread-parameter
    (lambda (ifn ofn) (compile-program ifn ofn))
    (lambda (x)
      (unless (procedure? x) ($oops who "~s is not a procedure" x))
      x)))

(define-who compress-format
  (case-lambda
    [()
     (let ([x ($tc-field 'compress-format ($tc))])
       (cond
         [(eqv? x (constant COMPRESS-GZIP)) 'gzip]
         [(eqv? x (constant COMPRESS-LZ4)) 'lz4]
         [else ($oops who "unexpected $compress-format value ~s" x)]))]
    [(x)
     ($tc-field 'compress-format ($tc)
       (case x
         [(gzip) (constant COMPRESS-GZIP)]
         [(lz4) (constant COMPRESS-LZ4)]
         [else ($oops who "~s is not a supported format" x)]))]))

(define-who compress-level
  (case-lambda
    [()
     (let ([x ($tc-field 'compress-level ($tc))])
       (cond
         [(eqv? x (constant COMPRESS-MIN)) 'minimum]
         [(eqv? x (constant COMPRESS-LOW)) 'low]
         [(eqv? x (constant COMPRESS-MEDIUM)) 'medium]
         [(eqv? x (constant COMPRESS-HIGH)) 'high]
         [(eqv? x (constant COMPRESS-MAX)) 'maximum]
         [else ($oops who "unexpected $compress-level value ~s" x)]))]
    [(x)
     ($tc-field 'compress-level ($tc)
       (case x
         [(minimum) (constant COMPRESS-MIN)]
         [(low) (constant COMPRESS-LOW)]
         [(medium) (constant COMPRESS-MEDIUM)]
         [(high) (constant COMPRESS-HIGH)]
         [(maximum) (constant COMPRESS-MAX)]
         [else ($oops who "~s is not a supported level" x)]))]))

(define-who compile-omit-concatenate-support
  ($make-thread-parameter #f (lambda (x) (and x #t))))

(define-who compile-procedure-realm
  ($make-thread-parameter #f (lambda (x)
                               (when x
                                 (unless (symbol? x)
                                   ($oops who "~a not is #f or a symbol" x)))
                               x)))

(define-who debug-level
  ($make-thread-parameter
    1
    (lambda (x)
      (unless (and (fixnum? x) (<= 0 x 3))
        ($oops who "invalid level ~s" x))
      x)))

(define internal-defines-as-letrec*
  ($make-thread-parameter #t (lambda (x) (and x #t))))

(define self-evaluating-vectors
  ($make-thread-parameter #f (lambda (x) (and x #t))))

(set! $scheme-version (string->symbol ($format-scheme-version (constant scheme-version))))
)
