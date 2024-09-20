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

;;; cp0 is needed to optimize away run-time calls to record-constructor,
;;; record-predicate, etc., in define-record-type for rcd.
(eval-when (compile) (run-cp0 (default-run-cp0)))

;;; TODO:
;;; indirect flag for $record{,-ref,-set!}
;;; gc support for indirect records
;;; examples/foreign.ss support for (indirect) records
;;; support for more datatypes
;;; SWIG converter?
;;; include size of tag in record size OR don't include tag in record offsets

(let ()
  (define (rtd-ancestry x) ($object-ref 'scheme-object x (constant record-type-ancestry-disp)))
  (define (rtd-parent x) (let ([a (rtd-ancestry x)])
                           (vector-ref a (fx- (vector-length a) (constant ancestry-parent-offset)))))
  (define (rtd-size x) ($object-ref 'scheme-object x (constant record-type-size-disp)))
  (define (rtd-pm x) ($object-ref 'scheme-object x (constant record-type-pm-disp)))
  (define (rtd-mpm x) ($object-ref 'scheme-object x (constant record-type-mpm-disp)))
  (define (rtd-name x) ($object-ref 'scheme-object x (constant record-type-name-disp)))
  (define (rtd-flds x) ($object-ref 'scheme-object x (constant record-type-flds-disp)))
  (define (rtd-flags x) ($object-ref 'scheme-object x (constant record-type-flags-disp)))
  (define (rtd-uid x) ($object-ref 'scheme-object x (constant record-type-uid-disp)))

  (define (child-flds rtd)
    (let ([flds (rtd-flds rtd)] [prtd (rtd-parent rtd)])
      (if prtd
          (let ([p-flds (rtd-flds prtd)])
            (if (fixnum? flds)
                (fx- flds p-flds)
                (list-tail flds (length p-flds))))
          flds)))

  ;; assumes anonymous fields
  (define (parent-flds rtd)
    (let ([prtd (rtd-parent rtd)])
      (if prtd
          (rtd-flds prtd)
          0)))

  (define-syntax native-endianness-case
    (lambda (stx)
      (syntax-case stx (big little)
        [(_ [(big) b ...] [(little) l ...])
         #`(constant-case native-endianness
             [(big) b ...]
             [(little) l ...]
             [(unknown)
              (case (native-endianness)
                [(big) b ...]
                [(little) l ...])])])))

  (define-syntax build-multi-int
    (lambda (stx)
      (syntax-case stx ()
        [(moi (ref/set r offset arg ...) signed wide-bits narrow-bits swap?)
         #`(moi (ref/set r offset arg ...) signed wide-bits 0 narrow-bits swap?)]
        [(moi (ref/set r offset arg ...) signed wide-bits middle-bits narrow-bits swap?)
         (let ([mk (lambda (base n)
                     (datum->syntax #'moi (string->symbol (format "~a-~a" base n))))])
           (cond
             [(or (not (datum swap?))
                  (fx= (datum wide-bits) (datum narrow-bits)))
              (with-syntax ([signed-wide (mk (datum signed) (datum wide-bits))]
                            [unsigned-wide (mk 'unsigned (datum wide-bits))]
                            [unsigned-middle (mk 'unsigned (datum middle-bits))]
                            [signed-narrow (mk (datum signed) (datum narrow-bits))]
                            [unsigned-narrow (mk 'unsigned (datum narrow-bits))]
                            [wide-bytes (fxsrl (datum wide-bits) 3)]
                            [middle-bytes (fxsrl (datum middle-bits) 3)]
                            [narrow-bytes (fxsrl (datum narrow-bits) 3)])
                (with-syntax ([big-case
                               (cond
                                 [(null? #'(arg ...))
                                  ;; ref mode
                                  #`(logor
                                     (ash (ref/set 'signed-wide r offset) (+ narrow-bits middle-bits))
                                     #,(if (zero? (datum middle-bits))
                                           #`0
                                           #`(ash (ref/set 'unsigned-middle r (fx+ offset wide-bytes)) narrow-bits))
                                     (ref/set 'unsigned-narrow r (fx+ offset wide-bytes middle-bytes)))]
                                 [else
                                  ;; set mode
                                  #`(begin
                                      (ref/set 'signed-wide r offset
                                               (bitwise-arithmetic-shift-right arg ... (+ narrow-bits middle-bits)))
                                      #,(if (zero? (datum middle-bits))
                                            #`(void)
                                            #`(ref/set 'unsigned-middle r (fx+ offset wide-bytes)
                                                       (logand (bitwise-arithmetic-shift-right arg ... narrow-bits)
                                                               (- (expt 2 middle-bits) 1))))
                                      (ref/set 'unsigned-narrow r (fx+ offset middle-bytes wide-bytes)
                                               (logand arg ... (- (expt 2 narrow-bits) 1))))])]
                              [little-case
                               (cond
                                 [(null? #'(arg ...))
                                  ;; ref mode
                                  #`(logor
                                     (ref/set 'unsigned-wide r offset)
                                     #,(if (zero? (datum middle-bits))
                                           0
                                           #`(ash (ref/set 'unsigned-middle r (fx+ offset wide-bytes)) wide-bits))
                                     (ash (ref/set 'signed-narrow r (fx+ offset wide-bytes middle-bytes)) (+ wide-bits middle-bits)))]
                                 [else
                                  ;; set mode
                                  #`(begin
                                      (ref/set 'unsigned-wide r offset
                                               (logand arg ... (- (expt 2 wide-bits) 1)))
                                      #,(if (zero? (datum middle-bits))
                                            #`(void)
                                            #`(ref/set 'unsigned-middle r (fx+ offset wide-bytes)
                                                       (logand (bitwise-arithmetic-shift-right arg ... wide-bits)
                                                               (- (expt 2 middle-bits) 1))))
                                      (ref/set 'signed-narrow r (fx+ offset middle-bytes wide-bytes)
                                               (bitwise-arithmetic-shift-right arg ... (+ wide-bits middle-bits))))])])
                  (if (not (datum swap?))
                      #'(native-endianness-case
                         [(big) big-case]
                         [(little) little-case])
                      #'(native-endianness-case
                         [(big) little-case]
                         [(little) big-case]))))]
             [else
              ;; For general swap mode, perform a sequence of byte reads or writes
              (let ([mk (lambda (big?)
                          (let* ([bits (+ (datum wide-bits) (datum middle-bits) (datum narrow-bits))]
                                 [bytes (fxsrl bits 3)])
                            (let gen ([bits bits]
                                      [type (mk (datum signed) 8)]
                                      [shift (- bits 8)]
                                      [delta (if big? 0 (- bytes 1))])
                              (cond
                                [(= bits 8)
                                 (cond
                                   [(null? #'(arg ...))
                                    ;; ref mode
                                    #`(ash (ref/set '#,type r (fx+ offset #,delta)) #,shift)]
                                   [else
                                    ;; set mode
                                    #`(ref/set '#,type r (fx+ offset #,delta) (logand #xff (bitwise-arithmetic-shift-right arg ... #,shift)))])]
                                [else
                                 #`(#,(if (null? #'(arg ...)) #'logor #'begin)
                                     #,(gen 8 type shift delta)
                                     #,(gen (- bits 8) (mk 'unsigned 8) (- shift 8) (+ delta (if big? 1 -1))))]))))])
                #`(native-endianness-case
                   [(big) #,(mk #f)]
                   [(little) #,(mk #t)]))]))])))

  ; $record is hand-coded and is defined in prims.ss

  (let ([addr? (constant-case ptr-bits
                 [(32) $integer-32?]
                 [(64) $integer-64?])])
    (set-who! foreign-alloc
      (let ([malloc (foreign-procedure "(cs)malloc" (fixnum) uptr)])
        (lambda (n)
          (unless (and (fixnum? n) (fx> n 0))
            ($oops who "~s is not a positive fixnum" n))
          (malloc n))))

    (set-who! foreign-free
      (let ([free (foreign-procedure "(cs)free" (uptr) void)])
        (lambda (addr)
          (unless (addr? addr) ($oops who "invalid foreign address ~s" addr))
          (free addr))))

    (let ()
      (define (check-args who ty addr offset)
        (define-syntax check-ending-addr
          (syntax-rules ()
            [(_ type bytes pred)
             (unless (addr? (+ addr offset (fx- bytes 1)))
               ($oops who "invalid effective address (+ ~s ~s) for ~s-byte type ~s" addr offset bytes 'type))]))
        (unless (addr? addr) ($oops who "invalid address ~s" addr))
        (unless (fixnum? offset) ($oops who "~s is not a fixnum" offset))
        (unless (addr? (+ addr offset)) ($oops who "invalid effective address (+ ~s ~s)" addr offset))
        (record-datatype cases (filter-foreign-type ty) check-ending-addr
          ($oops who "unrecognized type ~s" ty)))
      (let ()
        (define-syntax set-foreign-ref!
          (syntax-rules ()
            [(_ foreign-ref swap?)
             (set-who! foreign-ref  ; checks ty, addr, and offset, but inherently unsafe
               (lambda (ty addr offset)
                 (define-syntax ref
                   (syntax-rules (scheme-object char wchar boolean stdbool
                                                integer-24 unsigned-24 integer-40 unsigned-40 integer-48 unsigned-48
                                                integer-56 unsigned-56 integer-64 unsigned-64)
                     [(_ scheme-object bytes pred) ($oops who "cannot load scheme pointers from foreign memory")]
                     [(_ char bytes pred) (integer->char (#3%foreign-ref 'unsigned-8 addr offset))]
                     [(_ wchar bytes pred)
                      (constant-case wchar-bits
                        [(16) (integer->char (#3%foreign-ref 'unsigned-16 addr offset))]
                        [(32) (integer->char (#3%foreign-ref 'unsigned-32 addr offset))])]
                     [(_ boolean bytes pred)
                      (constant-case int-bits
                        [(32) (not (eq? (#3%foreign-ref 'integer-32 addr offset) 0))]
                        [(64) (not (eq? (#3%foreign-ref 'integer-64 addr offset) 0))])]
                     [(_ stdbool bytes pred)
                      (constant-case stdbool-bits
                        [(8) (not (eq? (#3%foreign-ref 'integer-8 addr offset) 0))])]
                     [(_ integer-24 bytes pred)
                      (eq? 'unknown (constant native-endianness))
                      (build-multi-int (#3%foreign-ref addr offset) integer 16 8 swap?)]
                     [(_ unsigned-24 bytes pred)
                      (eq? 'unknown (constant native-endianness))
                      (build-multi-int (#3%foreign-ref addr offset) unsigned 16 8 swap?)]
                     [(_ integer-40 bytes pred)
                      (eq? 'unknown (constant native-endianness))
                      (build-multi-int (#3%foreign-ref addr offset) integer 32 8 swap?)]
                     [(_ unsigned-40 bytes pred)
                      (eq? 'unknown (constant native-endianness))
                      (build-multi-int (#3%foreign-ref addr offset) unsigned 32 8 swap?)]
                     [(_ integer-48 bytes pred)
                      (eq? 'unknown (constant native-endianness))
                      (build-multi-int (#3%foreign-ref addr offset) integer 32 16 swap?)]
                     [(_ unsigned-48 bytes pred)
                      (eq? 'unknown (constant native-endianness))
                      (build-multi-int (#3%foreign-ref addr offset) unsigned 32 16 swap?)]
                     [(_ integer-56 bytes pred)
                      (eq? 'unknown (constant native-endianness))
                      (build-multi-int (#3%foreign-ref addr offset) integer 32 16 8 swap?)]
                     [(_ unsigned-56 bytes pred)
                      (eq? 'unknown (constant native-endianness))
                      (build-multi-int (#3%foreign-ref addr offset) unsigned 32 16 8 swap?)]
                     [(_ integer-64 bytes pred)
                      (< (constant ptr-bits) 64)
                      (build-multi-int (#3%foreign-ref addr offset) integer 32 32 swap?)]
                     [(_ unsigned-64 bytes pred)
                      (< (constant ptr-bits) 64)
                      (build-multi-int (#3%foreign-ref addr offset) unsigned 32 32 swap?)]
                     [(_ type bytes pred) (#3%foreign-ref 'type addr offset)]))
                 (check-args who ty addr offset)
                 (record-datatype cases (filter-foreign-type ty) ref
                                  ($oops who "unrecognized type ~s" ty))))]))
        (set-foreign-ref! foreign-ref #f)
        ;; Only used for slow cases of `$fptr-ref-...`
        (set-foreign-ref! $foreign-swap-ref #t))

      (let ()
        (define-syntax set-foreign-set!!
          (syntax-rules ()
            [(_ foreign-set! swap?)
             (set-who! foreign-set! ; checks ty, addr, offset, and v, but inherently unsafe
               (lambda (ty addr offset v)
                 (define (value-err x t) ($oops who "invalid value ~s for foreign type ~s" x t))
                 (define-syntax set
                   (syntax-rules (scheme-object char wchar boolean stdbool
                                                integer-24 unsigned-24 integer-40 unsigned-40 integer-48 unsigned-48
                                                integer-56 unsigned-56 integer-64 unsigned-64 double-float single-float)
                     [(_ scheme-object bytes pred) ($oops who "cannot store scheme pointers into foreign memory")]
                     [(_ char bytes pred)
                      (begin
                        (unless (pred v) (value-err v ty))
                        (#3%foreign-set! 'unsigned-8 addr offset (char->integer v)))]
                     [(_ wchar bytes pred)
                      (begin
                        (unless (pred v) (value-err v ty))
                        (constant-case wchar-bits
                          [(16) (#3%foreign-set! 'unsigned-16 addr offset (char->integer v))]
                          [(32) (#3%foreign-set! 'unsigned-32 addr offset (char->integer v))]))]
                     [(_ boolean bytes pred)
                      (constant-case int-bits
                        [(32) (#3%foreign-set! 'integer-32 addr offset (if v 1 0))]
                        [(64) (#3%foreign-set! 'integer-64 addr offset (if v 1 0))])]
                     [(_ stdbool bytes pred)
                      (constant-case stdbool-bits
                        [(8) (#3%foreign-set! 'integer-8 addr offset (if v 1 0))])]
                     [(_ integer-24 bytes pred)
                      (eq? 'unknown (constant native-endianness))
                      (begin
                        (unless (pred v) (value-err v ty))
                        (build-multi-int (#3%foreign-set! addr offset v) integer 16 8 swap?))]
                     [(_ unsigned-24 bytes pred)
                      (eq? 'unknown (constant native-endianness))
                      (begin
                        (unless (pred v) (value-err v ty))
                        (build-multi-int (#3%foreign-set! addr offset v) unsigned 16 8 swap?))]
                     [(_ integer-40 bytes pred)
                      (or (< (constant ptr-bits) 64)
                          (eq? 'unknown (constant native-endianness)))
                      (begin
                        (unless (pred v) (value-err v ty))
                        (build-multi-int (#3%foreign-set! addr offset v) integer 32 8 swap?))]
                     [(_ unsigned-40 bytes pred)
                      (or (< (constant ptr-bits) 64)
                          (eq? 'unknown (constant native-endianness)))
                      (begin
                        (unless (pred v) (value-err v ty))
                        (build-multi-int (#3%foreign-set! addr offset v) unsigned 32 8 swap?))]
                     [(_ integer-48 bytes pred)
                      (or (< (constant ptr-bits) 64)
                          (eq? 'unknown (constant native-endianness)))
                      (begin
                        (unless (pred v) (value-err v ty))
                        (build-multi-int (#3%foreign-set! addr offset v) integer 32 16 swap?))]
                     [(_ unsigned-48 bytes pred)
                      (or (< (constant ptr-bits) 64)
                          (eq? 'unknown (constant native-endianness)))
                      (begin
                        (unless (pred v) (value-err v ty))
                        (build-multi-int (#3%foreign-set! addr offset v) unsigned 32 16 swap?))]
                     [(_ integer-56 bytes pred)
                      (or (< (constant ptr-bits) 64)
                          (eq? 'unknown (constant native-endianness)))
                      (begin
                        (unless (pred v) (value-err v ty))
                        (build-multi-int (#3%foreign-set! addr offset v) integer 32 16 8 swap?))]
                     [(_ unsigned-56 bytes pred)
                      (or (< (constant ptr-bits) 64)
                          (eq? 'unknown (constant native-endianness)))
                      (begin
                        (unless (pred v) (value-err v ty))
                        (build-multi-int (#3%foreign-set! addr offset v) unsigned 32 16 8 swap?))]
                     [(_ integer-64 bytes pred)
                      (< (constant ptr-bits) 64)
                      (begin
                        (unless (pred v) (value-err v ty))
                        (build-multi-int (#3%foreign-set! addr offset v) integer 32 32 swap?))]
                     [(_ unsigned-64 bytes pred)
                      (< (constant ptr-bits) 64)
                      (begin
                        (unless (pred v) (value-err v ty))
                        (build-multi-int (#3%foreign-set! addr offset v) unsigned 32 32 swap?))]
                     [(_ double-float bytes pred)
                      (and swap? (< (constant ptr-bits) 64))
                      (begin
                        (unless (pred v) (value-err v ty))
                        (let ([bv (make-bytevector 8)])
                          (bytevector-ieee-double-native-set! bv 0 v)
                          (foreign-set! 'unsigned-32 addr offset (bytevector-u32-native-ref bv 0))
                          (foreign-set! 'unsigned-32 addr (fx+ offset 4) (bytevector-u32-native-ref bv 4))))]
                     [(_ single-float bytes pred)
                      swap?
                      (begin
                        (unless (pred v) (value-err v ty))
                        (let ([bv (make-bytevector 4)])
                          (bytevector-ieee-single-native-set! bv 0 v)
                          (foreign-set! 'unsigned-32 addr offset (bytevector-u32-native-ref bv 0))))]
                     [(_ type bytes pred)
                      (begin
                        (unless (pred v) (value-err v ty))
                        (#3%foreign-set! 'type addr offset v))]))
                 (check-args who ty addr offset)
                 (record-datatype cases (filter-foreign-type ty) set
                                  ($oops who "unrecognized type ~s" ty))))]))
        (set-foreign-set!! foreign-set! #f)
        ;; Only used for slow cases of `$fptr-set-...!`
        (set-foreign-set!! $foreign-swap-set! #t))))

  (set-who! $filter-foreign-type
    ; version that filters using host-machine information
    (lambda (ty)
      (filter-foreign-type ty)))

  (set-who! $object-ref ; not safe, just handles non-constant types
    (lambda (ty r offset)
      (define-syntax ref
        (syntax-rules (char wchar boolean stdbool
                            integer-24 unsigned-24 integer-40 unsigned-40 integer-48 unsigned-48
                            integer-56 unsigned-56 integer-64 unsigned-64)
          [(_ char bytes pred) (integer->char (#3%$object-ref 'unsigned-8 r offset))]
          [(_ wchar bytes pred)
           (constant-case wchar-bits
             [(16) (integer->char (#3%$object-ref 'unsigned-16 r offset))]
             [(32) (integer->char (#3%$object-ref 'unsigned-32 r offset))])]
          [(_ boolean bytes pred)
           (constant-case int-bits
             [(32) (not (eq? (#3%$object-ref 'integer-32 r offset) 0))]
             [(64) (not (eq? (#3%$object-ref 'integer-64 r offset) 0))])]
          [(_ stdbool bytes pred)
           (constant-case stdbool-bits
             [(8) (not (eq? (#3%$object-ref 'integer-8 r offset) 0))])]
          [(_ integer-24 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$object-ref r offset) integer 16 8 #f)]
          [(_ unsigned-24 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$object-ref r offset) unsigned 16 8 #f)]
          [(_ integer-40 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$object-ref r offset) integer 32 8 #f)]
          [(_ unsigned-40 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$object-ref r offset) unsigned 32 8 #f)]
          [(_ integer-48 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$object-ref r offset) integer 32 16 #f)]
          [(_ unsigned-48 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$object-ref r offset) unsigned 32 16 #f)]
          [(_ integer-56 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$object-ref r offset) integer 32 16 8 #f)]
          [(_ unsigned-56 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$object-ref r offset) unsigned 32 16 8 #f)]
          [(_ type bytes pred) (#3%$object-ref 'type r offset)]))
      (record-datatype cases (filter-foreign-type ty) ref
        ($oops who "unrecognized type ~s" ty))))

  (set-who! $swap-object-ref ; not safe, just handles non-constant types
    (lambda (ty r offset)
      (define-syntax ref
        (syntax-rules (char wchar boolean stdbool
                            integer-24 unsigned-24 integer-40 unsigned-40 integer-48 unsigned-48
                            integer-56 unsigned-56 integer-64 unsigned-64)
          [(_ char bytes pred) (integer->char (#3%$swap-object-ref 'unsigned-8 r offset))]
          [(_ wchar bytes pred)
           (constant-case wchar-bits
             [(16) (integer->char (#3%$swap-object-ref 'unsigned-16 r offset))]
             [(32) (integer->char (#3%$swap-object-ref 'unsigned-32 r offset))])]
          [(_ boolean bytes pred)
           (constant-case int-bits
             [(32) (not (eq? (#3%$swap-object-ref 'integer-32 r offset) 0))]
             [(64) (not (eq? (#3%$swap-object-ref 'integer-64 r offset) 0))])]
          [(_ stdbool bytes pred)
           (constant-case stdbool-bits
             [(8) (not (eq? (#3%$swap-object-ref 'integer-8 r offset) 0))])]
          [(_ integer-24 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$swap-object-ref r offset) integer 16 8 #t)]
          [(_ unsigned-24 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$swap-object-ref r offset) unsigned 16 8 #t)]
          [(_ integer-40 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$swap-object-ref r offset) integer 32 8 #t)]
          [(_ unsigned-40 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$swap-object-ref r offset) unsigned 16 8 #t)]
          [(_ integer-48 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$swap-object-ref r offset) integer 32 16 #t)]
          [(_ unsigned-48 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$swap-object-ref r offset) unsigned 16 16 #t)]
          [(_ integer-56 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$swap-object-ref r offset) integer 32 16 8 #t)]
          [(_ unsigned-56 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$swap-object-ref r offset) unsigned 32 16 8 #t)]
          [(_ type bytes pred) (#3%$swap-object-ref 'type r offset)]))
      (record-datatype cases (filter-foreign-type ty) ref
        ($oops who "unrecognized type ~s" ty))))

  (set-who! $object-set! ; not safe, just handles non-constant types
    (lambda (ty r offset v)
      (define-syntax set
        (syntax-rules (char wchar boolean stdbool
                            integer-24 unsigned-24 integer-40 unsigned-40 integer-48 unsigned-48
                            integer-56 unsigned-56 integer-64 unsigned-64)
          [(_ char bytes pred)
           (#3%$object-set! 'unsigned-8 r offset (char->integer v))]
          [(_ wchar bytes pred)
           (constant-case wchar-bits
             [(16) (#3%$object-set! 'unsigned-16 r offset (char->integer v))]
             [(32) (#3%$object-set! 'unsigned-32 r offset (char->integer v))])]
          [(_ boolean bytes pred)
           (constant-case int-bits
             [(32) (#3%$object-set! 'integer-32 r offset (if v 1 0))]
             [(64) (#3%$object-set! 'integer-64 r offset (if v 1 0))])]
          [(_ stdbool bytes pred)
           (constant-case stdbool-bits
             [(8) (#3%$object-set! 'integer-8 r offset (if v 1 0))])]
          [(_ integer-24 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$object-set! r offset v) integer 16 8 #f)]
          [(_ unsigned-24 bytes pred)
           (eq? 'unknown (constant native-endianness))
           (build-multi-int (#3%$object-set! r offset v) unsigned 16 8 #f)]
          [(_ integer-40 bytes pred)
           (or (< (constant ptr-bits) 64)
               (eq? 'unknown (constant native-endianness)))
           (build-multi-int (#3%$object-set! r offset v) integer 32 8 #f)]
          [(_ unsigned-40 bytes pred)
           (or (< (constant ptr-bits) 64)
               (eq? 'unknown (constant native-endianness)))
           (build-multi-int (#3%$object-set! r offset v) unsigned 32 8 #f)]
          [(_ integer-48 bytes pred)
           (or (< (constant ptr-bits) 64)
               (eq? 'unknown (constant native-endianness)))
           (build-multi-int (#3%$object-set! r offset v) integer 32 16 #f)]
          [(_ unsigned-48 bytes pred)
           (or (< (constant ptr-bits) 64)
               (eq? 'unknown (constant native-endianness)))
           (build-multi-int (#3%$object-set! r offset v) unsigned 32 16 #f)]
          [(_ integer-56 bytes pred)
           (or (< (constant ptr-bits) 64)
               (eq? 'unknown (constant native-endianness)))
           (build-multi-int (#3%$object-set! r offset v) integer 32 16 8 #f)]
          [(_ unsigned-56 bytes pred)
           (or (< (constant ptr-bits) 64)
               (eq? 'unknown (constant native-endianness)))
           (build-multi-int (#3%$object-set! r offset v) unsigned 32 16 8 #f)]
          [(_ integer-64 bytes pred)
           (< (constant ptr-bits) 64)
           (build-multi-int (#3%$object-set! r offset v) integer 32 32 #f)]
          [(_ unsigned-64 bytes pred)
           (< (constant ptr-bits) 64)
           (build-multi-int (#3%$object-set! r offset v) unsigned 32 32 #f)]
          [(_ type bytes pred) (#3%$object-set! 'type r offset v)]))
      (record-datatype cases (filter-foreign-type ty) set
        ($oops who "unrecognized type ~s" ty))))

  (set-who! foreign-sizeof
    (lambda (ty)
      (define-syntax size
        (syntax-rules ()
          [(_ type bytes pred) bytes]))
      (record-datatype cases (filter-foreign-type ty) size
        ($oops who "invalid foreign type specifier ~s" ty))))

  (set-who! foreign-alignof
    (lambda (ty)
      (define-syntax size
        (syntax-rules ()
          [(_ type bytes pred)
           ;; rely on cp0 expansion:
           (case 'type
             [(double-float) (foreign-alignof 'double)]
             [(single-float) (foreign-alignof 'float)]
             [(integer-64) (foreign-alignof 'integer-64)]
             [(unsigned-64) (foreign-alignof 'unsigned-64)]
             [else bytes])]))
      (record-datatype cases (filter-foreign-type ty) size
        ($oops who "invalid foreign type specifier ~s" ty))))

  (set-who! #(csv7: record-type-descriptor)
    (lambda (r)
      (unless (record? r) ($oops who "~s is not a record" r))
      (#3%record-rtd r)))

  (set-who! record-rtd
    (lambda (r)
      (unless (record? r) ($oops who "~s is not a record" r))
      (#3%record-rtd r)))

  (set! record-predicate
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
        ($oops 'record-predicate "~s is not a record type descriptor" rtd))
      (if (record-type-sealed? rtd)
          (rec predicate (lambda (x) ($sealed-record? x rtd)))
          (rec predicate (lambda (x) (record? x rtd))))))

  (let ((base-rtd #!base-rtd))
    (define (make-flags uid sealed? opaque? parent)
      (fxlogor
        (if uid 0 (constant rtd-generative))
        (if (or opaque? (and parent (record-type-opaque? parent)))
            (constant rtd-opaque)
            0)
        (if sealed? (constant rtd-sealed) 0)))
    (define ($mrt who base-rtd name parent uid flags fields anonymous-fields? extras)
      (include "layout.ss")
      (when parent
        (when ($record-type-act-sealed? parent)
          ($oops who "cannot extend sealed record type ~s as ~s" parent name))
        (if anonymous-fields?
            (unless (fixnum? (rtd-flds parent))
              ($oops who "cannot make anonymous-field record type ~s from named-field parent record type ~s" name parent))
            (when (fixnum? (rtd-flds parent))
              ($oops who "cannot make named-field record type ~s from anonymous-field parent record type ~s" name parent))))
      (let ([uid (or uid ((current-generate-id) name))])
       ; start base offset at rtd field
       ; synchronize with syntax.ss and front.ss
        (let-values ([(pm mpm flds size)
                      (if anonymous-fields?
                          (let ([parent-n (if parent
                                              (let ([p-flds (rtd-flds parent)])
                                                (if (fixnum? p-flds)
                                                    p-flds
                                                    (length p-flds)))
                                              0)]
                                [fields (car fields)]
                                [mutability-mask (cdr fields)])
                            (unless (< (+ fields parent-n 1) (fxsrl (most-positive-fixnum) (constant log2-ptr-bytes)))
                              ($oops who "cannot make record type with ~s fields" (+ fields parent-n)))
                            (compute-field-offsets who
                              (constant record-type-disp)
                              (fx+ parent-n fields 1)
                              (+ (bitwise-arithmetic-shift-left mutability-mask (fx+ parent-n 1))
                                 (if parent (rtd-mpm parent) 0))))
                          (let ([parent-fields (if (not parent) '() (csv7:record-type-field-decls parent))])
                            (compute-field-offsets who
                              (constant record-type-disp)
                              ; rtd must be immutable if we are ever to store records
                              ; in space pure
                              (cons `(immutable scheme-object ,uid)
                                    (append parent-fields fields)))))])
          (cond
            [(and (not (fxlogtest flags (constant rtd-generative)))
                  (let ([x ($sgetprop uid '*rtd* #f)])
                    (and (record-type-descriptor? x) x))) =>
             (lambda (rtd)
               (define same-fields?
                 (lambda (flds1 flds2)
                   (define same-field?
                     (lambda (fld1 fld2) ; mutability checked separately
                       (and (eq? (fld-name fld1) (fld-name fld2))
                           ; not using filter-foreign-type here.  this makes the
                           ; comparison faster and prevents unwanted machine-dependent
                           ; matches like int and integer-32.  it also prevents
                           ; ptr and scheme-object from matching---c'est la vie.
                            (eq? (fld-type fld1) (fld-type fld2))
                           ; following is paranoid; overall size
                           ; check should suffice
                            #;(= (fld-byte fld1) (fld-byte fld2)))))
                   (or (and (fixnum? flds1) (fixnum? flds2) (fx= flds1 flds2))
                       (and (not (fixnum? flds1)) (not (fixnum? flds2))
                            (fx= (length flds1) (length flds2))
                            (andmap same-field? flds1 flds2)))))
              ; following assumes extras match
               (let ()
                 (define (squawk what) ($oops who "incompatible record type ~s - ~a" name what))
                 (unless (eq? ($record-type-descriptor rtd) base-rtd) (squawk "different base rtd"))
                 (unless (eq? (rtd-parent rtd) parent) (squawk "different parent"))
                 (unless (same-fields? (rtd-flds rtd) (if (pair? flds) (cdr flds) (fx- flds 1))) (squawk "different fields"))
                 (unless (= (rtd-mpm rtd) mpm) (squawk "different mutability"))
                 (unless (fx= (rtd-flags rtd) flags) (squawk "different flags"))
                 (unless (eq? (rtd-size rtd) size) (squawk "different size")))
               rtd)]
            [else
             (let* ([len (if (not parent) 1 (vector-length (rtd-ancestry parent)))]
                    [ancestry (make-vector (fx+ 1 len) #f)])
               (let loop ([i 1])
                 (unless (fx= i len)
                   (vector-set! ancestry i (vector-ref (rtd-ancestry parent) i))
                   (loop (fx+ i 1))))
               (let ([rtd (apply #%$record base-rtd ancestry size pm mpm name
                                 (if (pair? flds) (cdr flds) (fx- flds 1)) flags uid #f extras)])
                 (vector-set! ancestry len rtd)
                 (with-tc-mutex ($sputprop uid '*rtd* rtd))
                 rtd))]))))

    (set-who! $remake-rtd
      (lambda (rtd compute-field-offsets)
        (let ([key ($target-machine)] [uid (rtd-uid rtd)])
          (assert (not (eq? key (machine-type))))
          (or ($sgetprop uid key #f)
              (let ([base-rtd ($record-type-descriptor rtd)]
                    [ancestry (rtd-ancestry rtd)]
                    [name (rtd-name rtd)]
                    [flags (rtd-flags rtd)]
                    [old-flds (rtd-flds rtd)])
                (let-values ([(pm mpm flds size)
                              (if (fixnum? old-flds)
                                  (compute-field-offsets who
                                    (constant record-type-disp)
                                    (fx+ old-flds 1) (rtd-mpm rtd))
                                  (let ([fields (csv7:record-type-field-decls rtd)])
                                    (compute-field-offsets who
                                      (constant record-type-disp)
                                      (cons `(immutable scheme-object ,uid) fields))))])
                  (define (share-with-remade-parent flds)
                    (let ([old-parent (rtd-parent rtd)])
                      (if (and old-parent
                               (not (eq? old-parent #!base-rtd)))
                          (let ([parent ($remake-rtd old-parent compute-field-offsets)])
                            (let loop ([flds flds]
                                       [old-flds old-flds]
                                       [parent-flds (rtd-flds parent)]
                                       [parent-old-flds (rtd-flds old-parent)])
                              (cond
                                [(null? parent-flds) flds]
                                [else
                                 (safe-assert (equal? (car flds) (car parent-flds)))
                                 (safe-assert (equal? (car old-flds) (car parent-old-flds)))
                                 (cons (if (eq? (car old-flds) (car parent-old-flds))
                                           (car parent-flds)
                                           (car flds))
                                       (loop (cdr flds) (cdr old-flds) (cdr parent-flds) (cdr parent-old-flds)))])))
                          flds)))
                  (let ([rtd (apply #%$record base-rtd ancestry size pm mpm name
                               (if (pair? flds)
                                   (share-with-remade-parent (cdr flds))
                                   (fx- flds 1))
                               flags uid #f
                               (let* ([n (length (rtd-flds ($record-type-descriptor base-rtd)))]
                                      [ls (list-tail (rtd-flds base-rtd) n)])
                                 (let f ([n n] [ls ls])
                                   (if (null? ls)
                                       '()
                                       (cons ((csv7:record-field-accessor base-rtd n) rtd)
                                         (f (fx+ n 1) (cdr ls)))))))])
                    (with-tc-mutex ($sputprop uid key rtd))
                    rtd)))))))

    (let ()
      (define (mrt base-rtd parent name fields sealed? opaque? extras)
        (cond
          [(gensym? name)
           ($mrt 'make-record-type base-rtd
             (string->symbol (symbol->string name)) parent name
             (make-flags name sealed? opaque? parent)
             fields #f extras)]
          [(string? name)
           ($mrt 'make-record-type base-rtd
             (string->symbol name) parent #f
             (make-flags #f sealed? opaque? parent)
             fields #f extras)]
          [else ($oops 'make-record-type "invalid record name ~s" name)]))

      (set-who! make-record-type
        (rec make-record-type
          (case-lambda
            [(name fields)
             (unless (list? fields)
               ($oops who "invalid field list ~s" fields))
             (mrt base-rtd #f name fields #f #f '())]
            [(parent name fields)
             (unless (or (not parent) (record-type-descriptor? parent))
               ($oops who "~s is not a record type descriptor"
                 parent))
             (unless (list? fields)
               ($oops who "invalid field list ~s" fields))
             (mrt base-rtd parent name fields #f #f '())])))

      (set! $make-record-type
        (lambda (base-rtd parent name fields sealed? opaque? . extras)
          (unless (record-type-descriptor? base-rtd)
            ($oops 'make-record-type "~s is not a record type descriptor"
              base-rtd))
          (unless (or (not parent) (record-type-descriptor? parent))
            ($oops 'make-record-type "~s is not a record type descriptor"
              parent))
          (unless (list? fields)
            ($oops 'make-record-type "invalid field list ~s" fields))
          (mrt base-rtd parent name fields sealed? opaque? extras))))

    (let ()
      (define (mrtd base-rtd name parent uid sealed? opaque? fields anon-ok? who extras)
        (unless (symbol? name)
          ($oops who "invalid record name ~s" name))
        (unless (or (not parent) (record-type-descriptor? parent))
          ($oops who "invalid parent ~s" parent))
        (unless (or (not uid) (symbol? uid))
          ($oops who "invalid uid ~s" uid))
        (if anon-ok?
            (unless (or (pair? fields)
                        (vector? fields))
              ($oops who "invalid field vector or pair ~s" fields))
            (unless (vector? fields)
              ($oops who "invalid field vector ~s" fields)))
        ($mrt who base-rtd name parent uid
              (make-flags uid sealed? opaque? parent)
              (cond
                [(and anon-ok?
                      (pair? fields))
                 (let ([fields (car fields)]
                       [mutability-mask (cdr fields)])
                   (unless (and (fixnum? fields)
                                (fx>= fields 0))
                     ($oops who "invalid field count ~s" fields))
                   (unless (and (or (fixnum? mutability-mask) (bignum? mutability-mask))
                                (eqv? 0 (bitwise-arithmetic-shift-right mutability-mask fields)))
                     ($oops who "invalid mutability mask ~s for field count ~s" mutability-mask fields)))
                 fields]
                [else
                 (let ([n (vector-length fields)])
                   (let f ([i 0])
                     (if (fx= i n)
                         '()
                         (let ([x (vector-ref fields i)])
                           (unless (and (pair? x)
                                        (memq (car x) '(mutable immutable))
                                        (let ([x (cdr x)])
                                          (and (pair? x)
                                               (symbol? (car x))
                                               (null? (cdr x)))))
                             ($oops who "invalid field specifier ~s" x))
                           (cons x (f (fx+ i 1)))))))])
              (pair? fields)
              extras))

      (set! $make-record-type-descriptor
        (case-lambda
         [(base-rtd name parent uid sealed? opaque? fields who . extras)
          (unless (record-type-descriptor? base-rtd)
            ($oops who "invalid base rtd ~s" base-rtd))
          (mrtd base-rtd name parent uid sealed? opaque? fields #t who extras)]))

      (set-who! make-record-type-descriptor
        (case-lambda
         [(name parent uid sealed? opaque? fields)
          (mrtd base-rtd name parent uid sealed? opaque? fields #t who '())]))

      (set-who! r6rs:make-record-type-descriptor
        (case-lambda
         [(name parent uid sealed? opaque? fields)
          (mrtd base-rtd name parent uid sealed? opaque? fields #f who '())])))

    (set! record-type-descriptor?
      (lambda (x)
        (#3%record? x base-rtd)))

    (set! record?
      (case-lambda
        [(x) (#3%record? x)]
        [(x rtd)
         (unless (#3%record? rtd base-rtd)
           ($oops 'record? "~s is not a record type descriptor" rtd))
         (#3%record? x rtd)])))

  (set! r6rs:record?
    (rec record?
      (lambda (x)
        (#3%r6rs:record? x))))

  (set! record-type-parent
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
        ($oops 'record-type-parent "~s is not a record type descriptor" rtd))
      (rtd-parent rtd)))

  (set-who! #(csv7: record-type-name)
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
        ($oops who "~s is not a record type descriptor" rtd))
      (symbol->string (rtd-name rtd))))

  (set-who! record-type-name
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
        ($oops who "~s is not a record type descriptor" rtd))
      (rtd-name rtd)))

  (set-who! #(csv7: record-type-symbol)
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
        ($oops who "~s is not a record type descriptor" rtd))
      (rtd-uid rtd)))

  (set-who! record-type-uid
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
        ($oops who "~s is not a record type descriptor" rtd))
      (rtd-uid rtd)))

  (set-who! record-type-has-named-fields?
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
        ($oops who "~s is not a record type descriptor" rtd))
      (not (fixnum? (rtd-flds rtd)))))

  (set-who! #(csv7: record-type-field-names)
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
        ($oops who "~s is not a record type descriptor" rtd))
      (let ([flds (rtd-flds rtd)])
        (if (fixnum? flds)
            (let loop ([i flds]) (if (fx= i 0) '() (cons 'field (loop (fx- i 1)))))
            (map (lambda (x) (fld-name x)) flds)))))

  (set-who! record-type-field-names
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
        ($oops who "~s is not a record type descriptor" rtd))
      (let ([flds (child-flds rtd)])
        (if (fixnum? flds)
            (make-vector flds 'field)
            (list->vector (map (lambda (x) (fld-name x)) flds))))))

  (set-who! #(csv7: record-type-field-decls)
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
        ($oops who "~s is not a record type descriptor" rtd))
      (let ([flds (rtd-flds rtd)])
        (if (fixnum? flds)
            (let loop ([flds flds])
              (if (fx= 0 flds)
                  '()
                  (cons '(mutable scheme-object unknown) (loop (fx- flds 1)))))
            (map (lambda (x)
                   `(,(if (fld-mutable? x) 'mutable 'immutable)
                     ,(fld-type x)
                     ,(fld-name x)))
                 flds)))))

  (set! $record-type-field-offsets
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
        ($oops '$record-type-field-offsets "~s is not a record type descriptor" rtd))
      (let ([flds (rtd-flds rtd)])
        (if (fixnum? flds)
            (let loop ([i flds])
              (if (fx= i flds)
                  '()
                  (cons (fx+ (constant record-data-disp) (fx* i (constant ptr-bytes)))
                        (loop (fx+ i 1)))))
            (map (lambda (x) (fld-byte x)) (rtd-flds rtd))))))

  (set! record-type-opaque?
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
        ($oops 'record-type-opaque? "~s is not a record type descriptor" rtd))
      (#3%record-type-opaque? rtd)))

  (set! record-type-sealed?
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
        ($oops 'record-type-sealed? "~s is not a record type descriptor" rtd))
      (#3%record-type-sealed? rtd)))

  (set-who! $record-type-act-sealed!
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
        ($oops who "~s is not a record type descriptor" rtd))
      (unless ($record-type-act-sealed? rtd)
        ($object-set! 'scheme-object rtd (constant record-type-flags-disp)
                      (fxior (rtd-flags rtd) (constant rtd-act-sealed))))))

  (set-who! $record-type-act-sealed?
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
        ($oops who "~s is not a record type descriptor" rtd))
      (#3%$record-type-act-sealed? rtd)))

  (set! record-type-generative?
    (lambda (rtd)
      (unless (record-type-descriptor? rtd)
        ($oops 'record-type-generative? "~s is not a record type descriptor" rtd))
      (#3%record-type-generative? rtd)))

  (let ()
    (define (make-default-fld field-spec mpm)
      (make-fld 'field
                (bitwise-bit-set? mpm (fx+ field-spec 1))
                'scheme-object
                (fx+ (constant record-data-disp) (fx* field-spec (constant ptr-bytes)))))
    (define (find-fld who rtd field-spec)
      (unless (record-type-descriptor? rtd)
        ($oops who "~s is not a record type descriptor" rtd))
      (cond
        [(symbol? field-spec)
        ; reverse order to check child's fields first
         (let loop ((flds (let ([flds (rtd-flds rtd)])
                            (if (fixnum? flds)
                                '()
                                (reverse flds)))))
           (when (null? flds)
             ($oops who "unrecognized field name ~s for type ~s"
               field-spec rtd))
           (let ((fld (car flds)))
             (if (eq? field-spec (fld-name fld))
                 fld
                 (loop (cdr flds)))))]
        [(and (fixnum? field-spec) (fx>= field-spec 0))
         (let* ((flds (rtd-flds rtd))
                (n-flds (if (fixnum? flds) flds (length flds))))
           (when (fx>= field-spec n-flds)
             ($oops who "invalid field ordinal ~s for type ~s"
               field-spec rtd))
           (if (fixnum? flds)
               (make-default-fld field-spec (rtd-mpm rtd))
               (list-ref flds field-spec)))]
        [else ($oops who "invalid field specifier ~s" field-spec)]))

    (define (r6rs:find-fld who rtd field-spec)
      (unless (record-type-descriptor? rtd)
        ($oops who "~s is not a record type descriptor" rtd))
      (cond
        [(and (fixnum? field-spec) (fx>= field-spec 0))
         (let* ((flds (child-flds rtd))
                (n-flds (if (fixnum? flds) flds (length flds))))
           (when (fx>= field-spec n-flds)
             ($oops who "invalid field index ~s for type ~s"
               field-spec rtd))
           (if (fixnum? flds)
               (make-default-fld (fx+ field-spec (parent-flds rtd)) (rtd-mpm rtd))
               (list-ref flds field-spec)))]
        [else ($oops who "invalid field specifier ~s" field-spec)]))

    (let ()
      (define (rfa who rtd fld)
        (let ((record-err (lambda (x) ($record-oops #f x rtd)))
              (offset (fld-byte fld))
              (ty (fld-type fld)))
          (define-syntax ref
            (syntax-rules ()
              [(_ type bytes pred)
               (rec accessor
                 (lambda (x)
                   (unless (record? x rtd) (record-err x))
                   (#3%$object-ref 'type x offset)))]))
          (record-datatype cases (filter-foreign-type ty) ref
            ($oops who "unrecognized type ~s" ty))))
      (set-who! #(csv7: record-field-accessor)
        (lambda (rtd field-spec)
          (rfa who rtd (find-fld who rtd field-spec))))
      (set-who! record-accessor
        (lambda (rtd field-spec)
          (rfa who rtd (r6rs:find-fld who rtd field-spec)))))

    (let ()
      (define (rfm who rtd fld field-spec)
        (if (fld-mutable? fld)
            (let ((record-err (lambda (x t) ($record-oops #f x t)))
                  (value-err (lambda (x t) ($oops #f "invalid value ~s for foreign type ~s" x t)))
                  (offset (fld-byte fld))
                  (ty (fld-type fld)))
              (define-syntax set
                (syntax-rules (scheme-object)
                  [(_ scheme-object bytes pred)
                   (rec mutator
                     (lambda (x v)
                       (unless (record? x rtd) (record-err x rtd))
                       (#3%$object-set! 'scheme-object x offset v)))]
                  [(_ type bytes pred)
                   (rec mutator
                     (lambda (x v)
                       (unless (record? x rtd) (record-err x rtd))
                       (unless (pred v) (value-err v ty))
                       (#3%$object-set! 'type x offset v)))]))
              (record-datatype cases (filter-foreign-type ty) set
                ($oops who "unrecognized type ~s" ty)))
            ($oops who "field ~s of ~s is immutable"
              field-spec rtd)))
      (set-who! #(csv7: record-field-mutator)
        (lambda (rtd field-spec)
          (rfm who rtd (find-fld who rtd field-spec) field-spec)))
      (set-who! record-mutator
        (lambda (rtd field-spec)
          (rfm who rtd (r6rs:find-fld who rtd field-spec) field-spec))))

    (set-who! #(csv7: record-field-accessible?)
     ; if this is ever made to do anything reasonable, revisit handlers in
     ; cp0 and cp1in as well
      (lambda (rtd field-spec)
        (find-fld who rtd field-spec)
        #t))

    (set-who! #(csv7: record-field-mutable?)
      (lambda (rtd field-spec)
        (cond
         [(and (fixnum? field-spec)
               (record-type-descriptor? rtd))
          ;; Try fast path
          (let ([flds (rtd-flds rtd)])
            (cond
             [(and (fixnum? flds)
                   ($fxu< field-spec flds))
              (bitwise-bit-set? (rtd-mpm rtd) (fx+ field-spec 1))]
             [else
              (fld-mutable? (find-fld who rtd field-spec))]))]
         [else (fld-mutable? (find-fld who rtd field-spec))])))

    (set-who! record-field-mutable?
      (lambda (rtd field-spec)
        (fld-mutable? (r6rs:find-fld who rtd field-spec)))))

  (let ()
   ; if you update this, also update duplicate in cp0.ss
    (define-record-type rcd
      (fields (immutable rtd) (immutable prcd) (immutable protocol))
      (nongenerative #{rcd qh0yzh5qyrxmz2l-a})
      (sealed #t))

    (set! record-constructor-descriptor?
      (lambda (x)
        (rcd? x)))

    (let ()
      (define (mrcd rtd prcd protocol who)
        (unless (record-type-descriptor? rtd)
          ($oops who "~s is not a record-type descriptor" rtd))
        (unless (or (not prcd) (rcd? prcd))
          ($oops who "invalid record constructor descriptor ~s" prcd))
        (unless (or (not protocol) (procedure? protocol))
          ($oops who "invalid protocol ~s" protocol))
        (unless (eqv? (rtd-pm rtd) -1) ; all pointers?
          ($oops who "cannot create constructor descriptor for record type with non-scheme-object fields"))
        (let ([prtd (record-type-parent rtd)])
          (when (and prcd (not prtd))
            ($oops who
              "record constructor descriptor ~s specified for base record type ~s"
              prcd rtd))
          (when (and prcd prtd (not (eq? (rcd-rtd prcd) prtd)))
            ($oops who
              "record constructor descriptor ~s is not for parent of record type ~s"
              prcd rtd))
          (when (and (not protocol) prcd (rcd-protocol prcd))
            ($oops who "no protocol specified, but parent ~s has protocol" prcd))
          (make-rcd rtd prcd protocol)))

      (set! $make-record-constructor-descriptor
        (lambda (rtd prcd protocol who)
          (mrcd rtd prcd protocol who)))

      (set! make-record-constructor-descriptor
        (lambda (rtd prcd protocol)
          (mrcd rtd prcd protocol 'make-record-constructor-descriptor))))

    (let ()
      (define $rtd->record-constructor
        (lambda (rtd)
          (define type->pred
            (lambda (ty)
              (define-syntax ->pred
                (syntax-rules () ((_ type bytes pred) 'pred)))
              (record-datatype cases ty ->pred
                ($oops 'record-constructor "unrecognized type ~s" ty))))
          (let* ((flds (rtd-flds rtd)) (nflds (if (fixnum? flds) flds (length flds))))
            (if (eqv? (rtd-pm rtd) -1) ; all pointers?
                (let ()
                  (define-syntax nlambda
                    (lambda (x)
                      (syntax-case x ()
                        [(_ n)
                         (with-syntax (((t ...)
                                        (generate-temporaries
                                          (make-list
                                            (datum n)))))
                           #'(rec constructor
                               (lambda (t ...) ($record rtd t ...))))])))
                  (case nflds
                    [(0) (nlambda 0)]
                    [(1) (nlambda 1)]
                    [(2) (nlambda 2)]
                    [(3) (nlambda 3)]
                    [(4) (nlambda 4)]
                    [(5) (nlambda 5)]
                    [(6) (nlambda 6)]
                    [else (rec constructor
                            ($make-wrapper-procedure
                             (lambda xr
                               (unless (fx= (length xr) nflds)
                                 ($oops #f "incorrect number of arguments to ~s" constructor))
                               (apply $record rtd xr))
                             (ash 1 nflds)))]))
                ;; In this case, `flds` will be a list
                (let* ([args (make-record-call-args flds (rtd-size rtd)
                               (map (lambda (x) 0) flds))]
                       [nargs (length args)]
                       [setters (map (lambda (fld)
                                       (let ([byte (fld-byte fld)]
                                             [ty (fld-type fld)])
                                         (let ([msg (format "invalid value ~~s for foreign type ~s" ty)])
                                           (define-syntax init
                                             (syntax-rules (scheme-object)
                                               [(_ scheme-object bytes pred)
                                                (lambda (x v)
                                                  (#3%$object-set! 'scheme-object x byte v))]
                                               [(_ type bytes pred)
                                                (lambda (x v)
                                                  (unless (pred v) ($oops #f msg v))
                                                  (#3%$object-set! 'type x byte v))]))
                                           (record-datatype cases (filter-foreign-type ty) init
                                             ($oops 'record-constructor "unrecognized type ~s" ty)))))
                                  flds)])
                  (define-syntax nmlambda
                    (lambda (x)
                      (syntax-case x ()
                        [(_ n m)
                         (with-syntax ([(t ...) (generate-temporaries
                                                  (make-list (datum n)))]
                                       [(z ...) (make-list (datum m) 0)])
                           (with-syntax ([(t! ...) (generate-temporaries #'(t ...))])
                             #'(apply
                                 (lambda (t! ...)
                                   (rec constructor
                                     (lambda (t ...)
                                       (let ([x ($record rtd z ...)])
                                         (t! x t) ...
                                         x))))
                                 setters)))])))
                  (or (constant-case ptr-bits
                        [(64)
                         (case nflds
                           [(0) (and (= nargs 0) (nmlambda 0 0))]
                           [(1) (and (= nargs 1) (nmlambda 1 1))]
                           [(2) (case nargs
                                  [(1) (nmlambda 2 1)]
                                  [(2) (nmlambda 2 2)]
                                  [else #f])]
                           [(3) (case nargs
                                  [(1) (nmlambda 3 1)]
                                  [(2) (nmlambda 3 2)]
                                  [(3) (nmlambda 3 3)]
                                  [else #f])]
                           [(4) (case nargs
                                  [(1) (nmlambda 4 1)]
                                  [(2) (nmlambda 4 2)]
                                  [(3) (nmlambda 4 3)]
                                  [(4) (nmlambda 4 4)]
                                  [else #f])]
                           [else #f])]
                        [(32)
                         (case nflds
                           [(0) (nmlambda 0 0)]
                           [(1) (case nargs
                                  [(1) (nmlambda 1 1)]
                                  [(2) (nmlambda 1 2)]
                                  [(3) (nmlambda 1 3)]
                                  [else #f])]
                           [(2) (case nargs
                                  [(1) (nmlambda 2 1)]
                                  [(2) (nmlambda 2 2)]
                                  [(3) (nmlambda 2 3)]
                                  [(4) (nmlambda 2 4)]
                                  [(5) (nmlambda 2 5)]
                                  [else #f])]
                           [(3) (case nargs
                                  [(1) (nmlambda 3 1)]
                                  [(2) (nmlambda 3 2)]
                                  [(3) (nmlambda 3 3)]
                                  [(4) (nmlambda 3 4)]
                                  [(5) (nmlambda 3 5)]
                                  [(6) (nmlambda 3 6)]
                                  [(7) (nmlambda 3 7)]
                                  [else #f])]
                           [(4) (case nargs
                                  [(1) (nmlambda 4 1)]
                                  [(2) (nmlambda 4 2)]
                                  [(3) (nmlambda 4 3)]
                                  [(4) (nmlambda 4 4)]
                                  [(5) (nmlambda 4 5)]
                                  [(6) (nmlambda 4 6)]
                                  [(7) (nmlambda 4 7)]
                                  [(8) (nmlambda 4 8)]
                                  [(9) (nmlambda 4 9)]
                                  [else #f])]
                           [else #f])])
                     (rec constructor
                       ($make-wrapper-procedure
                        (lambda xr
                          (unless (fx= (length xr) nflds)
                            ($oops #f "incorrect number of arguments to ~s" constructor))
                          (let ([x (apply $record rtd args)])
                            (for-each (lambda (setter v) (setter x v)) setters xr)
                            x))
                        (ash 1 nflds)))))))))

      (define ($rcd->record-constructor rcd)
        (let ([rtd (rcd-rtd rcd)] [protocol (rcd-protocol rcd)])
          (let ([rc ($rtd->record-constructor rtd)])
            (if protocol
                (protocol
                  (cond
                    [(rtd-parent rtd) =>
                     (lambda (prtd)
                       (lambda pp-args
                         (lambda vals
                           (let f ([prcd (rcd-prcd rcd)] [prtd prtd] [pp-args pp-args] [vals vals])
                             (#2%apply
                               (cond
                                 [(and prcd (rcd-protocol prcd)) =>
                                  (lambda (protocol)
                                    (protocol
                                      (cond
                                        [(rtd-parent prtd) =>
                                         (lambda (prtd)
                                           (lambda pp-args
                                             (lambda new-vals
                                               (f (rcd-prcd prcd) prtd pp-args
                                                  (append new-vals vals)))))]
                                        [else
                                         (lambda new-vals
                                           (apply rc (append new-vals vals)))])))]
                                 [else
                                  (lambda new-vals
                                    (apply rc (append new-vals vals)))])
                               pp-args)))))]
                    [else rc]))
                rc))))

      (set! record-constructor
        (lambda (x)
          (cond
            [(record-type-descriptor? x) ($rtd->record-constructor x)]
            [(record-constructor-descriptor? x) ($rcd->record-constructor x)]
            [else ($oops 'record-constructor "~s is not a record type or constructor descriptor" x)])))

      (set-who! #(r6rs: record-constructor)
        (lambda (rcd)
          (unless (rcd? rcd)
            ($oops who "~s is not a record constructor descriptor" rcd))
          ($rcd->record-constructor rcd)))))
)
