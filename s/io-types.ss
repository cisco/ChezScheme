;;; io-types.ss
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

#|
In order to be thread safe, size must be zero and the handler procedures
must obtain the tc-mutex or use some other mechanism to guarantee mutual
exclusion while manipulating the buffer.

The built-in handlers for binary file output ports are thread-safe iff
the buffer mode is none.  The handlers for input ports are not thread-safe,
since the buffer size may be non-zero to handle lookahead and ungetting.

In order to be safe for continuation-based multitasking, the buffer must
be manipulated only by inline code (which runs between interrupt traps) or
within a critical section.  The built-in file handlers are task-safe, but
the handlers for custom ports and for bytevector ports are not.

In general caller will check immutable properties of inputs but the
handler must check mutable properties of inputs because other threads may
change those properties.  For example, handlers need not check the types
of most input values (e.g., ports, octets, bytevectors) but do have to
check for closed ports.  (Position and length arguments are an exception,
since they may vary by kind of port.) Furthermore, handlers, including put
and get, should not expect the buffer to be full or empty when they are
called, since in general this cannot be guaranteed if multiple tasks or
threads are running.  On the other hand, handlers generally won't be
called for every operation on a port, since data is usually inserted into
or taken from the buffer when appropriate.

To indicate an input buffer containing an #!eof object, handlers should
set the input size empty and set the port-eof-flag.

Handler fields for unsupported operations should be set to #f.  The others
must be procedures.  All port handlers must supply a procedure for
close-port.  Input port handlers must supply procedures for ready?,
lookahead, unget, get, and get-some.  Output port handlers must supply
procedures for put, put-some, and flush.

For port-position, set-port-position!, port-nonblocking?,
set-port-nonblocking!, port-length, and set-port-length!, the
corresponding "port-has" predicate will return true iff a procedure is
supplied.  These procedures must take into account input and output
buffers as appropriate.  Positions must be byte counts for binary ports
(see R6RS).  For output ports handler must flush the port on "set" (see
R6RS), and for input port handler must clear the buffer on "set" if
needed.

The get-some and put-some procedures should not block on nonblocking
ports, but should instead return 0 to indicate that no data was written or
read.  Exception: if a textual output port is line-buffered and the
string passed to put-some contains an eol character, put-some must
flush at least to the last eol character.

The close-port procedure must flush the output buffer as appropriate, set
the buffer size(s) to zero, clear the port-eof flag, and mark the port
closed.
|#

(define-syntax define-port-handler
  (lambda (x)
    (syntax-case x (->)
      [(_ (?record-name ?constructor-name ?pred-name) uid
            (?field ?param ... -> ?result) ...)
       (or (not (datum uid)) (identifier? #'uid))
       #`(begin
           (define-record-type (?record-name mph ?pred-name)
             #,(if (datum uid) #'(nongenerative uid) #'(nongenerative))
             (opaque #t)
             (sealed #t)
             (fields (immutable ?field) ...))
           (define-syntax ?constructor-name
             (lambda (x)
               (syntax-case x ()
                 [(_ [?name ?expr] (... ...))
                  (begin
                    (let loop ([field* '(?field ...)] [name* #'(?name (... ...))])
                      (if (null? field*)
                          (unless (null? name*)
                            (syntax-error (car name*) "unexpected"))
                          (if (null? name*)
                              (syntax-error x (format "missing ~s" (car field*)))
                              (if (eq? (syntax->datum (car name*)) (car field*))
                                  (loop (cdr field*) (cdr name*))
                                  (syntax-error (car name*) "unexpected")))))
                    (for-each
                      (lambda (name p expr)
                        (unless (p expr)
                          (syntax-error expr (format "invalid ~s ~s rhs syntax" (datum ?constructor-name) (syntax->datum name)))))
                      #'(?name (... ...))
                      (list
                        (lambda (expr)
                          (syntax-case expr (lambda)
                            [(lambda (?param ...) . body) #t]
                            [(lambda . rest) #f]
                            [_ #t]))
                        ...)
                      #'(?expr (... ...)))
                    #'(mph ?expr (... ...)))]))))])))

;; The following input types are guaranteed upon reaching a handler:
;;   who: symbol
;;   bool: any object
;;   p: input, output, or input/output port as appropriate
;;   elt (binary port): exact nonnegative integer <= 255
;;   elt (textual port): character
;;   elt/eof: elt or #!eof
;;   bv: bytevector
;;   start, count: exact nonnegative integer
;;
;; Also: start + count <= length(bv).
;;
;; The types of pos and len are port-specific and must be checked by
;; the handler

;; Handlers are responsible for returning appropriate values:
;;   bool: any object
;;   elt (binary port): exact nonnegative integer <= 255
;;   elt (textual port): character
;;   elt/eof: elt or eof
;;   count: exact nonnegative integer
;;   count/eof: count or eof
;;   pos (binary port): exact nonnegative integer
;;   pos (textual port): any object
;;   len (binary port): exact nonnegative integer
;;   len (textual port): any object
;;
;; Also: output count must be less than or equal to input count.

; exporting all but port-handler, since it conflicts with the
; primtiive named port-handler
(module (make-port-handler port-handler? port-handler-ready?
         port-handler-lookahead port-handler-unget
         port-handler-get port-handler-get-some
         port-handler-clear-input port-handler-put
         port-handler-put-some port-handler-flush
         port-handler-clear-output port-handler-close-port
         port-handler-port-position
         port-handler-set-port-position!
         port-handler-port-length
         port-handler-set-port-length!
         port-handler-port-nonblocking?
         port-handler-set-port-nonblocking!)
  (define-port-handler (port-handler make-port-handler port-handler?) #{port-handler cx3umjhy9nkkuqku-a}
   ; input:
    (ready? who p -> bool)
    (lookahead who p -> elt/eof)
    (unget who p elt/eof -> void)
    (get who p -> elt/eof)
    (get-some who p bv start count -> count/eof)
    (clear-input who p -> void)
  
   ; output:
    (put who p elt -> void)
    (put-some who p bv start count -> count)
    (flush who p -> void)
    (clear-output who p -> void)
  
   ; all:
    (close-port who p -> void)
  
   ; optional:
    (port-position who p -> pos)
    (set-port-position! who p pos -> void)
    (port-length who p -> len)
    (set-port-length! who p len -> void)
    (port-nonblocking? who p -> bool)
    (set-port-nonblocking! who p bool -> void)))

;;; max-*-copy is the maximum amount a bytevector put operation will copy
;;; from the supplied bytevector to the port's buffer.  beyond this amount
;;; it will get/send contents directly from/to the underlying source/sink.
(define max-put-copy 256)
(define max-get-copy 256)
