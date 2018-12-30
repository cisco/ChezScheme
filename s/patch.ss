;;; patch.ss
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

(define ($value x) x)

(printf "loading ~s cross compiler~%" (constant machine-type-name))

; (current-expand (lambda args (apply sc-expand args)))
; (current-eval (lambda args (apply interpret args)))

(when-feature pthreads
  (meta-cond
    [(not (threaded?))
     ; we must be cross-compiling from nonthreaded to threaded version
     ; handle thread parameter creation
     (define-syntax with-mutex
       (syntax-rules ()
         [(_ mexp e0 e1 ...) (begin e0 e1 ...)]))
     (set! make-thread-parameter make-parameter)
     (set! mutex-acquire (lambda (m) (void)))
     (set! mutex-release (lambda (m) (void)))
     (set! $tc-mutex (void))]))

