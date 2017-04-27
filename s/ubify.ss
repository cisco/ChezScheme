;;; ubify.ss
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

(for-each
  (let ()
    (define-record-type ub
      (fields (immutable x))
      (nongenerative)
      (sealed #t))
    (record-writer (type-descriptor ub)
      (lambda (x p wr)
        (display "#<unbound " p)
        (wr (ub-x x) p)
        (display ">" p)))
    (lambda (x)
      (unless ($top-level-bound? x)
        ($set-top-level-value! x (make-ub x)))))
  (oblist))

(set-who! compile (lambda args ($oops who "disabled when cross compiling")))
(set-who! $compile-backend
  (lambda (x)
    (lambda ()
      (lambda (fp-entry)
        (lambda fp-args
          (pretty-print ($uncprep x))
          ($oops who "attempt to call foreign-procedure created while cross compiling"))))))
