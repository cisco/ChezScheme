;;; expand-lang.ss
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

(define-record-type libreq
  (fields
    (immutable path)
    (immutable version)
    (immutable uid))
  (nongenerative #{libreq fnuxvkuvs8x0xbc68h3hm6-0})
  (sealed #t))

(define-record-type recompile-info
  (fields
    (immutable import-req*)
    (immutable include-req*))
  (nongenerative #{recompile-info fnuxvkuvs8x0xbc68h3hm6-1})
  (sealed #t))

(define-record-type library-info
  (nongenerative #{library-info e10vy7tci6bqz6pmnxgvlq-2})
  (fields
    (immutable path)
    (immutable version)
    (immutable uid)))

(define-record-type library/ct-info
  (parent library-info)
  (fields
    ; NB: include-req* should go away with new recompile support that uses recompile-info
    (immutable include-req*)
    (immutable import-req*)
    (immutable visit-visit-req*)
    (immutable visit-req*)
    (immutable clo*))
  (nongenerative #{library/ct-info fgf0koeh2zn6ajlujfyoyf-3})
  (sealed #t))

(define-record-type library/rt-info
  (parent library-info)
  (fields
    (immutable invoke-req*))
  (nongenerative #{library/rt-info ff86rtm7efmvxcvrmh7t0b-2})
  (sealed #t))

(define-record-type program-info
  (fields (immutable uid) (immutable invoke-req*))
  (nongenerative #{program-info fgc8ptwnu9i5gfqz3s85mr-0})
  (sealed #t))

(define (revisit-stuff? x) (and (pair? x) (eqv? (car x) (constant revisit-tag))))
(define (revisit-stuff-inner x) (cdr x))

(define (visit-stuff? x) (and (pair? x) (eqv? (car x) (constant visit-tag))))
(define (visit-stuff-inner x) (cdr x))

(module (Lexpand Lexpand?)
  (define library-path?
    (lambda (x)
      (and (list? x) (andmap symbol? x))))

  (define library-version?
    (lambda (x)
      (and (list? x)
           (andmap (lambda (x) (and (integer? x) (exact? x) (>= x 0))) x))))

  (define maybe-optimization-loc? (lambda (x) (or (not x) (box? x)))) ; should be a record
  
  (define maybe-label? (lambda (x) (or (not x) (gensym? x))))

  (define-language Lexpand
    (nongenerative-id #{Lexpand fgy7v2wrvj0so4ro8kvhqo-1})
    (terminals
      (maybe-label (dl))
      (gensym (uid))
      (library-path (path))
      (library-version (version))
      (maybe-optimization-loc (db))
      (prelex (dv))
      (libreq (import-req visit-req visit-visit-req invoke-req))
      (string (include-req))
      (Lsrc (lsrc body init visit-code import-code de)) => unparse-Lsrc
      (recompile-info (rcinfo))
      (library/ct-info (linfo/ct))
      (library/rt-info (linfo/rt))
      (program-info (pinfo)))
    (Outer (outer)
      rcinfo
      (group outer1 outer2)
      (visit-only inner)
      (revisit-only inner)
      inner)
    (Inner (inner)
      linfo/ct
      ctlib
      linfo/rt
      rtlib
      pinfo
      prog
      lsrc)
    (ctLibrary (ctlib)
      (library/ct uid import-code visit-code))
    (rtLibrary (rtlib)
      (library/rt uid
        (dl* ...)
        (db* ...)
        (dv* ...)
        (de* ...)
        body))
    (Program (prog)
      (program uid body))))
