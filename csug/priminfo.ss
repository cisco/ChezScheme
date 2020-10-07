;;; priminfo.ss
;;; Copyright 2005-2017 Cisco Systems, Inc.
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

(module priminfo (primvec get-libraries)

  (define prim-db (make-eq-hashtable))

  (define primvec
    (lambda ()
      (hashtable-keys prim-db)))

  (define get-libraries
    (lambda (name)
      (or (eq-hashtable-ref prim-db name #f)
          (errorf #f "unknown primitive ~s" name))))

  (define put-priminfo!
    (lambda (prim lib*)
      (when (eq-hashtable-contains? prim-db prim)
        (warning 'define-symbol-type "extra entry for ~s" prim))
      (eq-hashtable-set! prim-db prim lib*)))

  (define-syntax define-symbol-flags*
    (lambda (x)
      (syntax-case x (libraries)
        [(k ([libraries lib ...] [flags flag ...] ignore ...) entry ...)
         (or (memq 'system (datum (flag ...)))
             (memq 'system-keyword (datum (flag ...))))
         #'(void)]
        [(k ([libraries] ignore ...) entry ...)
         #'(k ([libraries (chezscheme)] ignore ...) entry ...)]
        [(_ ([libraries lib ...] ignore ...) entry ...)
         (if (syntax-case #'(lib ...) (rnrs)
                   [((rnrs x ...) y ...) #t]
                   [_ #f])
             #'(void)
             (let ()
               (define do-entry
                 (lambda (x)
                   (syntax-case x ()
                     [((prefix prim) ignore ...)
                      (and (identifier? #'prefix) (identifier? #'prim))
                      #'(put-priminfo! 'prim '(lib ...))]
                     [(prim ignore ...)
                      (identifier? #'prim)
                      #'(put-priminfo! 'prim '(lib ...))])))
               #`(begin #,@(map do-entry #'(entry ...)))))])))

  (include "primdata.ss")
)
