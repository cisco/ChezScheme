"primvars.ss"
;;; primvars.ss
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
  (include "primref.ss")

  (define record-prim!
    (lambda (prim unprefixed flags arity boolean-valued? signatures)
      (unless (eq? unprefixed prim) ($sputprop prim '*unprefixed* unprefixed))
      (let ([flags (if boolean-valued? (fxlogor flags (prim-mask boolean-valued)) flags)]
            [arity (and (not (null? arity)) arity)])
        ($sputprop prim '*flags* flags)
        (when (any-set? (prim-mask (or primitive system)) flags)
          ($sputprop prim '*prim2* (make-primref prim flags arity signatures))
          ($sputprop prim '*prim3* (make-primref prim (fxlogor flags (prim-mask unsafe)) arity signatures))))))

  (define-syntax setup
    (lambda (x)
      (import priminfo)
      ; sort vector of primitive names so boot files compare equal
      (let ([v-prim (vector-sort (lambda (x y) (string<=? (symbol->string x) (symbol->string y))) (primvec))])
        (let ([v-info (vector-map get-priminfo v-prim)])
          #`(vector-for-each record-prim!
              '#,(datum->syntax #'* v-prim)
              '#,(datum->syntax #'* (vector-map priminfo-unprefixed v-info))
              '#,(datum->syntax #'* (vector-map priminfo-mask v-info))
              '#,(datum->syntax #'* (vector-map priminfo-arity v-info))
              '#,(datum->syntax #'* (vector-map priminfo-boolean? v-info))
              '#,(datum->syntax #'* (vector-map priminfo-signatures v-info)))))))

  (for-each (lambda (x) (for-each (lambda (key) ($sremprop x key)) '(*prim2* *prim3* *flags* *unprefixed*))) (oblist))
  setup)
