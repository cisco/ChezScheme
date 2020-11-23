;;; hashtable-types.ss
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

(define-record-type (hashtable make-xht xht?)
  (fields (immutable type xht-type) (immutable mutable? xht-mutable?))
  (nongenerative #{hashtable bu811z2onf9o6tfc-0}))

(define-record-type ht
  (parent hashtable)
  (fields (mutable vec) (mutable minlen) (mutable size))
  (nongenerative #{ht bu811z2onf9o6tfc-6}))

(define-record-type eq-ht
  (parent ht)
  (fields (immutable subtype)) ; eq-hashtable-subtype-{normal,weak,ephemeron}
  (nongenerative #{eq-ht icguu8mlhm1y7ywsairxck-0})
  (sealed #t))

(define-record-type symbol-ht
  (parent ht)
  (fields (immutable equiv?))
  (nongenerative #{symbol-ht bu811z2onf9o6tfc-8})
  (sealed #t))

(define-record-type gen-ht
  (parent ht)
  (fields (immutable hash) (immutable equiv?) (immutable eqht))
  (nongenerative #{gen-ht bu811z2onf9o6tfc-9})
  (sealed #t))

(define-record-type eqv-ht
  (parent hashtable)
  (fields (immutable eqht) (immutable genht))
  (nongenerative #{eqv-ht bu811z2onf9o6tfc-4})
  (sealed #t))
