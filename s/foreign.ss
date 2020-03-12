;;; foreign.ss
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
   (define $foreign-address-name
      (foreign-procedure "(cs)foreign_address_name" (void*)
         string))

   (define $remove-foreign-entry
     (foreign-procedure "(cs)remove_foreign_entry"
       (string) scheme-object))

   (set! $foreign-entries
      (foreign-procedure "(cs)foreign_entries" ()
         scheme-object))

   (set! remove-foreign-entry
      (lambda (entry)
         (unless (string? entry)
            ($oops 'remove-foreign-entry "~s is not a string" entry))
         (unless ($remove-foreign-entry entry)
            ($oops 'remove-foreign-entry "no entry for ~s" entry))))

   (let ()
     (define lookup
       (foreign-procedure "(cs)lookup_foreign_entry" (string)
         void*))
     (set-who! foreign-entry?
       (lambda (str)
         (unless (string? str) ($oops who "~s is not a string" str))
         (if (eqv? (lookup str) 0) #f #t)))
     (set-who! foreign-entry
       (lambda (str)
         (unless (string? str) ($oops who "~s is not a string" str))
         (let ([x (lookup str)])
           (when (eqv? x 0) ($oops who "no entry for ~s" str))
           x))))

   (set-who! foreign-address-name
     (lambda (n)
       (define void*?
         (constant-case ptr-bits
           [(32) $integer-32?]
           [(64) $integer-64?]))
       (unless (void*? n) ($oops who "~s is not a valid address" n))
       ($foreign-address-name n)))

   (set! load-shared-object
      (if (foreign-entry? "(cs)load_shared_object")
          (let ()
            (define lso
              (foreign-procedure "(cs)load_shared_object"
                (string)
                void))
            (lambda (x)
              (unless (or (string? x) (eq? x #f))
                ($oops 'load-shared-object "invalid path ~s" x))
              (lso x)))
          (lambda args
             ($oops 'load-shared-object "not supported"))))
) ;let
