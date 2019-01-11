;;; fxmap.ss
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

;; Based on Okasaki and Gill's "Fast Mergeable Integer Maps" (1998).

(module fxmap
 (fxmap?
  empty-fxmap
  fxmap-empty?
  fxmap-count
  fxmap-ref
  fxmap-set
  fxmap-remove
  fxmap-remove/base
  fxmap-reset/base
  fxmap-advance/base
  fxmap-for-each
  fxmap-for-each/diff
  fxmap-changes

  ;; internals
  ; $branch? make-$branch $branch-prefix $branch-mask $branch-left $branch-right
  ; $leaf? make-$leaf $leaf-key $leaf-val

  ;; We treat $empty as a singleton, so don't use these functions.
  ; $empty? make-$empty
  )

 ;; record types

 (define-record-type $branch
   (fields prefix mask left right count changes)
   (nongenerative #{$branch pfv8jpsat5jrk6vq7vclc3ntg-1})
   (sealed #t))

 (define-record-type $leaf
   (fields key val changes)
   (nongenerative #{$leaf pfv8jq2dzw50ox4f6vqm1ff5v-1})
   (sealed #t))

 (define-record-type $empty
   (nongenerative #{$empty pfwk1nal7cs5dornqtzvda91m-0})
   (sealed #t))

 (define-syntax let-branch
   (syntax-rules ()
     [(_ ([(p m l r) d] ...) exp ...)
      (let ([p ($branch-prefix d)] ...
            [m ($branch-mask d)] ...
            [l ($branch-left d)] ...
            [r ($branch-right d)] ...)
        exp ...)]))

 ;; constants & empty

 (define empty-fxmap (make-$empty))

 (define (fxmap-empty? x) (eq? empty-fxmap x))

 ;; predicate

 (define (fxmap? x)
   (or ($branch? x)
       ($leaf? x)
       (eq? empty-fxmap x)))

 ;; count & changes

 (define (fxmap-count d)
   (cond
     [($branch? d)
      ($branch-count d)]
     [($leaf? d) 1]
     [else 0]))

 (define (fxmap-changes d)
   (cond
     [($branch? d)
      ($branch-changes d)]
     [($leaf? d)
      ($leaf-changes d)]
     [else 0]))

 ;; ref

 (define (fxmap-ref/leaf d key)
   (cond
     [($branch? d)
      (let-branch ([(p m l r) d])
        (cond
          [(fx<= key p)
           (fxmap-ref/leaf l key)]
          [else
           (fxmap-ref/leaf r key)]))]

     [($leaf? d)
      (if (fx= key ($leaf-key d))
          d
          #f)]

     [else
      #f]))

 (define (fxmap-ref d key default)
   (let ([d (fxmap-ref/leaf d key)])
     (if d
         ($leaf-val d)
         default)))

 (define (fxmap-ref/changes d key)
   (let ([d (fxmap-ref/leaf d key)])
     (if d
         ($leaf-changes d)
         0)))

 ;; set

 (define (fxmap-set/changes d key val changes)
   (cond
    [($branch? d)
     (let-branch ([(p m l r) d])
       (cond
        [(nomatch? key p m)
         (join key (make-$leaf key val (or changes 1)) p d)]
        [(fx<= key p)
         (br p m (fxmap-set/changes l key val changes) r)]
        [else
         (br p m l (fxmap-set/changes r key val changes))]))]

    [($leaf? d)
     (let ([k ($leaf-key d)])
       (if (fx= key k)
           (make-$leaf key val (or changes (fx+ ($leaf-changes d) 1)))
           (join key (make-$leaf key val (or changes 1)) k d)))]

    [else
     (make-$leaf key val (or changes 1))]))

 (define (fxmap-set d key val)
   (fxmap-set/changes d key val #f))

 ;; remove

 (define (fxmap-remove d key)
   (cond
    [($branch? d)
     (let-branch ([(p m l r) d])
       (cond
        [(nomatch? key p m) d]
        [(fx<= key p)       (br* p m (fxmap-remove l key) r)]
        [else               (br* p m l (fxmap-remove r key))]))]

    [($leaf? d)
     (if (fx= key ($leaf-key d))
         empty-fxmap
         d)]

    [else
     empty-fxmap]))

 (define (fxmap-remove/base d key base)
   ; Remove key from d, but try to reuse the branches from base when possible
   ; instead of creating new ones.
   ; TODO: This assumes that all the keys in base are in d too.
   ; Perhaps this restriction can be removed.
   (cond
     [($branch? base)
      (cond
        [($branch? d)
         (let-branch ([(p0 m0 l0 r0) base]
                      [(p m l r) d])
           (let ([sub-base (cond
                             [(fx< m0 m) base]
                             [(fx<= key p0) l0]
                             [else r0])])
             (cond
               [(nomatch? key p m)
                d]
               [(fx<= key p)
                (br*/base p m (fxmap-remove/base l key sub-base) r base)]
               [else
                (br*/base p m l (fxmap-remove/base r key sub-base) base)])))]

        [($leaf? d)
         (if (fx= key ($leaf-key d))
             empty-fxmap
             d)]

        [else
         empty-fxmap])]
    [else
     (fxmap-remove d key)]))

 ;; reset and advance

 (define (fxmap-reset/base d key base)
   ; Reset key in d to the value it has in base, but try to reuse the branches
   ; from base when possible instead of creating new ones.
   ; TODO: This assumes that all the keys in base are in d too.
   ; Perhaps this restriction can be removed.
   (cond
     [($branch? d)
      (let-branch ([(p m l r) d])
        (let ([sub-base (cond
                          [($branch? base)
                           (let-branch ([(p0 m0 l0 r0) base])
                             (cond
                               [(fx< m0 m) base]
                               [(fx<= key p0) l0]
                               [else r0]))]
                          [else base])])
           (cond
             [(nomatch? key p m)
              d]
             [(fx<= key p)
              (br*/base p m (fxmap-reset/base l key sub-base) r base)]
             [else
              (br*/base p m l (fxmap-reset/base r key sub-base) base)])))]

      [(and ($leaf? d)
            (fx= key ($leaf-key d))
            ($leaf? base)
            (fx= key ($leaf-key base)))
       base]

      [else
       (error 'fxmap-reset/base "")]))

 (define (fxmap-advance/base d key base)
   (let ([changes (fx+ (fxmap-ref/changes base key) 1)]
         [l (fxmap-ref/leaf d key)])
     (if l
         (if (fx= changes ($leaf-changes l))
             d
            (fxmap-set/changes d key ($leaf-val l) changes))
         (error 'fxmap-advance/base ""))))

 ;; set and remove utilities

 (define-syntax define-syntax-rule
  (syntax-rules ()
    [(_ (name arg ...) e ...)
     (define-syntax name
       (syntax-rules ()
         [(_ arg ...) e ...]))]))

 (define (br p m l r)
   (make-$branch p m l r
                 (fx+ (fxmap-count l) (fxmap-count r))
                 (fx+ (fxmap-changes l) (fxmap-changes r))))

 (define (br* p m l r)
   (cond [(eq? empty-fxmap r) l]
         [(eq? empty-fxmap l) r]
         [else (br p m l r)]))

 (define (br*/base p m l r base)
   (cond [(eq? empty-fxmap r) l]
         [(eq? empty-fxmap l) r]
         [(and ($branch? base)
               (eq? l ($branch-left base))
               (eq? r ($branch-right base)))
          base]
         [else (br p m l r)]))

 (define (join p0 d0 p1 d1)
   (let ([m (branching-bit p0 p1)])
     (if (fx<= p0 p1)
         (br (mask p0 m) m d0 d1)
         (br (mask p0 m) m d1 d0))))

 (define (join* p1 d1 p2 d2)
   (cond
     [(eq? empty-fxmap d1) d2]
     [(eq? empty-fxmap d2) d1]
     [else (join p1 d1 p2 d2)]))

 (define (branching-bit p m)
   (highest-set-bit (fxxor p m)))

 (define-syntax-rule (mask h m)
   (fxand (fxior h (fx1- m)) (fxnot m)))

 (define highest-set-bit
   (if (fx= (fixnum-width) 61)
       (lambda (x1)
         (let* ([x2 (fxior x1 (fxsrl x1 1))]
                [x3 (fxior x2 (fxsrl x2 2))]
                [x4 (fxior x3 (fxsrl x3 4))]
                [x5 (fxior x4 (fxsrl x4 8))]
                [x6 (fxior x5 (fxsrl x5 16))]
                [x7 (fxior x6 (fxsrl x6 32))])
           (fxxor x7 (fxsrl x7 1))))
       (lambda (x1)
         (let* ([x2 (fxior x1 (fxsrl x1 1))]
                [x3 (fxior x2 (fxsrl x2 2))]
                [x4 (fxior x3 (fxsrl x3 4))]
                [x5 (fxior x4 (fxsrl x4 8))]
                [x6 (fxior x5 (fxsrl x5 16))])
           (fxxor x6 (fxsrl x6 1))))))


 (define-syntax-rule (nomatch? h p m)
   (not (fx= (mask h m) p)))

 ;; merge

 (define (fxmap-merge bin f id g1 g2 d1 d2)
   (define-syntax go
     (syntax-rules ()
       [(_ d1 d2) (fxmap-merge bin f id g1 g2 d1 d2)]))

   (cond
    [(eq? d1 d2) (id d1)]

    [($branch? d1)
     (cond
      [($branch? d2)
       (let-branch ([(p1 m1 l1 r1) d1]
                    [(p2 m2 l2 r2) d2])
        (cond
         [(fx> m1 m2) (cond
                       [(nomatch? p2 p1 m1) (join* p1 (g1 d1) p2 (g2 d2))]
                       [(fx<= p2 p1)        (bin p1 m1 (go l1 d2) (g1 r1))]
                       [else                (bin p1 m1 (g1 l1) (go r1 d2))])]
         [(fx> m2 m1) (cond
                       [(nomatch? p1 p2 m2) (join* p1 (g1 d1) p2 (g2 d2))]
                       [(fx<= p1 p2)        (bin p2 m2 (go d1 l2) (g2 r2))]
                       [else                (bin p2 m2 (g2 l2) (go d1 r2))])]
         [(fx= p1 p2) (bin p1 m1 (go l1 l2) (go r1 r2))]
         [else        (join* p1 (g1 d1) p2 (g2 d2))]))]

      [($leaf? d2)
       (let ([k2 ($leaf-key d2)])
         (let merge0 ([d1 d1])
           (cond
            [(eq? d1 d2)
             (id d1)]

            [($branch? d1)
             (let-branch ([(p1 m1 l1 r1) d1])
              (cond [(nomatch? k2 p1 m1) (join* p1 (g1 d1) k2 (g2 d2))]
                    [(fx<= k2 p1)        (bin p1 m1 (merge0 l1) (g1 r1))]
                    [else                (bin p1 m1 (g1 l1) (merge0 r1))]))]

            [($leaf? d1)
             (let ([k1 ($leaf-key d1)])
               (cond [(fx= k1 k2) (f d1 d2)]
                     [else        (join* k1 (g1 d1) k2 (g2 d2))]))]

            [else ; (eq? empty-fxmap d1)
             (g2 d2)])))]

      [else ; (eq? empty-fxmap d2)
       (g1 d1)])]

    [($leaf? d1)
     (let ([k1 ($leaf-key d1)])
       (let merge0 ([d2 d2])
         (cond
          [(eq? d1 d2)
           (id d1)]

          [($branch? d2)
           (let-branch ([(p2 m2 l2 r2) d2])
            (cond [(nomatch? k1 p2 m2) (join* k1 (g1 d1) p2 (g2 d2))]
                  [(fx<= k1 p2)        (bin p2 m2 (merge0 l2) (g2 r2))]
                  [else                (bin p2 m2 (g2 l2) (merge0 r2))]))]

          [($leaf? d2)
           (let ([k2 ($leaf-key d2)])
             (cond [(fx= k1 k2) (f d1 d2)]
                   [else        (join* k1 (g1 d1) k2 (g2 d2))]))]

          [else ; (eq? empty-fxmap d2)
           (g1 d1)])))]

    [else ; (eq? empty-fxmap d1)
     (g2 d2)]))

 ;; merge*
 ; like merge, but the result is (void)

 (define (fxmap-merge* f id g1 g2 d1 d2)
   (define (merge* f id g1 g2 d1 d2)
     (define-syntax go
       (syntax-rules ()
         [(_ d1 d2) (merge* f id g1 g2 d1 d2)]))

     (cond
      [(eq? d1 d2) (id d1)]

      [($branch? d1)
       (cond
        [($branch? d2)
         (let-branch ([(p1 m1 l1 r1) d1]
                      [(p2 m2 l2 r2) d2])
          (cond
           [(fx> m1 m2) (cond
                         [(nomatch? p2 p1 m1) (g1 d1) (g2 d2)]
                         [(fx<= p2 p1)        (go l1 d2) (g1 r1)]
                         [else                (g1 l1) (go r1 d2)])]
           [(fx> m2 m1) (cond
                         [(nomatch? p1 p2 m2) (g1 d1) (g2 d2)]
                         [(fx<= p1 p2)        (go d1 l2) (g2 r2)]
                         [else                (g2 l2) (go d1 r2)])]
           [(fx= p1 p2) (go l1 l2) (go r1 r2)]
           [else        (g1 d1) (g2 d2)]))]

        [else ; ($leaf? d2)
         (let ([k2 ($leaf-key d2)])
           (let merge*0 ([d1 d1])
             (cond
              [(eq? d1 d2)
               (id d1)]

              [($branch? d1)
               (let-branch ([(p1 m1 l1 r1) d1])
                (cond [(nomatch? k2 p1 m1) (g1 d1) (g2 d2)]
                      [(fx<= k2 p1)        (merge*0 l1) (g1 r1)]
                      [else                (g1 l1) (merge*0 r1)]))]

              [else ; ($leaf? d1)
               (let ([k1 ($leaf-key d1)])
                 (cond [(fx= k1 k2) (f d1 d2)]
                       [else        (g1 d1) (g2 d2)]))])))])]

      [($leaf? d1)
       (let ([k1 ($leaf-key d1)])
         (let merge*0 ([d2 d2])
           (cond
            [(eq? d1 d2)
             (id d1)]

            [($branch? d2)
             (let-branch ([(p2 m2 l2 r2) d2])
              (cond [(nomatch? k1 p2 m2) (g1 d1) (g2 d2)]
                    [(fx<= k1 p2)        (merge*0 l2) (g2 r2)]
                    [else                (g2 l2) (merge*0 r2)]))]

            [else ; ($leaf? d2)
             (let ([k2 ($leaf-key d2)])
               (cond [(fx= k1 k2) (f d1 d2)]
                     [else        (g1 d1) (g2 d2)]))])))]))

   (cond
    [(eq? d1 d2)
     (id d1)]
    [(eq? empty-fxmap d1)
     (g2 d2)]
    [(eq? empty-fxmap d2)
     (g1 d1)]
    [else
     (merge* f id g1 g2 d1 d2)])
   (void))

 ;; for-each

 (define (fxmap-for-each g1 d1)
   (cond
     [($branch? d1)
      (fxmap-for-each g1 ($branch-left d1))
      (fxmap-for-each g1 ($branch-right d1))]
     [($leaf? d1)
      (g1 ($leaf-key d1) ($leaf-val d1))]
     [else ; (eq? empty-fxmap d1)
      (void)])
   (void))

 (define (fxmap-for-each/diff f g1 g2 d1 d2)
   (fxmap-merge* (lambda (x y) (f ($leaf-key x) ($leaf-val x) ($leaf-val y)))
                 (lambda (x) (void))
                 (lambda (x) (fxmap-for-each g1 x))
                 (lambda (x) (fxmap-for-each g2 x))
                 d1
                 d2)
   (void))
)
