;; Based on Okasaki and Gill's "Fast Mergeable Integer Maps" (1998).

(module fxmap
 (fxmap?
  empty-fxmap
  fxmap-empty?
  fxmap-count
  fxmap-ref
  fxmap-set
  fxmap-remove
  fxmap-merge

  ;; internals
  $branch? make-$branch $branch-prefix $branch-mask $branch-left $branch-right
  $leaf? make-$leaf $leaf-key $leaf-val
  $empty?)

 ;; record types

 (define-record-type $branch
   [fields prefix mask left right]
   [nongenerative #{$branch pfv8jpsat5jrk6vq7vclc3ntg-0}]
   [sealed #t])

 (define-record-type $leaf
   [fields key val]
   [nongenerative #{$leaf pfv8jq2dzw50ox4f6vqm1ff5v-0}]
   [sealed #t])

 (define-record-type $empty
   [nongenerative #{$empty pfwk1nal7cs5dornqtzvda91m-0}]
   [sealed #t])

 ;; constants

 (define empty-fxmap (make-$empty))

 ;; predicate

 (define (fxmap? x)
   (or ($branch? x)
       ($leaf? x)
       ($empty? x)))

 ;; count & empty

 (define (fxmap-count d)
   (let loop ([d d] [n 0])
     (cond [($branch? d)
            (let ([nl (loop ($branch-left d) n)])
              (loop ($branch-right d) nl))]
           [($leaf? d) (fx1+ n)]
           [else n])))

 (define fxmap-empty? $empty?)

 ;; ref

 (define (fxmap-ref d key default)
   (cond [($branch? d)
          (if (fx<= key ($branch-prefix d))
              (fxmap-ref ($branch-left d) key default)
              (fxmap-ref ($branch-right d) key default))]

         [($leaf? d)
          (if (fx= key ($leaf-key d))
              ($leaf-val d)
              default)]

         [else
          default]))

 ;; set

 (define (fxmap-set d key val)
   (cond
    [($branch? d)
     (let ([p ($branch-prefix d)]
           [m ($branch-mask d)])
       (cond
        [(nomatch? key p m)
         (join key (make-$leaf key val) p d)]
        [(fx<= key p)
         (br p m (fxmap-set ($branch-left d) key val) ($branch-right d))]
        [else
         (br p m ($branch-left d) (fxmap-set ($branch-right d) key val))]))]

    [($leaf? d)
     (let ([k ($leaf-key d)])
       (if (fx= key k)
           (make-$leaf key val)
           (join key (make-$leaf key val) k d)))]

    [else
     (make-$leaf key val)]))

 ;; remove

 (define (fxmap-remove d key)
   (cond
    [($branch? d)
     (let ([p ($branch-prefix d)]
           [m ($branch-mask d)])
       (cond
        [(nomatch? key p m) d]
        [(fx<= key p)       (br* p m (fxmap-remove ($branch-left d) key) ($branch-right d))]
        [else               (br* p m ($branch-left d) (fxmap-remove ($branch-right d) key))]))]

    [($leaf? d)
     (if (fx= key ($leaf-key d))
         empty-fxmap
         d)]

    [else
     empty-fxmap]))

 ;; set and remove utilities

 (define-syntax define-syntax-rule
  (syntax-rules ()
    [(_ (name arg ...) e ...)
     (define-syntax name
       (syntax-rules ()
         [(_ arg ...) e ...]))]))

 (define br make-$branch)

 (define (br* p m l r)
   (cond [($empty? r) l]
         [($empty? l) r]
         [else (br p m l r)]))

 (define (join p0 d0 p1 d1)
   (let ([m (branching-bit p0 p1)])
     (if (fx<= p0 p1)
         (br (mask p0 m) m d0 d1)
         (br (mask p0 m) m d1 d0))))

 (define (join* p1 d1 p2 d2)
   (cond
    [($empty? d1) d2]
    [($empty? d2) d1]
    [else (join p1 d1 p2 d2)]))

 (define (branching-bit p m)
   (highest-set-bit (fxxor p m)))

 (define-syntax-rule (mask h m)
   (fxand (fxior h (fx1- m)) (fxnot m)))

 (define (highest-set-bit x1)
   (let* ([x2 (fxior x1 (fxsrl x1 1))]
          [x3 (fxior x2 (fxsrl x2 2))]
          [x4 (fxior x3 (fxsrl x3 4))]
          [x5 (fxior x4 (fxsrl x4 8))]
          [x6 (fxior x5 (fxsrl x5 16))]
          [x7 (fxior x6 (fxsrl x6 32))])
     (fxxor x7 (fxsrl x7 1))))

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
       (let-branch
        ([(p1 m1 l1 r1) d1] [(p2 m2 l2 r2) d2])
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
             (let-branch
              ([(p1 m1 l1 r1) d1])
              (cond [(nomatch? k2 p1 m1) (join* p1 (g1 d1) k2 (g2 d2))]
                    [(fx<= k2 p1)        (bin p1 m1 (merge0 l1) (g1 r1))]
                    [else                (bin p1 m1 (g1 l1) (merge0 r1))]))]

            [($leaf? d1)
             (let ([k1 ($leaf-key d1)])
               (cond [(fx= k1 k2) (f d1 d2)]
                     [else        (join* k1 (g1 d1) k2 (g2 d2))]))]

            [else ; ($empty? d1)
             (g2 d2)])))]

      [else ;; ($empty? d2)
       (g1 d1)])]

    [($leaf? d1)
     (let ([k1 ($leaf-key d1)])
       (let merge0 ([d2 d2])
         (cond
          [(eq? d1 d2)
           (id d1)]

          [($branch? d2)
           (let-branch
            ([(p2 m2 l2 r2) d2])
            (cond [(nomatch? k1 p2 m2) (join* k1 (g1 d1) p2 (g2 d2))]
                  [(fx<= k1 p2)        (bin p2 m2 (merge0 l2) (g2 r2))]
                  [else                (bin p2 m2 (g2 l2) (merge0 r2))]))]

          [($leaf? d2)
           (let ([k2 ($leaf-key d2)])
             (cond [(fx= k1 k2) (f d1 d2)]
                   [else        (join* k1 (g1 d1) k2 (g2 d2))]))]

          [else ; ($empty? d2)
           (g1 d1)])))]

    [else ; ($empty? d1)
     (g2 d2)]))

 (define-syntax let-branch
   (syntax-rules ()
     [(_ ([(p m l r) d] ...) exp ...)
      (let ([p ($branch-prefix d)] ...
            [m ($branch-mask d)] ...
            [l ($branch-left d)] ...
            [r ($branch-right d)] ...)
        exp ...)])))
