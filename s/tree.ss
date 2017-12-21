(module (empty-tree full-tree tree-extract tree-for-each tree-fold-left tree-bit-set? tree-bit-set tree-bit-unset
                    tree-bit-count tree-bit-length tree-simplify-for-readonly tree-same? tree-merge
                    make-memoized-tree-reduce)
  ; tree -> fixnum | (tree-node tree tree) | #t
  ; 0 represents any tree or subtree with no bits set, and a tree or subtree
  ; with no bits set is always 0.
  ; #t represents a subtree with all bits set
  (define empty-tree 0)

  ; any tree or subtree with all bits set
  (define full-tree #t)

  (define (full-fixnum size) (fxsrl (most-positive-fixnum) (fx- (fx- (fixnum-width) 1) size)))

  (define compute-split
    (lambda (size)
      (fxsrl size 1)
      ; 2015/03/15 rkd: tried the following under the theory that we'd allocate
      ; fewer nodes.  for example, say fixmun-width is 30 and size is 80.  if we
      ; split 40/40 we create two nodes under the current node.  if instead we
      ; split 29/51 we create just one node and one fixnum under the current
      ; node.  this worked as planned; however, it reduced the number of nodes
      ; created by only 3.3% on the x86 and made compile times slightly worse.
      #;(if (fx<= size (fx* (fx- (fixnum-width) 1) 3)) (fx- (fixnum-width) 1) (fxsrl size 1))))

  ;; A tree is represented with pairs so that it can serve directly as
  ;; a livemask when paired with its sized, as long as the tree
  ;; doesn't have any `full-tree` parts
  (module (make-tree-node tree-node? tree-node-left tree-node-right)
    (define make-tree-node cons)
    (define tree-node? pair?)
    (define tree-node-left car)
    (define tree-node-right cdr))

  (define tree-extract ; assumes empty-tree is 0
    (lambda (st size v)
      (let extract ([st st] [size size] [offset 0] [x* '()])
        (cond
          [(fixnum? st)
            (do ([st st (fxsrl st 1)]
                 [offset offset (fx+ offset 1)]
                 [x* x* (if (fxodd? st) (cons (vector-ref v offset) x*) x*)])
             ((fx= st 0) x*))]
          [(eq? st full-tree)
           (do ([size size (fx- size 1)]
                [offset offset (fx+ offset 1)]
                [x* x* (cons (vector-ref v offset) x*)])
             ((fx= size 0) x*))]
          [else
            (let ([split (compute-split size)])
              (extract (tree-node-right st) (fx- size split) (fx+ offset split)
                       (extract (tree-node-left st) split offset x*)))]))))

  (define tree-for-each ; assumes empty-tree is 0
    (lambda (st size start end action)
      (let f ([st st] [size size] [start start] [end end] [offset 0])
        (cond
          [(fixnum? st)
           (unless (eq? st empty-tree)
             (do ([st (fxbit-field st start end) (fxsrl st 1)] [offset (fx+ offset start) (fx+ offset 1)])
                 ((fx= st 0))
               (when (fxodd? st) (action offset))))]
          [(eq? st full-tree)
           (do ([start start (fx+ start 1)] [offset offset (fx+ offset 1)])
               ((fx= start end))
             (action offset))]
          [else
           (let ([split (compute-split size)])
             (when (fx< start split)
               (f (tree-node-left st) split start (fxmin end split) offset))
             (when (fx> end split)
               (f (tree-node-right st) (fx- size split) (fxmax (fx- start split) 0) (fx- end split) (fx+ offset split))))]))))

  (define tree-fold-left ; assumes empty-tree is 0
    (lambda (proc size init st)
      (let f ([st st] [size size] [offset 0] [init init])
        (cond
          [(fixnum? st)
            (do ([st st (fxsrl st 1)]
                 [offset offset (fx+ offset 1)]
                 [init init (if (fxodd? st) (proc init offset) init)])
             ((fx= st 0) init))]
          [(eq? st full-tree)
           (do ([size size (fx- size 1)]
                [offset offset (fx+ offset 1)]
                [init init (proc init offset)])
             ((fx= size 0) init))]
          [else
            (let ([split (compute-split size)])
              (f (tree-node-left st) split offset
                (f (tree-node-right st) (fx- size split) (fx+ offset split) init)))]))))

  (define tree-bit-set? ; assumes empty-tree is 0
    (lambda (st size bit)
      (let loop ([st st] [size size] [bit bit])
        (cond
          [(fixnum? st)
            (and (not (eqv? st empty-tree))
                 ; fxlogbit? is unnecessarily general, so roll our own
                (fxlogtest st (fxsll 1 bit)))]
          [(eq? st full-tree) #t]
          [else
            (let ([split (compute-split size)])
              (if (fx< bit split)
                  (loop (tree-node-left st) split bit)
                  (loop (tree-node-right st) (fx- size split) (fx- bit split))))]))))

  (define tree-bit-set ; assumes empty-tree is 0
    (lambda (st size bit)
      ; set bit in tree.  result is eq? to tr if result is same as tr.
      (cond
        [(eq? st full-tree) st]
        [(fx< size (fixnum-width))
         (let ([st (fxlogbit1 bit st)])
           (if (fx= st (full-fixnum size))
               full-tree
               st))]
        [else
          (let ([split (compute-split size)])
            (if (eqv? st empty-tree)
                (if (fx< bit split)
                    (make-tree-node (tree-bit-set empty-tree split bit) empty-tree)
                    (make-tree-node empty-tree (tree-bit-set empty-tree (fx- size split) (fx- bit split))))
                (let ([lst (tree-node-left st)] [rst (tree-node-right st)])
                  (if (fx< bit split)
                      (let ([new-lst (tree-bit-set lst split bit)])
                        (if (eq? new-lst lst)
                            st
                            (if (and (eq? new-lst full-tree) (eq? rst full-tree))
                                full-tree
                                (make-tree-node new-lst rst))))
                      (let ([new-rst (tree-bit-set rst (fx- size split) (fx- bit split))])
                        (if (eq? new-rst rst)
                            st
                            (if (and (eq? lst full-tree) (eq? new-rst full-tree))
                                full-tree
                                (make-tree-node lst new-rst))))))))])))

  (define tree-bit-unset ; assumes empty-tree is 0
    (lambda (st size bit)
      ; reset bit in tree.  result is eq? to tr if result is same as tr.
      (cond
        [(fixnum? st)
         (if (eqv? st empty-tree)
             empty-tree
             (fxlogbit0 bit st))]
        [(eq? st full-tree)
         (if (fx< size (fixnum-width))
             (fxlogbit0 bit (full-fixnum size))
             (let ([split (compute-split size)])
               (if (fx< bit split)
                   (make-tree-node (tree-bit-unset full-tree split bit) full-tree)
                   (make-tree-node full-tree (tree-bit-unset full-tree (fx- size split) (fx- bit split))))))]
        [else
          (let ([split (compute-split size)] [lst (tree-node-left st)] [rst (tree-node-right st)])
            (if (fx< bit split)
                (let ([new-lst (tree-bit-unset lst split bit)])
                  (if (eq? new-lst lst)
                      st
                      (if (and (eq? new-lst empty-tree) (eq? rst empty-tree))
                          empty-tree
                          (make-tree-node new-lst rst))))
                (let ([new-rst (tree-bit-unset rst (fx- size split) (fx- bit split))])
                  (if (eq? new-rst rst)
                      st
                      (if (and (eq? lst empty-tree) (eq? new-rst empty-tree))
                          empty-tree
                          (make-tree-node lst new-rst))))))])))

  (define tree-bit-count ; assumes empty-tree is 0
    (lambda (st size)
      (cond
        [(fixnum? st) (fxbit-count st)]
        [(eq? st full-tree) size]
        [else
          (let ([split (compute-split size)])
            (fx+
              (tree-bit-count (tree-node-left st) split)
              (tree-bit-count (tree-node-right st) (fx- size split))))])))

  (define tree-bit-length
    (lambda (st size)
      (cond
        [(fixnum? st) (integer-length st)]
        [(eq? st full-tree) size]
        [else
         (let ([split (compute-split size)])
           (let ([rlen (tree-bit-length (tree-node-right st) (fx- size split))])
             (if (fx= 0 rlen)
                 (tree-bit-length (tree-node-left st) split)
                 (fx+ split rlen))))])))

  ;; If a tree has only a leftmost fixnum, then we can
  ;; reperesent it for `tree-bit-set?` as just the fixnum.
  (define tree-simplify-for-readonly
    (lambda (st size)
      (cond
        [(fixnum? st) st]
        [(eq? st full-tree)
         (and (fx< size (fixnum-width))
              (full-fixnum size))]
        [else
         (and (eq? (tree-node-right st) empty-tree)
              (tree-simplify-for-readonly (tree-node-left st) (compute-split size)))])))

  (define tree-same? ; assumes empty-tree is 0
    (lambda (st1 st2)
      (or (eq? st1 st2) ; assuming fixnums and full trees are eq-comparable
          (and (tree-node? st1)
               (tree-node? st2)
               (tree-same? (tree-node-left st1) (tree-node-left st2))
               (tree-same? (tree-node-right st1) (tree-node-right st2))))))

  (define tree-merge
   ; merge tr1 and tr2.  result is eq? to tr1 if result is same as tr1.
    (lambda (st1 st2 size)
      (cond
        [(or (eq? st1 st2) (eq? st2 empty-tree)) st1]
        [(eq? st1 empty-tree) st2]
        [(or (eq? st1 full-tree) (eq? st2 full-tree)) full-tree]
        [(fixnum? st1)
         (safe-assert (fixnum? st2))
         (let ([st (fxlogor st1 st2)])
           (if (fx= st (full-fixnum size))
               full-tree
               st))]
        [else
         (let ([lst1 (tree-node-left st1)]
               [rst1 (tree-node-right st1)]
               [lst2 (tree-node-left st2)]
               [rst2 (tree-node-right st2)])
           (let ([split (compute-split size)])
             (let ([l (tree-merge lst1 lst2 split)] [r (tree-merge rst1 rst2 (fx- size split))])
             (cond
               [(and (eq? l lst1) (eq? r rst1)) st1]
               [(and (eq? l lst2) (eq? r rst2)) st2]
               [(and (eq? l full-tree) (eq? r full-tree)) full-tree]
               [else (make-tree-node l r)]))))])))

  (define make-memoized-tree-reduce
    ;; A function produced by `make-memoized-tree-reduce` is meant to
    ;; be applied to different trees where the same bit in different
    ;; trees means the same thing. Memoizing reduces over multiple
    ;; trees is useful when the tree are likely to share nodes.
    ;; Memoizing is approximate, applied only when it seems like to be
    ;; worthwhile. Memoizing also relies on results being non-#f.
    (lambda (total-size leaf-proc merge-proc init)
      (cond
       [(< total-size (* 8 (fixnum-width)))
        ;; Memoizing probably isn't worthwhile
        (case-lambda
         [(st)
          (tree-fold-left leaf-proc total-size init st)]
         [(st . extra-leaf-args)
          (tree-fold-left (lambda (init offset)
                            (apply leaf-proc init offset extra-leaf-args))
                          total-size init st)])]
       [else
        ;; Memoizing could be worthwhile...
        (let ([cache (make-eq-hashtable)] ; maps st to a result
              [full-cache (make-eqv-hashtable)] ; maps offset+size to a result for full trees
              [apply-leaf-proc (lambda (init offset extra-leaf-args)
                                 (if (null? extra-leaf-args)
                                     (leaf-proc init offset)
                                     (apply leaf-proc init offset extra-leaf-args)))])
          (lambda (st . extra-leaf-args)
            (let f ([st st] [size total-size] [offset 0])
              (cond
               [(fixnum? st)
                ;; No memoizing at fixnum leaves, since it seems unlikely
                ;; to be useful
                (do ([st st (fxsrl st 1)]
                     [offset offset (fx+ offset 1)]
                     [init init (if (fxodd? st) (apply-leaf-proc init offset extra-leaf-args) init)])
                    ((fx= st 0) init))]
               [(eq? st full-tree)
                ;; Memoizing at full subtrees uses offset and size
                ;; (combined into one number) to identity the subtree.
                (let ([key (+ offset (* total-size size))])
                  (cond
                   [(hashtable-ref full-cache key #f)
                    => (lambda (v) v)]
                   [else
                    (let ([v (do ([size size (fx- size 1)]
                                  [offset offset (fx+ offset 1)]
                                  [init init (apply-leaf-proc init offset extra-leaf-args)])
                                 ((fx= size 0) init))])
                      (hashtable-set! full-cache key v)
                      v)]))]
               [else
                ;; We're relying on a fresh `cons`es to repersent different parts
                ;; of a tree, even if the parts have the same local content. So,
                ;; `eq?` identifies a subtree.
                (let ([cell (eq-hashtable-cell cache st #f)]
                      [key (+ offset (* total-size size))])
                  (cond
                   [(cdr cell) => (lambda (v) v)]
                   [else
                    (let ([v (let ([split (compute-split size)])
                               (merge-proc (f (tree-node-left st) split offset)
                                           (f (tree-node-right st) (fx- size split) (fx+ offset split))))])
                      (set-cdr! cell v)
                      v)]))]))))]))))
