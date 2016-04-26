;;; freq.ss
;;; Copyright (C) 1996 R. Kent Dybvig
;;; from "The Scheme Programming Language, 2ed" by R. Kent Dybvig

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

;;; If the next character on p is a letter, get-word reads a word
;;; from p and returns it in a string.  If the character is not a
;;; letter, get-word returns the character (on eof, the eof-object).
(define get-word
  (lambda (p)
    (let ((c (read-char p)))
      (if (eq? (char-type c) 'letter)
          (list->string
            (let loop ((c c))
              (cons c
                (if (memq (char-type (peek-char p)) '(letter digit))
                    (loop (read-char p))
                    '()))))
          c))))

;;; char-type tests for the eof-object first, since the eof-object
;;; may not be a valid argument to char-alphabetic? or char-numeric?
;;; It returns the eof-object, the symbol letter, the symbol digit,
;;; or the argument itself if it is not a letter or digit.
(define char-type
  (lambda (c)
    (cond
      ((eof-object? c) c)
      ((char-alphabetic? c) 'letter)
      ((char-numeric? c) 'digit)
      (else c))))

;;; Trees are represented as vectors with four fields: word, left,
;;; right, and count.  Only one field, word, is initialized by an
;;; argument to the constructor procedure make-tree.  The remaining
;;; fields are explicitly initialized and changed by subsequent
;;; operations.  Most Scheme systems provide structure definition
;;; facilities that automate creation of structure manipulation
;;; procedures, but we simply define the procedures by hand here.
(define make-tree
  (lambda (word)
    (vector word '() '() 1)))

(define tree-word (lambda (tree) (vector-ref tree 0)))

(define tree-left (lambda (tree) (vector-ref tree 1)))
(define set-tree-left!
  (lambda (tree new-left)
    (vector-set! tree 1 new-left)))

(define tree-right (lambda (tree) (vector-ref tree 2)))
(define set-tree-right!
  (lambda (tree new-right)
    (vector-set! tree 2 new-right)))

(define tree-count (lambda (tree) (vector-ref tree 3)))
(define set-tree-count!
  (lambda (tree new-count)
    (vector-set! tree 3 new-count)))

;;; If the word already exists in the tree, tree increments its
;;; count.  Otherwise, a new tree node is created and put into the
;;; tree.  In any case, the new or modified tree is returned.
(define tree
  (lambda (node word)
    (cond
      ((null? node) (make-tree word))
      ((string=? word (tree-word node))
       (set-tree-count! node (+ (tree-count node) 1))
       node)
      ((string<? word (tree-word node))
       (set-tree-left! node (tree (tree-left node) word))
       node)
      (else
       (set-tree-right! node (tree (tree-right node) word))
       node))))

;;; tree-print prints the tree in "in-order," i.e., left subtree,
;;; then node, then right subtree.  For each word, the count and the
;;; word are printed on a single line.
(define tree-print
  (lambda (node p)
    (if (not (null? node))
        (begin
          (tree-print (tree-left node) p)
          (write (tree-count node) p)
          (write-char #\space p)
          (display (tree-word node) p)
          (newline p)
          (tree-print (tree-right node) p)))))

;;; frequency is the driver routine.  It opens the files, reads the
;;; words, and enters them into the tree.  When the input port
;;; reaches end-of-file, it prints the tree and closes the ports.
(define frequency
  (lambda (infn outfn)
    (let ((ip (open-input-file infn))
          (op (open-output-file outfn)))
      (let loop ((root '()))
        (let ((w (get-word ip)))
          (cond
             ((eof-object? w) (tree-print root op))
             ((string? w) (loop (tree root w)))
             (else (loop root)))))
       (close-input-port ip)
       (close-output-port op))))
