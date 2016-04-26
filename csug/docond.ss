(define docond-ht (make-eq-hashtable))
(hashtable-set! docond-ht '&condition '())
(define (docond expr)
  (syntax-case expr (define-condition-type)
    [(define-condition-type &name &parent make-name name?
       (field-name field-accessor) ...)
     (let ([pfields (hashtable-ref docond-ht #'&parent #f)])
       (unless pfields (error 'docond "unrecognized parent ~s" #'&parent))
       (printf "\\formdef{~s}{\\categorysyntax}{~s}\n" #'&name #'&name)
       (let ([fields (append pfields #'(field-name ...))])
         (printf "\\formdef{~s}{\\categoryprocedure}{(~s~{ \\var{~s}~})}\n"
           #'make-name #'make-name fields)
         (hashtable-set! docond-ht #'&name fields))
       (printf "\\returns a condition of type \\scheme{~s}\n" #'&name)
       (printf "\\formdef{~s}{\\categoryprocedure}{(~s \\var{obj})}\n" #'name? #'name?)
       (printf "\\returns \\scheme{#t} if \\var{obj} is a condition of type \\scheme{~s}, \\scheme{#f} otherwise\n"
         #'&name)
       (for-each
         (lambda (field get-field)
           (printf "\\formdef{~s}{\\categoryprocedure}{(~s \\var{condition})}\n" get-field get-field)
           (printf "\\returns the contents of \\var{condition}'s \\scheme{~s} field\n" field))
         #'(field-name ...)
         #'(field-accessor ...))
       (printf "\\listlibraries\n"))]))
