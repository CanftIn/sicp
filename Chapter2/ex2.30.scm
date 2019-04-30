(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree)
         '())
        ((not (pair? tree))
         (square tree))
        (else
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))))

(define square-tree-test (square-tree
                     (list 1
                           (list 2 (list 3 4) 5)
                           6 7)))

(define (square-tree-new tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-new sub-tree)
             (square sub-tree)))
       tree))

(define square-tree-new-test (square-tree-new
                     (list 1
                           (list 2 (list 3 4) 5)
                           6 7)))
