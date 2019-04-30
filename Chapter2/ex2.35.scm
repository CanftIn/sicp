(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree) 
	(cond ((null? tree) '())
        ((not (pair? tree)) (list 1))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree)))))
  )

(define (count-leaves t)
	(accumulate + 0 (map (lambda (x) x) (enumerate-tree t)))
  )
