(define x (list (list 1 2) (list 3 4)))

(define (reverse a)
  (if (null? a)
      '()
      (append (reverse (cdr a))
              (list (car a)))))

(define (deep-reverse a)
  (if (null? a)
      '()
      (append (deep-reverse (cdr a))
              (if (pair? (car a))
                  (list (reverse (car a)))
                  (list (car a))))))
