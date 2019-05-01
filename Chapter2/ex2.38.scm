(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car test))
              (cdr rest))))
  (iter initial sequence))

(fold-left / 1 (list 1 2 3))
(fold-left list '() (list 1 2 3)
