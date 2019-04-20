(define (fast-expt b n)
  (expt-iter b n 1))

(define (expt-iter b n a)
  (cond ((= n 0)
         a)
        ((even? n)
         (expt-iter (square b)
                    (/ n 2)
                    a))
        ((odd? n)
         (expt-iter b
                    (- n 1)
                    (* b a)))))

(define (square x) (* x x))

(define (exp-iter b n)
  (define (exp-iter-internal a b n)
    (cond ((= n 1) (* a b))
          ((even? n) (exp-iter-internal a (* b b) (/ n 2)))
          (else
           (exp-iter-internal (* a a) (* b b) (/ (- n 1) 2)))))
  (exp-iter-internal 1 b n))
