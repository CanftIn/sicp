(define (mycons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (mycar z)
  (if (= 0 (remainder z 2))
      (+ 1 (mycar (/ z 2)))
      0))

(define (mycdr z)
  (if (= 0 (remainder z 3))
      (+ 1 (cdr (/ z 3)))
      0))
