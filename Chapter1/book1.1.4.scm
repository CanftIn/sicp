(define (square x)
    (* x x))

(begin  (display
            (square 21))
        (newline)
        (display
            (square (+ 2 5)))
        (newline)
        (display
            (square (square 3)))
        (newline))

(define (sum-of-squares x y)
    (+  (square x) (square y)))

(begin  (display
            (sum-of-squares 3 4))
        (newline))