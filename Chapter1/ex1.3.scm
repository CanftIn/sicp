(define (three_big x y z)
    (if (> x y)
        (if (> y z)
            y
            z
            )
        ; y 较大
        (if (> x z)
            x
            z
            )))

(define (bigger x y)
    (if (> x y)
        x
        y))

(define (smaller x y)
    (if (> x y)
        y
        x))

;(define (bigger) (bigger x y))
(define (another-bigger x y z) (bigger (smaller x y) z))

