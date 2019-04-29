(define (make-accumulator value)
  (lambda (add-value)
    (set! value (+ value add-value))
    value))

(define A (make-accumulator 5))
