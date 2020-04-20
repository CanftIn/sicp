(define-syntax equal??
  (syntax-rules ()
    ((_ test-exp correct-ans)
     (let ((observed-ans test-exp))
       (if (not (equal? observed-ans correct-ans))
           (printf "~s returned ~s, should have returned ~s~%"
                   'test-exp
                   observed-ans
                   correct-ans)
           (printf "equal pass.\n"))))))


(define in-S?
  (lambda (n)
    (if (zero? n) #t
        (if (>= (- n 3) 0) (in-S? (- n 3))
            #f))))

