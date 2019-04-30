(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (let ((leftbranch (left-branch mobile))
        (rightbranch (right-branch mobile)))
        (+
         (if (pair? (branch-structure leftbranch))
             (total-weight (branch-structure leftbranch))
             (branch-structure leftbranch))
         (if (pair? (branch-structure rightbranch))
             (total-weight (branch-structure rightbranch))
             (branch-structure rightbranch)))
    )
  )

(define total-weight-test (total-weight (make-mobile
                                    (make-branch 2 (make-mobile (make-branch 1 3) (make-branch 1 4)))
                                    (make-branch 2 (make-mobile (make-branch 1 5) (make-branch 1 6))))))

