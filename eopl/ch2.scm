(define-syntax equal??
  (syntax-rules ()
    ((_ x y)
     (let ((x^ x) (y^ y))
       (if (not (equal? x y))
           (error 'equal??
                       "~s is not equal to ~s" 'x 'y)
           (printf "unit tests completed"))))))

(define report-unit-tests-completed
  (lambda (fn-name)
    (printf "unit tests completed: ~s~%" fn-name)))


(let ()
  (define zero (lambda () '()))
  (define is-zero? (lambda (n) (null? n)))
  (define successor (lambda (n) (cons #t n)))
  (define predecessor (lambda (n) (cdr n)))

  (define (plus x y)
    (if (is-zero? x)
        y
        (successor (plus (predecessor x) y))))

  (define (scheme-int->my-int n)
    (if (zero? n) (zero)
        (successor (scheme-int->my-int (- n 1)))))

  (define (my-int->scheme-int x)
    (if (is-zero? x) 0
        (+ 1 (my-int->scheme-int (predecessor x)))))

  (equal??
   (my-int->scheme-int
    (plus
     (scheme-int->my-int 3)
     (scheme-int->my-int 7)))
   10)

  (report-unit-tests-completed 'unary-representation)
  )


;; empty-env : () -> Env
(define empty-env
  (lambda () (list 'empty-env)))

;; extend-env : Var * Schemeval * Env -> Env
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

;; apply-env : Env * Var -> Schemeval
(define apply-env
  (lambda (env search-var)
    (cond
     ((eqv? (car env) 'empty-env)
      (report-no-binding-found search-var))
     ((eqv? (car env) 'extend-env)
      (let ((saved-var (cadr env))
            (saved-val (caddr env))
            (saved-env (cadddr env)))
        (if (eqv? search-var saved-var)
            saved-val
            (apply-env saved-env search-var))))
     (else
      (report-invalid-env env)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))

(equal?? (apply-env e 'd) 6)
(equal?? (apply-env e 'y) 8)
(equal?? (apply-env e 'x) 7)

(report-unit-tests-completed 'apply-env)


(define var-exp
  (lambda (var)
    `(var-exp ,var)))

(define lambda-exp
  (lambda (var lc-exp)
    `(lambda-exp ,var ,lc-exp)))

(define app-exp
  (lambda (lc-exp1 lc-exp2)
    `(app-exp ,lc-exp1 ,lc-exp2)))

(define var-exp?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'var-exp))))

(define lambda-exp?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'lambda-exp))))

(define app-exp?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'app-exp))))

(define var-exp->var
  (lambda (x)
    (cadr x)))

(define lambda-exp->bound-var
  (lambda (x)
    (cadr x)))

(define lambda-exp->body
  (lambda (x)
    (caddr x)))

(define app-exp->rator
  (lambda (x)
    (cadr x)))

(define app-exp->rand
  (lambda (x)
    (caddr x)))

(define occurs-free?
  (lambda (search-var exp)
    (cond
     ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
     ((lambda-exp? exp)
      (and
       (not (eqv? search-var (lambda-exp->bound-var exp)))
       (occurs-free? search-var (lambda-exp->body exp))))
     (else
      (or
       (occurs-free? search-var (app-exp->rator exp))
       (occurs-free? search-var (app-exp->rand exp)))))))

;; a few small unit tests

(equal??
 (occurs-free? 'a (lambda-exp 'a (app-exp (var-exp 'b) (var-exp 'a))))
 #f)

(equal??
 (occurs-free? 'b (lambda-exp 'a (app-exp (var-exp 'b) (var-exp 'a))))
 #t)

(report-unit-tests-completed 'occurs-free?)

