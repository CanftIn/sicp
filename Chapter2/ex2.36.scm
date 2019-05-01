(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (get-first-elements seqs)
  (if (null? seqs)
      '()
      (cons (car (car seqs))
            (get-first-elements (cdr seqs)))))

(define (get-cdr-elements seqs)
  (if (null? seqs)
      '()
      (cons (cdr (car seqs))
            (get-cdr-elements (cdr seqs)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (get-first-elements seqs))
             (accumulate op init (get-cdr-elements seqs)))))

