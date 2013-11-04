(define (counter a)
    (define (next to-add)
        (counter (+ a to-add))
    )
    (cons a next)
)

(define result (counter 5))
(define result2 ((cdr result) 100))
(define result3 ((cdr result2) 50))
(define result4 ((cdr result) 30))

