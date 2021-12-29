
(module lab05 racket

  (provide sum-func
	   make-exponentiator
	   make-series
	   make-verifier
	   f-upc)


  ;; ****************************************************************************
  ;; Exercise 5.6, p. 113
  (define sum-func
  (lambda (low high f)
	(define sum-range
	  (lambda (x total)
		(if (> x high)
			total
			(sum-range (+ x 1)
					   (+ total (f x))))))
	(sum-range low 0)))

  ;; ****************************************************************************
  ;; Exercise 5.7, p. 119

  (define make-exponentiator
    (lambda (e)
      (lambda (x) (expt x e))))


  ;; ****************************************************************************
  ;; Exercise 5.8, p. 120
  (define make-series
  (lambda (f)
	(define countdown
	  (lambda (n result)
		(if (= n 1)
			result
			(countdown (- n 1)
					   (f n result)))))

	(lambda (n)
	  (countdown n 1))))

  ;; ****************************************************************************
  ;; Exercise 5.11, p. 121
  (define evenly-divisible-by?
  (lambda (x y)
	(= (remainder x y) 0)))
  
  (define sum-digits-map
  (lambda (f m)
	(define sum-map
	  (lambda (m index total)
		(if (= m 0)
			total
			(sum-map (quotient m 10) (+ index 1) (+ (f index (remainder m 10)) total) ))))
	(sum-map m 1 0)))

(define make-verifier
  (lambda (f m)
	(lambda (n)
	  (evenly-divisible-by? (sum-digits-map f n) m))))


  ;; ****************************************************************************
  ;; Exercise 5.12, p. 122
(define f-upc
   (lambda (i di)
       (if (even? i)
	   (* 3 di) di)))
)
