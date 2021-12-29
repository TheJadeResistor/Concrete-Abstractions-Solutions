;; <Sage Magras>
;; Lab Exercise 7
;; CS 1581 Honors Computer Science I
;; <Fall, 2021>

(module lab07 racket

  (provide integers-from-to
	   count
	   count-satisfying
           my-list-ref
           position)

  ;; **************************************************************
  ;; Exercise 7.4, p. 172

(define integers-from-to
  (lambda (low high)
    (define iter
      (lambda (low lst)
	(if (> low high)
            lst
            (iter (+ 1 low) (cons low lst)))))
	(reverse (iter low '()))))

  ;; **************************************************************
  ;; Exercise 7.6, p. 173

  ;; Part a
(define count
  (lambda (element lst)
    (define helper
      (lambda (lst instances)
	(if (null? lst)
            instances
            (helper (cdr lst)
              (if (equal? (car lst) element)
		(+ instances 1)
		instances)))))
	(helper lst 0)))

  ;; Part b
(define count-satisfying
  (lambda (pred? lst)
     (define count
       (lambda (lst matches)
	(if (null? lst)
       	     matches
	    (count (cdr lst)
	        (if (pred? (car lst)) (+ matches 1)
		     matches)))))
	(count lst 0)))

  ;; **************************************************************
  ;; Exercise 7.7, p. 174
  (define my-list-ref
    (lambda (lst n)
      (define iter
        (lambda (lrem prem)
          (cond ((null? lrem) (car '(!!!ERROR!!!))) ;n lst))
                ((= prem 0) (car lrem))
                (else (iter (cdr lrem) (- prem 1))))))
      (iter lst n)))

  ;; **************************************************************
  ;; Exercise 7.8e, p. 174
(define index-of
  (lambda (pred? lst)
	(define iter
	  (lambda (lst pos)
		(cond ((null? lst)
			   -1);#f
			  ((pred? (car lst))
			   pos)
			  (else
			   (iter (cdr lst)
					 (+ pos 1))))))
	(iter lst 0)))

(define position
  (lambda (element lst)
	(index-of (lambda (x)
				(equal? x element))
			  lst)))

)