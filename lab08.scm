;; <Sage Magras>
;; Lab Exercise 8
;; CS 1581 Honors Computer Science I
;; <fall, 2021>

(module lab08 racket

  (provide movies-satisfying
	   director-is-actor?
	   the-only-element-in
	   actors-action
	   all-movies-for-actor-action
	   substitutions-in-to-match
	   matches?)

  (require "movie-database.scm")
  (require "movie-adts.scm")

  ;; **************************************************************
  ;; Exercise 7.24, p. 190
  
  (define movies-satisfying
  (lambda (movies pred selector)
	(map selector (filter pred movies))))

  ;; An added problem for 7.24
  (define director-is-actor?
    (lambda (movie)
      (define looped
        (lambda (actor)
          (if (member (movie-director movie) actor)
              #t
              #f)))
       (looped (movie-actors movie))))


  ;; **************************************************************
  ;; Exercise 7.26, p. 197

;(define substitutions-in-to-match ;!!!! works, but adds one extra set of parenthesis for the first test, !!!!
;  (lambda (pattern query)         ;not sure how to fix, but adding an extra set to the test "equals?" answer makes everything work  !!!!
;    (cond ((equal? pattern query) '() )) 
;	(define skip-to-wildcard
;	  (lambda (p q)
;		(if (or (null? p) (equal? (car p) '...))
;			q
;			(skip-to-wildcard (cdr p) (cdr q)))))
;	(list (skip-to-wildcard pattern query))))
  (define substitution-in-to-match-internal
  (lambda (pattern question accumulator)
    (cond
      ((null? pattern)
       accumulator)
      ((equal? (car pattern) '...)
       (cons question accumulator))
      ((list? (car pattern))
       (if (null? (member (car question) (car pattern)))
           #f
           (substitution-in-to-match-internal (cdr pattern) (cdr question) (cons (car question) accumulator))))
      ((not (list? (car pattern)))
       (if (equal? (car pattern) (car question))
           (substitution-in-to-match-internal (cdr pattern) (cdr question) accumulator)
           #f))
      (else
       '()))))

(define substitutions-in-to-match
  (lambda (pattern question)
    (reverse (substitution-in-to-match-internal pattern question '()))))

  ;; **************************************************************
  ;; Exercise 7.27, p. 197

  ;; You don't have to write any code for this exercise, just run the
  ;; test file.


  ;; **************************************************************
  ;; Exercise 7.28, p. 197

  (define the-only-element-in
  (lambda (lst)
	(cond ((null? lst)
		   '())
		  ((null? (cdr lst))
		   (car lst))
		  (else
		   lst))))

(define movie-p/a-list
  (list (make-pattern/action
		 '(who is the director of ...)
		 (lambda (title)
		   (movies-satisfying
			our-movie-database
			(lambda (movie)
			  (equal? (movie-title movie) title))
                        movie-director)))
		(make-pattern/action '(who acted in ...)
		 (lambda (title)
		   (the-only-element-in (movies-satisfying our-movie-database
			 (lambda (movie)
			   (equal? (movie-title movie)
					   title))
			 movie-actors))))))


  ;; **************************************************************
  ;; Exercise 7.25, p. 196

(define actors-action
  (lambda (title)
    (the-only-element-in
     (movies-satisfying 
      our-movie-database
      (lambda (movie) (equal? (movie-title movie) title))
      movie-actors))))

  (define all-movies-for-actor-action
    (lambda (actor)
       (movies-satisfying our-movie-database (lambda (movie) (member actor (movie-actors movie))) movie-title)))

  ;; **************************************************************
  ;; matches? is provided to run the tests

  (define matches?
    (lambda (pattern question)
      (cond ((null? pattern)  (null? question))
	    ((null? question) #f)
	    ((list? (car pattern))
	     (if (member (car question) (car pattern))
		 (matches? (cdr pattern)
			   (cdr question))
		 #f))
	    ((equal? (car pattern) '...) #t)
	    ((equal? (car pattern) (car question))
	     (matches? (cdr pattern)
		       (cdr question)))
	    (else #f))))

)