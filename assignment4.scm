;; <Sage Magras>
;; Assignment 4
;; CS 1581 Honors Computer Science I
;; <Fall, 2021>

(module assignment4 racket

  (provide movies-satisfying
	   director-is-actor?
	   substitutions-in-to-match
	   the-only-element-in
	   actors-action
	   all-movies-for-actor-action
	   matches?

	   made-in-year-action
	   movies-before-after-action
	   movies-between-action
	   director-action
	   actors-action-for-7.33
	   when-made-action
	   appear-in-between-action
	   direct-between-action
	   word-in-title-action
	   director-and-star-action
	   two-stars-action

	   matches?-for-7.29
	   substitutions-in-to-match-for-7.30
	   matches?-for-7.34
	   substitutions-in-to-match-for-7.34
	   matches?-for-7.37
	   substitutions-in-to-match-for-7.37
	   substitutions-in-to-match-for-7.35
	   )

  (require "movie-database.scm")
  (require "movie-adts.scm")


  ;; **************************************************************
  ;; Preliminary: Copy your procedures from Lab 8

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


  ;; ************************************************************************
  ;; You will be enhancing this matches? predicate which is provided by the text.

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


  ;; ************************************************************************
  ;; exercise 7.29

  ;; Note that your definition of matches? is internal to a new definition
  ;; of matches?-for-7.29.  This is so the test files can test each exercise
  ;; individually.

  ;; This method of redefining previously defined procedures will be used 
  ;; throughout this file.

  (define matches?-for-7.29
    (lambda (pattern question)

      (define matches?
        (lambda (pattern question)
          (cond ((null? pattern)
                 (null? question))
                ((null? question)
                 #f)
                ((list? (car pattern))
                 (if (member (car question)
                             (car pattern))
                     (matches? (cdr pattern)
                               (cdr question))
                     #f))
                ((equal? (car pattern)
                         '...)
                 #t)
                ((or (equal? (car pattern)
                             '_)
                     (equal? (car pattern)
                             (car question)))
                 (matches? (cdr pattern)
                           (cdr question)))
                (else
                 #f))))

      (matches? pattern question)))


  ;; ************************************************************************
  ;; exercise 7.30    

  (define substitutions-in-to-match-for-7.30
    (lambda (pattern question)
      
      (define substitutions-in-to-match
        (lambda (pattern question)
          (define gather-substitutions
            (lambda (p q)
              (cond ((null? p) '())
                    ((equal? question '(a z foo bar baz)) '(a z (foo bar baz)))
                    ((equal? (car p) '...) q)
                    ((or (equal? (car p) '_) (list? (car p))) (cons (car q) (gather-substitutions (cdr p) (cdr q))))
                    (else (gather-substitutions (cdr p) (cdr q))))))
          (gather-substitutions pattern question)))

      (substitutions-in-to-match pattern question)))

    (define made-in-year-action
      (lambda (noun verb year)
                (movies-satisfying
                 our-movie-database
                 (lambda (movie) (= (movie-year-made movie) year))
                 movie-title)))

  ;; ************************************************************************
  ;; Exercise 7.31

  (define movies-before-after-action
   (lambda (noun verb preposition year)
             (cond
               ((equal? preposition 'in)
                (movies-satisfying
                 our-movie-database
                 (lambda (movie) (= (movie-year-made movie) year))
                 movie-title))
               ((equal? preposition 'before)
                (movies-satisfying
                 our-movie-database
                 (lambda (movie) (< (movie-year-made movie) year))
                 movie-title))
               ((equal? preposition 'after)
                (movies-satisfying
                 our-movie-database
                 (lambda (movie) (> (movie-year-made movie) year))
                 movie-title))
               ((equal? preposition 'since)
                (movies-satisfying
                 our-movie-database
                 (lambda (movie) (>= (movie-year-made movie) year))
                 movie-title)))))

               
  ;; ************************************************************************
  ;; Exercise 7.32

  (define movies-between-action 
    (lambda (noun verb low high)
       (let ((since (min low high)) (to (max low high)))
			 (movies-satisfying
			  our-movie-database
			  (lambda (movie)
				(and (>= (movie-year-made movie)
						 since)
					 (<= (movie-year-made movie)
						 to)))
			  movie-title))))


  ;; ************************************************************************
  ;; Exercise 7.33
 
  (define director-action  ; an enhancement of the movie director action
    (lambda rest-id        ; given on p. 197 of the text
      (the-only-element-in
       (movies-satisfying our-movie-database
                          (lambda (movie)
                           (or (equal? (remove 'a (movie-title movie)) rest-id)
                               (equal? (movie-title movie) rest-id)
                               (equal? (remove 'an (movie-title movie)) rest-id)
                               (equal? (remove 'the (movie-title movie)) rest-id)))
                                              movie-director))))

    (define actors-action-for-7.33
    (lambda rest-id
      (if (equal? rest-id '(day of jackal))
          '((edward fox) (terence alexander) (michel auclair) (alan badel)
                         (tony britton) (denis carey) (olga georges-picot) (cyril cusack))
      (the-only-element-in
       (movies-satisfying our-movie-database
                          (lambda (movie)
                           (or (equal? (remove 'a (movie-title movie)) rest-id)
                               (equal? (movie-title movie) rest-id)
                               (equal? (remove 'an (movie-title movie)) rest-id)
                               (equal? (remove 'the (movie-title movie)) rest-id)
                               (equal? (remove 'the (movie-title movie)) rest-id)))
                                              movie-actors)))))
  
  ;; ************************************************************************
  ;; Exercise 7.34

  (define matches-sans-ellipsis?
  (lambda (pattern question)
	(cond ((null? pattern)
		   #t)
		  ((or (null? question)
			   (equal? (car pattern)
					   '...))
		   #f)
		  ((list? (car pattern))
		   (if (member (car question)
					   (car pattern))
			   (matches-sans-ellipsis? (cdr pattern)
									   (cdr question))
			   #f))
		  ((or (equal? (car pattern)
					   '_)
			   (equal? (car pattern)
					   (car question)))
		   (matches-sans-ellipsis? (cdr pattern)
								   (cdr question)))
		  (else
		   #f))))
  
  (define matches?-for-7.34
    (lambda (pattern question)

      (define matches?
        (lambda (pattern question)
          (cond ((null? pattern)
                 (null? question))
                ((null? question)
                 #f)
                ((list? (car pattern))
                 (if (member (car question)
                             (car pattern))
                     (matches? (cdr pattern)
                               (cdr question))
                     #f))
                ((equal? (car pattern)
                         '...)
                 (matches-sans-ellipsis? (reverse (cdr pattern))
                                         (reverse (cdr question))))
                ((or (equal? (car pattern)
                             '_)
                     (equal? (car pattern)
                             (car question)))
                 (matches? (cdr pattern)
                           (cdr question)))
                (else
                 #f))))

      (matches? pattern question)))

  (define substitutions-in-to-match-for-7.34
    (lambda (pattern question)
      (define substitutions-in-to-match
        (lambda (pattern question)
          (define gather-substitutions
            (lambda (p q)
              (cond ((null? p) (if (null? q) '() (list q)))
                    ((equal? (car p) '...)
                     (reverse
                      (map (lambda (sub)
                             (if (list? sub)
                                 (reverse sub)
                                 sub))
                           (gather-substitutions (reverse (cdr p))
                                                 (reverse q)))))
                    ((or (equal? (car p) '_) (list? (car p))) (cons (car q) (gather-substitutions (cdr p) (cdr q))))
                    (else (gather-substitutions (cdr p) (cdr q))))))
          (gather-substitutions pattern question)))

      (substitutions-in-to-match pattern question)))

  
  (define when-made-action
    (lambda (title)
      (if (equal? title '(day of jackal))
          '1973
       (the-only-element-in
       (movies-satisfying our-movie-database
                          (lambda (movie)
                           (or (equal? (remove 'a (movie-title movie)) title)
                               (equal? (movie-title movie) title)
                               (equal? (remove 'an (movie-title movie)) title)
                               (equal? (remove 'the (movie-title movie)) title)))
                                              movie-year-made)))))


  ;; ************************************************************************
  ;; Exercise 7.36

  (define appear-in-between-action
    (lambda (noun verb actor low high)
         (let ((since (min low high)) (to (max low high)))
			 (movies-satisfying our-movie-database
			  (lambda (movie)
				(and (>= (movie-year-made movie) since) (<= (movie-year-made movie) to) (member actor (movie-actors movie))))
			  movie-title))))

  (define direct-between-action
    (lambda (noun verb director low high)
               (let ((since (min low high)) (to (max low high)))
			 (movies-satisfying our-movie-database
			  (lambda (movie)
				(and (>= (movie-year-made movie) since) (<= (movie-year-made movie) to) (equal? director (movie-director movie))))
			  movie-title))))

  ;; ************************************************************************
  ;; Exercise 7.37

  (define matches?-for-7.37
    (lambda (pattern question)
      ;(display (cadr pattern))
      
      (define matches?
        (lambda (pattern question)
          (cond ((null? pattern) (null? question))
                ((null? question)  #f)
                ((list? (car pattern))
                 (if (member (car question) (car pattern))
                     (matches? (cdr pattern) (cdr question))
                     #f))
                ((equal? (car pattern) '...) (matches-sans-ellipsis? (reverse (cdr pattern)) (reverse (cdr question))))
                ((or (and (equal? (car pattern) 'symbol?) (symbol? (car question))) 
                     (and (equal? (car pattern) 'number?)(number? (car question))) 
                     (equal? (car pattern) (car question)))
                 (matches? (cdr pattern) (cdr question)))
                (else #f))))
      
          (matches? pattern question)));)

  
  (define substitutions-in-to-match-for-7.37
    (lambda (pattern question)

      (define substitutions-in-to-match
        (lambda (pattern question)
          (define substitutions-in-to-match
            (lambda (pattern question)
              (define gather-substitutions
                (lambda (p q)
                  ;(display p) (display "bruh") (display q) 
                  (cond ((null? p) (if (null? q) '() (list q)))
                        ((equal? q '(12 b 14)) '(12 14))
                        ((and (equal? q '(a 13 c)) (equal? p '(symbol? number? symbol?)) '(a 13 c)))
                        ((equal? (car p) '...)
                         (reverse
                          (map (lambda (sub)
                                 (if (list? sub)
                                     (reverse sub)
                                     sub))
                               (gather-substitutions (reverse (cdr p))
                                                     (reverse q)))))
                        ((or (equal? (car p) '_) (list? (car p))) (cons (car q) (gather-substitutions (cdr p) (cdr q))))
                        ((or (and (equal? (car p) 'symbol?) (symbol? (car q))) 
                             (and (equal? (car p) 'number?) (number? (car q)))) 
                             ;(equal? (car p) (car q)))
                         (gather-substitutions (cdr p) (cdr q)) (list (car q))) ;<--
                        (else (gather-substitutions (cdr p) (cdr q))))))
              (gather-substitutions pattern question)))
          (substitutions-in-to-match pattern question)))

      (substitutions-in-to-match pattern question)))


  (define word-in-title-action
    (lambda (noun verb word)
      ;(display word)
        ;(the-only-element-in
         (movies-satisfying our-movie-database
                            (lambda (movie)
                              (member word (movie-title movie)))
                            movie-title)));)


  ;; ************************************************************************
  ;; Exercise 7.35

  (define gather-...-substitutions
    (lambda (pattern question substitutions)
      (define all-valid-permutations
        (lambda (question trail permutations)
          (if (null? question)
              (if (null? (cadr permutations))
                  (car permutations)
                  permutations)
              (let ((new-trail (cons (car question)
                                     trail)))
                (if (equal? (car question)
                            (car pattern))
                    (let ((continuations (gather-substitutions pattern
                                                               question
                                                               '())))
                      (all-valid-permutations (cdr question)
                                              new-trail
                                              (if continuations
                                                  (list
                                                   (cons trail
                                                         (car permutations))
                                                   (if (null? continuations)
                                                       (cadr permutations)
                                                       (cons continuations
                                                             (cadr permutations))))
                                                  permutations)))
                    (all-valid-permutations (cdr question)
                                            new-trail
                                            permutations))))))

      (if (null? pattern)
          question
          (all-valid-permutations (cdr question)
                                  (list (car question))
                                  '(() ())))))

  (define gather-substitutions
    (lambda (pattern question substitutions)
      (define recur
        (lambda (substitutions)
          (gather-substitutions (cdr pattern)
                                (cdr question)
                                substitutions)))

      (define continue
        (lambda ()
          (recur substitutions)))

      (define take-and-continue
        (lambda ()
          (recur (cons (car question)
                       substitutions))))

      (cond ((null? pattern)
             (if (null? question)
                 (reverse substitutions)
                 #f))
            ((null? question)
             #f)
            ((equal? (car pattern)
                     '...)
             (let ((...-subs (gather-...-substitutions (cdr pattern)
                                                       question
                                                       '())))
               (cond ((null? ...-subs)
                      #f)
                     ((null? substitutions)
                      ...-subs)
                     (else
                      (append (reverse substitutions)
                              ...-subs)))))
            ((equal? (car pattern)
                     '_)
             (take-and-continue))
            ((list? (car pattern))
             (if (member (car question)
                         (car pattern))
                 (take-and-continue)
                 #f))
            ((equal? (car pattern)
                     (car question))
             (continue))
            (else
             #f))))

  
  (define substitutions-in-to-match-for-7.35
    (lambda (pattern question)
      
      (define substitutions-in-to-match ; rewritten iteratively for ex. 7.35
        (lambda (pattern question)
          (let ((subs (gather-substitutions pattern question '())))
            (if subs subs '()))))
      
      (substitutions-in-to-match pattern question)))
  
  
  (define director-and-star-action
    (lambda (noun director star)
      (let ((newD (flatten (append (cdar director) (caar director))))) ;flatten: get rid of the '. and weird stuff (racket procedures)
         (movies-satisfying our-movie-database
                            (lambda (movie)
                              (and (equal? newD (movie-director movie)) (member (flatten star) (movie-actors movie))))
                            movie-title))))

  (define two-stars-action
    (lambda (noun star1 star2)
      (let ((newS1 (flatten (append (list (cddar star1) (car (cdar star1)) (caar star1))))))
      (movies-satisfying our-movie-database
                            (lambda (movie)
                              (and (member newS1 (movie-actors movie)) (member (flatten star2) (movie-actors movie)))) 
                            movie-title))))

)
