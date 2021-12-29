;; Sage Magras
;; Lab Exercise 1
;; CS 1581 Honors Computer Science I
;; <Fall 2021>

(module lab01 racket

  (provide turkey-servings
	   1.7a
	   1.7b
	   half-turn
	   quarter-turn-left
	   side-by-side
	   pinwheel
	   corner-bb
	   rcross-bb
	   my-bb)

  (require (lib "fungraph.ss" "concabs"))

;; **************************************************************
;; Textbook Exercise 1.6

  ;(define turkey-servings lambda (lbs))
  (define turkey-servings
    (lambda (lbs)
      (if (<= lbs 12)
          (/ lbs 3/4)
      (/ lbs 1/2))))

;; **************************************************************
;; Textbook Exercise 1.7

  (define 1.7a "puzzle1 computes the sum of a and b if b is larger than c, if b is smaller it computes the sum of a and c" )

  (define 1.7b "puzzle2 computes the absolute value of a number entered, if 0 is entered it returns zero" )

;; **************************************************************
;; Textbook Exercise 1.9

  (define half-turn
    (lambda (tri)
      (quarter-turn-right(quarter-turn-right tri))))

  (define quarter-turn-left 
    (lambda (tri)
      (quarter-turn-right(quarter-turn-right(quarter-turn-right tri)))))

  (define side-by-side 
    (lambda (tri1 tri2)
      (quarter-turn-left(stack ;horizontal tri's without turn
                         (quarter-turn-right tri1)
                         (quarter-turn-right tri2)))))

;; **************************************************************
;; Textbook Exercise 1.10

  (define pinwheel 
    (lambda(tri)
      (stack
       (side-by-side (quarter-turn-right tri) (half-turn tri))
       (side-by-side tri (quarter-turn-left tri)))))
      

;; **************************************************************
;; Textbook Exercise 1.11

  (define corner-bb 
    (filled-triangle 1 0 0 1 1 1))

  (define rcross-bb 
    (overlay(overlay
            (overlay (filled-triangle -.5 .5 -.5 -.5 .5 -.5)(filled-triangle 1 -1 1 .5 .5 -.5))
            (overlay (filled-triangle -1 1 1 1 -.5 .5) (filled-triangle -.5 .5 1 1 1 .5)))
            (overlay (filled-triangle .5 -.5 .5 .5 1 .5) (filled-triangle -.5 .5 1 1 1 .5))
    ))

  (define my-bb 
    (overlay (filled-triangle 0 1 0 0 -1 0)(filled-triangle 0 1 0 0 1 -1))
    )
)
