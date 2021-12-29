
(module loop-lab racket

  (provide power
	   multiply
	   num-sixes
	   num-digits
	   stack-copies-of
	   quilt
	   divides?
	   sum-of-divisors-eff)

  (require "lab01.scm")
  (require (lib "fungraph.ss" "concabs"))

  ;; **************************************************************
  ;; Solution for Ex. 2.1, page 28

  (define power
    (lambda (base exponent)
      (if (= exponent 0)
             1
             (* base (power base (- exponent 1))))))


  ;; **************************************************************
  ;; Solution for Ex. 2.5, page 36

  (define multiply
    (lambda (m n)
      (if (= m 0)
             0
             (if (= n 0)
                 0
                 (if (< n 0)
                     (if (< m 0)
                        (- (multiply m (+ n 1)) m)
                     (+ (multiply n (- m 1)) n))
                 (+ m (multiply m (- n 1))))))))


  ;; **************************************************************
  ;; Solution for Ex. 2.9a, page 39

  (define num-sixes         ; number of 6's in decimal rep of n
    (lambda (n)
      (if (< n 0)
          (num-sixes (* n -1))
          (if (= n 1)
              0
              (if (= n 0)
                  0
                  (if (= (modulo n 10) 6)
                      (+ 1 (num-sixes  (floor(/ n 10))))
                      (if (not (= (modulo n 10) 6))
                          (num-sixes (floor(/ n 10)))
                          0)))))))


  ;; **************************************************************
  ;; Solution for Ex. 2.9b, page 39
  
  (define test 
    (lambda (n d)
      (if (< n 0)
          (num-digits (* n -1) d)
          (if (= n 1)
              0
              (if (= n 0)
                  0
                  (if (= (modulo n 10) d)
                      (+ 1 (num-digits (floor(/ n 10)) d))
                      (if (not (= (modulo n 10) d))
                          (num-digits (floor(/ n 10)) d)
                          0)))))))

   (define num-digits
      (lambda (n d)
        (cond
          ;[(and (= n 0) (= d 0)) 1]
          [(= (modulo n 10) d) (+ 1 (test (floor(/ n 10)) d))]
          [(not (= (modulo n 10) d)) (test (floor(/ n 10)) d)]
          [1])))


  ;; **************************************************************
  ;; Solution for Ex. 2.13, page 40
 
  (define stack-copies-of       ; stack n copies of image on top
    (lambda (n image)           ; one another
      (if (not(= n 1))
         (stack
          (half-turn (half-turn image))
          (stack-copies-of (- n 1) image))
         (half-turn (half-turn image)))))


  ;; **************************************************************
  ;; Solution for Ex. 2.14, page 40
  
  (define quilt
  (lambda (image w h)
    (let ((column (stack-copies-of h image)))
          (if (= w 1)
              column
              (side-by-side column
                            (quilt image (- w 1) h))))))

  ;; **************************************************************
  ;; Exercise 3.6, p. 60

  (define divides?
    (lambda (a b)
      (= (remainder b a) 0)))

  (define square
    (lambda (n) (* n n)))

(define sum-of-divisors-eff
  (lambda (n)
    (define sum-from-plus ; sum of all divisors of n which are
      (lambda (low addend) ; >= low, plus addend
        (if (> low (sqrt n))
            addend  ;no divisors of n are greater than n
            (if (= low (sqrt n))
                (+ addend low)
            (sum-from-plus (+ low 1) 
                           (if (divides? low n)
                               (+ addend (+ low (/ n low)))
                               addend))))))
    (sum-from-plus 1 0)))
  
)
