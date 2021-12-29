
(module lab06 racket
  
  (provide make-interval
           lower-endpoint
           upper-endpoint
           midpoint
           right-half
           make-3D-vector
           x-coord
           y-coord
           z-coord
           3D-vector-equals
           3D-vector-add
           3D-vector-dot-product
           3D-vector-scale)
  
  ;; *******************************************************************
  ;; Preliminary Exercise for 6.22: A simple ADT for an interval
  
  (define make-interval
    (lambda (lower upper)
      (cons lower upper)
    ))
  
  (define lower-endpoint
    (lambda (interval)
      (car interval)))
  
  (define upper-endpoint
    (lambda (interval)
      (cdr interval)))
  
  ;; **************************************************************
  ;; Exercise 6.22, p. 161
  
  (define midpoint
    (lambda (interval)
      (/ (+ (lower-endpoint interval) (upper-endpoint interval)) 2)))
  
  (define right-half
    (lambda (interval)
      (cons (midpoint interval) (upper-endpoint interval))))
  
  ;; *******************************************************************
  ;; Preliminary Exercise for 6.23: A simple ADT for a three-dimensional
  ;; (3D) vector
  
  (define make-3D-vector   ; 3D vector constructor
    (lambda (x y z)
      (cons x (cons y z))))
  
  (define x-coord          ; 3D vector accessors
    (lambda (v)
      (car v)))

  (define y-coord
    (lambda (v)
      (car (cdr v))))

  (define z-coord
    (lambda (v)
      (cdr (cdr v))))
  
  (define 3D-vector-equals ; Testing 3D vector equality
    (lambda (v1 v2)
      (if (and (= (x-coord v1) (x-coord v2)) (= (y-coord v1) (y-coord v2)) (= (z-coord v1) (z-coord v2)))
          #t
          #f)))
  
  ;; **************************************************************
  ;; Exercise 6.23, p. 162
  
  (define 3D-vector-add
    (lambda (v1 v2)
      (make-3D-vector (+ (x-coord v1) (x-coord v2)) (+ (y-coord v1) (y-coord v2)) (+ (z-coord v1) (z-coord v2)))
      ))
  
  (define 3D-vector-dot-product
    (lambda (v1 v2)
      (+ (* (x-coord v1) (x-coord v2)) (* (y-coord v1) (y-coord v2)) (* (z-coord v1) (z-coord v2)))
      ))
  
  (define 3D-vector-scale
    (lambda (v factor)
      (make-3D-vector (*(x-coord v) factor) (*(y-coord v) factor) (*(z-coord v) factor))
       ))
)
