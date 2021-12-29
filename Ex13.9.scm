
(module Ex13.9 racket

  (provide make-ra-stack
           height
	   empty-ra-stack?
	   top-minus
	   pop!
	   push!)

  ;; ********************************************************
  ;; Advertised procedures -- these make up the ADT interface

  (define make-ra-stack
    (lambda ()
      (make-ra-stack-with-at-most 8))) 

  (define height
    (lambda (ra-stack)
      (vector-ref ra-stack 0)))

  (define empty-ra-stack?
    (lambda (ra-stack)
      (= 0 (height ra-stack))))

  (define top-minus
    (lambda (ra-stack offset)
      (cond ((< offset 0)
	     (error "TOP-MINUS: offset < 0" offset))
	    ((>= offset (height ra-stack))
	     (error "TOP-MINUS: offset too large for stack"
		    offset (height ra-stack)))
	    (else
	     (vector-ref (cells ra-stack)
			 (- (height ra-stack)
			    (+ offset 1)))))))

  (define pop!
    (lambda (ra-stack)
      (if (empty-ra-stack? ra-stack)
	  (error "POP!: attempted pop from an empty stack")
	  (begin
	    (set-height! ra-stack
			 (- (height ra-stack) 1))
	    ra-stack))))

  ;; ********************************************************************
  ;; Exercise 13.9, p. 441

  (define new-vect ;helper
    (lambda (stack)
      (make-vector (* 2 (vector-length (cells stack))))))

  (define enlarge-stack!
    (lambda (stack)
      (define loop
        (lambda (loc)
          (cond ((= loc (vector-length stack)))
                (else   (let ((new-cells (new-vect stack)))                               ;; Creates empty cells twice the original length.
                          (vector-copy! new-cells 0 (cells stack) 0 (height stack))       ;; Copies the contents of the old cells to the new cells.
                          (set-cells! stack new-cells)) 
                        (loop (+ loc 1))))))
      (loop 0)))
  
  (define push!
    (lambda (ra-stack item)
      (if (<= (vector-length (cells ra-stack))
              (height ra-stack))
          
          (begin
            (enlarge-stack! ra-stack)
            (push! ra-stack item)
            ra-stack)
          (begin
            (vector-set! (cells ra-stack) (height ra-stack) item)
            (set-height! ra-stack (+ (height ra-stack) 1)) ra-stack))))

  
  ;; ********************************************************
  ;; Unadvertised procedures -- these are for implementors only

  (define make-ra-stack-with-at-most
    (lambda (max-height)
      (let ((header (make-vector 2))
	    (cells (make-vector max-height)))
	(vector-set! header 0 0)     ; header[0] = height = 0
	(vector-set! header 1 cells) ; header[1] = cells
	header)))

  (define cells  ; use only within the ADT implementation
    (lambda (ra-stack)
      (vector-ref ra-stack 1)))

  (define set-height!  ; use only within the ADT implementation
    (lambda (ra-stack new-height)
      (vector-set! ra-stack 0 new-height)))

  (define set-cells!  ; use only within the ADT implementation
    (lambda (ra-stack new-cells)
      (vector-set! ra-stack 1 new-cells)))

)
