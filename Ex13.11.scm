;; <Sage Magras>
;; Lab Exercise 10 -- Text Exercise 13.11
;; CS 1581 Honors Computer Science I
;; Fall 2021

(module Ex13.11 racket

  (provide make-queue
           empty-queue?
	   head
	   enqueue!
	   dequeue!
	   queue-cells ; provided only for testing purposes
	   )

  ;; ********************************************************
  ;; Advertised procedures -- these make up the ADT interface

  (define make-queue
    (lambda ()
      (let ((cells (make-vector 8)) ; 8 is arbitrary
	    (header (make-vector 3)))
	(set-queue-length! header 0)
	(set-queue-start! header 0) ; arbitrary start
	(set-queue-cells! header cells)
	header)))

  (define empty-queue?
    (lambda (queue)
      (= (queue-length queue) 0)))

  (define head
    (lambda (queue)
      (if (empty-queue? queue)
	  (error "attempt to take head of an empty queue")
	  (vector-ref (queue-cells queue)
		      (queue-start queue)))))

  (define enqueue!
    (lambda (queue new-item)
      (let ((length (queue-length queue))
	    (start (queue-start queue))
	    (cells (queue-cells queue)))
	(if (= length (vector-length cells))
	    (begin
	      (enlarge-queue! queue)
	      (enqueue! queue new-item))
	    (begin
	      (vector-set! cells
			   (remainder (+ start length)
				      (vector-length cells))
			   new-item)
	      (set-queue-length! queue (+ length 1))
	      queue)))))


  ;; ********************************************************************          
  ;; Exercise 13.11, p. 451

  (define dequeue!
    (lambda (queue)
      (let ((length (queue-length queue))
            (start (queue-start queue))
            (cells (queue-cells queue)))
        (if (empty-queue? queue)
            (error "empty queue")
            (if (= length (vector-length cells))
                (begin
                  (enlarge-queue! queue)
                  (dequeue! queue))
                (begin
                  (set-queue-start! queue (modulo (+ start 1) (vector-length cells)))
                  (set-queue-length! queue (- length 1))
                  queue))
            )
        )))

  ;; ********************************************************
  ;; Unadvertised procedures -- these are for implementors only

  (define from-to-do  
    (lambda (start stop body)
      (if (> start stop)
	  (values)
	  (begin (body start)
		 (from-to-do (+ 1 start) stop body)))))

  (define enlarge-queue! ;use only within the ADT implementation
    (lambda (queue)
      (let ((length (queue-length queue))
	    (start (queue-start queue))
	    (cells (queue-cells queue)))
	(let ((cells-length (vector-length cells)))
	  (let ((new-cells (make-vector (* 2 cells-length))))
	    (from-to-do
	     0 (- length 1)
	     (lambda (i)
	       (vector-set! new-cells i
			    (vector-ref cells
					(remainder (+ start i)
						   cells-length)))))
	    (set-queue-start! queue 0)
	    (set-queue-cells! queue new-cells)
	    queue)))))

  (define queue-length ; use only within the ADT implementation
    (lambda (queue)
      (vector-ref queue 0)))

  (define set-queue-length! ; use only within the ADT implementation
    (lambda (queue new-length)
      (vector-set! queue 0 new-length)))

  (define queue-start ;use only within the ADT implementation
    (lambda (queue)
      (vector-ref queue 1)))

  (define set-queue-start! ; use only within the ADT implementation
    (lambda (queue new-start)
      (vector-set! queue 1 new-start)))

  (define queue-cells ; use only within the ADT implementation
    (lambda (queue)
      (vector-ref queue 2)))

  (define set-queue-cells! ; use only within the ADT implementation
    (lambda (queue new-cells)
      (vector-set! queue 2 new-cells)))

)