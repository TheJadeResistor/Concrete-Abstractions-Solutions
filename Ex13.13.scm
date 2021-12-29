
(module Ex13.13 racket

  (provide make-queue
           empty-queue?
	   head
	   enqueue!
	   dequeue!)

  ;; ********************************************************************
  ;; Exercise 13.13, p. 453

  (define make-queue
    (lambda ()
      (let ((cells (make-vector 8))
            (header (make-vector 2)))
        (set-start! header '())
        (set-tail! header '())
        header)))
  
  (define empty-queue?
    (lambda (queue)
      (or (null? (start queue)) (null? (tail queue))) ))

  (define head
    (lambda (queue)
      (if (empty-queue? queue)
	  (error "attempt to take head of an empty queue")
          (mcar (start queue)))
      ))

     (define make-node mcons)
     (define node-element mcar)
     (define node-rest mcdr)
     (define node-set-element! set-mcar!)
     (define node-set-rest! set-mcdr!)
  

  (define enqueue!    ;Thank you for the help!!                        
    (lambda (queue new-item)
      (let ((new-tail (mcons new-item null))) ;Let NewTail = Mcons (NewItem, Null Pointer)    ;; The tail/mcdr of the tail node is always null.
        (if (empty-queue? queue) ;IF (Queue is empty)                          ;; If filling an empty list, then set both the head and tail to the new item.
            (begin
              (set-start! queue new-tail) ;Queue Start = NewTail
              (set-tail! queue new-tail)) ;Queue Tail  = NewTail
            (begin
              (set-mcdr! (tail queue) new-tail)
              (set-tail! queue new-tail)
              )))))
  
  (define dequeue!
    (lambda (queue)
      (if (empty-queue? queue)
	  (error "attempt to take head of an empty queue")
          (begin
            (set-start! queue (mcdr (start queue))))
          )))

  
  ;; Any supporting non-ADT operations here:
  
  (define start      ;accessor for start of queue
    (lambda (queue)
      (vector-ref queue 0)))
    
  (define set-start! ;mutator for start of queue
    (lambda (queue node)
      (vector-set! queue 0 node)))

  (define tail       ;accessor for tail of queue
    (lambda (queue)
      (vector-ref queue 1)))
    
  (define set-tail!  ;mutator for tail of queue
    (lambda (queue node)
      (vector-set! queue 1 node)))

)
