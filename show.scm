(module show racket

  (provide show)

  (require (lib "graphics.ss" "graphics"))
  (require "graphics.scm")

  (define show
    (lambda (image)
      (define point->posn ; convert to DrScheme's position (posn) data type,
	(lambda (point)   ;  which has y-axis reversed from our convention
	  (make-posn (x-coord point)
		     (- (height image) (y-coord point)))))
      (open-graphics)
      (let ((pixmap (open-pixmap "image" 
				 (inexact->exact (round (width image)))
				 (inexact->exact (round (height image))))))
	(draw-on image
		 (lambda (op)  ; this is the drawing medium
		   (cond ((equal? op 'draw-line)
			  (lambda (point0 point1)
			    ((draw-line pixmap) 
			     (point->posn point0)
			     (point->posn point1))))
			 ((equal? op 'draw-filled-triangle)
			  (lambda (point0 point1 point2)
			    ((draw-solid-polygon pixmap)
			     (map point->posn (list point0 point1 point2))
			     (make-posn 0 0))))
			 (else
			  (error "Unknown operation on a DrScheme drawing medium"
				 op)))))
	;; we put a frame around the image, just to make clear its extent
	((draw-rectangle pixmap) (make-posn 0 0) (width image) (height image))
	(viewport->snip pixmap))))

)
