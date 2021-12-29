
(module lab04 racket

  (provide my-curve
	   triangle
	   sierpinskis-gasket)

  (require (lib "fungraph.ss" "concabs"))

  (set-default-image-size! 300)

  ;; *****************************************************************
  ;; Exercise 4.8, p. 99

  (define my-curve
    (lambda (x0 y0 x1 y1 level)
      (if (= level 0)
          (line x0 y0 x1 y1)
          (let ((ycut (/ (+ x0 x1) 3))
                (xcut (/ (+ y0 y1) 3))
                (dx (- x1 x0))
                (dy (- y1 y0)))
            (let ((xa (* (- xcut (/ dy 2)) 1.5))
                  (ya (+ ycut (/ dx 2))))
              (overlay (my-curve x0 y0 xa ya (- level 1))
                       (my-curve xa ya x1 y1 (- level 1))))))))


  ; *****************************************************************
  ;; Exercise 4.9, p. 99

  (define triangle
    (lambda (x0 y0 x1 y1 x2 y2)
      (overlay
       (overlay (line x0 y0 x2 y2) (line x0 y0 x1 y1))
       (overlay (line x1 y1 x2 y2) (line x1 y1 x2 y2)))))

  ; *****************************************************************
  ;; Exercise 4.10, p. 99

  (define sierpinskis-gasket
    (lambda (x0 y0 x1 y1 x2 y2 level)
      (if (= level 0)
          (triangle x0 y0 x1 y1 x2 y2)
          (let ((x0new (/ (+ x2 x0) 2))
               (y0new (/ (+ y2 y0) 2))
               (x1new (/ (+ x1 x0) 2))
               (y1new (/ (+ y1 y0) 2))
               (x2new (/ (+ x1 x2) 2))
               (y2new (/ (+ y2 y1) 2)))
          (overlay
           (overlay (sierpinskis-gasket x0new y0new x1new y1new x2new y2new (- level 1)) (sierpinskis-gasket x0new y0new x1new y1new x2new y2new (- level 1)))
           (overlay (sierpinskis-gasket x0 y0 x1 y1 x2 y2 (- level 1)) (sierpinskis-gasket x0 y0 x1 y1 x2 y2 (- level 1))))))))

;  (define sierpinskis-gasket
;    (lambda (x0 y0 x1 y1 x2 y2 level)
;      (if (= level 0)
;          (triangle x0 y0 x1 y1 x2 y2)
;          (let ((x0new (/ (+ x2 x0) 2))
;               (y0new (/ (+ y2 y0) 2))
;               (x1new (/ (+ x1 x0) 2))
;               (y1new (/ (+ y1 y0) 2))
;               (x2new (/ (+ x1 x2) 2))
;               (y2new (/ (+ y2 y1) 2)))
;          (overlay
;           (overlay (sierpinskis-gasket x0new y0new x1 y1 x2 y2 (- level 1)) (sierpinskis-gasket x0 y0 x1new y1new x2 y2 (- level 1)))
;           (overlay (sierpinskis-gasket x0 y0 x1new y1new x2new y2new (- level 1)) (sierpinskis-gasket x0new y0new x1 y1 x2 y2 (- level 1))))))))

)
