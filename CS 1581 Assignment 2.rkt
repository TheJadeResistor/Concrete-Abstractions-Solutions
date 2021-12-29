;3.13;;;;;;;;;;
;This is a recursive procedure and a recursive process.
;Recursive process means that the process is executed in steps, going into sub-problems and solving them,
;while iterative has the ability to reduce problems.
;Although this has a base case and base value, this is a recursive process as it is simply
;calling itself as the end with the initial value/2.


;3.14a;;;;;;;;;;
;This procedure calls (+ 1 (closest-power b (quotient n b))) at the end of it.
;This is therefore a recursive process as the main procedure is called
;and it does not utilize a seperate procedure or make this one more efficient.



;3.14b;;;;;;;;;;
;closet power with incursion

;original
;(define closest-power
;  (lambda (b n)
;    (if (< n b)
;        0
;        (+ 1 (closest-power b (quotient n b))))))


;______________________________________________
(define closest-power
  (lambda (b n)
    (let loop ([i (+ 1 (* 2 b))])
      (if (>= (expt b i) n)
          (- i 1)
          (+ i 9)))))
;______________________________________________


;3.15;;;;;;;;;;
;done in doc



;3.19;;;;;;;;;;
;done in doc



;4.1;;;;;;;;;;
;done in doc



;4.3;;;;;;;;;;
;(define mod-expt
;  (lambda (base exponent modulus)
;	(define mod*
;	  (lambda (x y)
;		(remainder (* x y)
;		modulus)))
;	(if (= exponent 0)
;		1
;		(if (even? exponent)
;			(let ((x (mod-expt base
;                            (/ exponent 2)
;			    modulus)))
;			  (mod* x x))
;			(mod* (mod-expt base
;				(- exponent 1)
;				modulus)
;		  base)))))
;
;(define mod-expt-mults
;  (lambda (exponent)
;	(define decrement-exponent-step
;	  (lambda (exponent result)
;		(if (= exponent 0)
;			result
;			(decrement-exponent-step
;			 (if (even? exponent)
;				 (/ exponent 2)
;				 (- exponent 1))
;			 (+ result 1)))))
;
;	(decrement-exponent-step exponent 0)))


;4.4;;;;;;;;;;
(define modulus 671629488048603400615365258174985654900765971941961654084193604750896012182890124354255484422321487634816640987992317596893099956961956383454333339584850276505584537663630293912940840460009374858969)

(define signature 143676221783307728140118556730532825709962359695147398872633033728948225540940112091576952965868445265161373616153020167902900930324840824269164789456142215776895016041636987254848119449940440885630)

(define signing-exponent 447752992032402267076910172116657103267177314627974436056129069833930674788593416236170322948214322483305175278012793102392215895931470577163544613600143471679799876664686423606429437389098641670667)


(define mod-expt;just like power, just applies the modulus
  (lambda (base exponent modulus)
	(define mod*
	  (lambda (x y)
		(remainder (* x y)
				   modulus)))

	(define helper
	  (lambda (b e result)
		(cond ((= e 0)
			   result)
			  ((even? e)
			   (helper (* b b)
						(/ e 2)
						result))
			  (else
			   (helper b
						(- e 1)
						(mod* b result))))))

	(helper base exponent 1)))

