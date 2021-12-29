;; <Sage Magras>
;; Lab Exercise 9
;; CS 1581 Honors Computer Science I
;; <Fall, 2021>

(module tree-lab racket

  (provide minimum
	   number-of-nodes
	   inorder
	   postorder
	   insert
	   list->bstree)

  (require "bstree.scm") 

  ;; **************************************************************
  ;; Exercise 8.1, p. 217

(define minimum
  (lambda (tree)
    (if (empty-tree? tree)
        #f
        (if (empty-tree? (left-subtree tree))
            (root tree)
            (minimum (left-subtree tree))))))


  ;; **************************************************************
  ;; Exercise 8.2, p. 217

 (define number-of-nodes
  (lambda (tree)
    (if (empty-tree? tree)
        0
        (+ 1 (number-of-nodes (left-subtree tree)) (number-of-nodes (right-subtree tree))))))

  ;; **************************************************************
  ;; Exercise 8.4, p. 220

(define inorder
  (lambda (tree)
	(define inorder-onto
	  (lambda (tree list)
		(if (empty-tree? tree)
			list
			(inorder-onto (left-subtree tree)
						  (cons (root tree)
								(inorder-onto (right-subtree tree)
											  list))))))

	(inorder-onto tree '())))


  ;; **************************************************************
  ;; Exercise 8.5, p. 220

(define postorder
  (lambda (tree)
    (if (empty-tree? tree)
        '()
        (append (postorder (left-subtree tree)) (postorder (right-subtree tree)) (list (root tree))))))


  ;; **************************************************************
  ;; Exercise 8.6, p. 220

(define insert
  (lambda (n tree)
	(cond ((empty-tree? tree) (make-nonempty-tree n (make-empty-tree) (make-empty-tree)))
              ((< n (root tree)) (make-nonempty-tree (root tree) (insert n (left-subtree tree)) (right-subtree tree)))
              (else (make-nonempty-tree (root tree) (left-subtree tree) (insert n (right-subtree tree)))))))

  ;; **************************************************************
  ;; Exercise 8.7, p. 220

(define list->bstree ;!!!!--8.7 only creates a tree in order, cannot figure out how to make it sort and equal the lists!!!
  (lambda (list-of-numbers)
	(define insert-onto
	  (lambda (lst tree)
		(if (null? lst)
			tree
			(insert-onto (cdr lst) (insert (car lst) tree)))))

	(insert-onto list-of-numbers (make-empty-tree))))
  
)