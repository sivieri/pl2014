#lang racket

; Alessandro Sivieri <alessandro.sivieri@polimi.it> - 02/04/2014

; Argument-passing
(define (greet name)
  (string-append "Hello, " name))

; The previous definition is syntactic sugar for the following
(define lgreet (lambda (name) (string-append "Hello, " name)))

(define (optgreet name #:hi [hi "Hello"])
  (string-append hi ", " name))

(define (restgreet #:hi [hi "Hello"] . others)
  (if (null? others)
      ""
      (string-append hi ", " (car others) (apply restgreet #:hi "" (cdr others)))))

; Matrix definition and initialization (using vectors)
; Note: you cannot use something like
;
; (make-vector rows (make-vector cols fill))
;
; because the interpreter will evaluate the inner call
; and then use the same result in all the rows. This means
; that each cell in the main vector would have a reference
; to the same vector. You need to create different vectors
; for each row.
(define (make-matrix rows cols fill)
  (let ((vec (make-vector rows)))
    (let loop ((x 0))
      (if (< x rows)
          (begin
            (vector-set! vec x (make-vector cols fill))
            (loop (+ x 1)))
          vec))))

; Count elements in a nested list (first version)
(define (count-elements l)
  (if (null? l)
      0
      (if (list? (car l))
          (+ (count-elements (car l)) (count-elements (cdr l)))
          (+ 1 (count-elements (cdr l))))))

; Count elements in a nested list (using cond)
(define (count-elements-2 l)
  (cond ((null? l) 0)
        ((list? (car l)) (+ (count-elements (car l)) (count-elements (cdr l))))
        (#t (+ 1 (count-elements (cdr l))))))

; Count elements in a nested list (tail recursion)
(define (count-elements-3 l)
  (let helper ((lst l)
               (acc 0))
    (cond ((null? lst) acc)
          ((list? (car lst)) (helper (cdr lst) (helper (car lst) acc)))
          (#t (helper (cdr lst) (+ 1 acc))))))

; Count elements in a nested list (mutability and other stuff,
; kinda messy but shows mutability, for-each...)
(define (count-elements-4 l)
  (define stack0 (list l))
  (let loop ([stack (cdr stack0)]
             [res 1]
             [curr (car stack0)])
    (when (list? curr)
      (for-each (lambda (x) 
                  (set! stack (cons x stack)))
                (cdr curr)))
    (if (null? stack)
        res
        (loop (cdr stack)
              (+ 1 res)
              (car stack)))))

; K combinations from a list of length N
; (a b c d e) -> ((a b c) (a b d) (a b e) ...)
(define (combination n l)
  (cond ((= 0 n) '(()))
        ((null? l) '())
        (#t (append (let ((item (car l)))
              (map (lambda (x) (cons item x)) (combination (- n 1) (cdr l))))
                    (combination n (cdr l))))))

; Assert function
(define (assert c)
  (unless c
    (error "Assertion failed")))

; Binary trees
; As said in class: this may not be the best implementation of
; binary trees, but it is an example on how to work with structs.
; Note that there is no mutability.
(struct leaf
  (content empty))

(struct node leaf
  (left right))

(define (create-leaf v)
  (node v #f (create-empty) (create-empty)))

(define (create-node v t1 t2)
  (node v #f t1 t2))

(define (create-empty)
  (leaf -1 #t))

; Display a tree
(define (display-tree t)
  (assert (leaf? t))
  (if (leaf-empty t)
      (display "-")
      (begin
        (display "Node: ")
        (display (leaf-content t))
        (newline)
        (display "Left: ")
        (display-tree (node-left t))
        (newline)
        (display "Right: ")
        (display-tree (node-right t))
        (newline))))

; Sum all content of a tree
(define (tree-sum t)
  (assert (leaf? t))
  (if (leaf-empty t)
      0
      (+ (leaf-content t) (+ (tree-sum (node-left t)) (tree-sum (node-right t))))))

; Collect all leaves values in a list
(define (tree-leaves t)
  (let helper ((tree t)
               (acc '()))
    (cond ((leaf-empty tree) acc)
          ((and (leaf-empty (node-left tree)) (leaf-empty (node-right tree))) (cons (leaf-content tree) acc))
          (#t (append (helper (node-left tree) acc) (helper (node-right tree) acc))))))

; Macro: while loop
(define-syntax while
  (syntax-rules ()
    ((_ condition body ...)
     (let loop ()
       (when condition
         (begin
           body ...
           (loop)))))))

; Macro: rotate a list
(define-syntax-rule (swap x y)
  (let ((tmp x))
    (set! x y)
    (set! y tmp)))

(define-syntax rotate
  (syntax-rules ()
    ((rotate a) (void))
    ((rotate a b c ...) (begin
                          (swap a b)
                          (rotate b c ...)))))

; Macro to compute the max between two values
(define-syntax my-max
  (syntax-rules ()
    (( _ a b)
     (if (> a b) a b))))

; Macro to compute the max between two values where
; the input expressions are evaluated only once
(define-syntax my-max-2
  (syntax-rules ()
    (( _ a b) 
     (let ((new-a a)
           (new-b b))
       (if (> new-a new-b) new-a new-b)))))
