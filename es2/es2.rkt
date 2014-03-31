#lang racket

; Alessandro Sivieri <alessandro.sivieri@polimi.it> - 02/04/2014

; Argument-passing
(define (greet name)
  (string-append "Hello, " name))

(define lgreet (lambda (name) (string-append "Hello, " name)))

(define (optgreet name #:hi [hi "Hello"])
  (string-append hi ", " name))

(define (restgreet #:hi [hi "Hello"] . others)
  (if (null? others)
      ""
      (string-append hi ", " (car others) (apply restgreet #:hi "" (cdr others)))))

; Matrix definition and initialization (using vectors)
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

; Count elements in a nested list (mutability and other stuff)
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
(struct leaf
  ((content #:mutable)
   (empty #:mutable)))

(struct node leaf
  ((left #:mutable)
   (right #:mutable)))

(define (create-leaf v)
  (leaf v #f))

(define (create-node v t1 t2)
  (node v #f t1 t2))

(define (create-empty)
  (leaf -1 #t))

; Display a tree
(define (display-tree t)
  (assert (or (leaf? t) (node? t)))
  (if (leaf-empty t)
      (display "-")
      (begin
        (display "Node: ")
        (display (leaf-content t))
        (newline)
        (if (node? t)
            (begin
              (display "Left: ")
              (display-tree (node-left t))
              (newline)
              (display "Right: ")
              (display-tree (node-right t))
              (newline))
            (display "")))))

; Sum all content of a tree
(define (tree-sum t)
  (assert (leaf? t))
  (cond ((leaf-empty t) 0)
        ((node? t) (+ (leaf-content t) (+ (tree-sum (node-left t)) (tree-sum (node-right t)))))
        (#t (leaf-content t))))
