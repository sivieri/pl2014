#lang racket

; Alessandro Sivieri <alessandro.sivieri@polimi.it> - 21/03/2014

; Factorial
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

; List length
(define (len l)
  (if (null? l)
      0
      (+ 1 (len (cdr l)))))

; Range function
(define (range lo hi)
  (if (> lo hi)
      (error "lo > hi")
      (if (< lo hi)
          (cons lo (range (+ 1 lo) hi))
          (list lo))))

; Range step
(define (rangestep lo hi step)
  (if (< lo hi)
      (cons lo (rangestep (+ step lo) hi step))
      '()))

; Compress
(define (compress head . tail)
  (if (null? tail)
      head
      (if (eqv? head (car tail))
          (compress tail)
          (cons head (compress tail)))))

; Compress (tail recursive)
(define (compresstr head . tail)
  (define (compress-helper in acc last)
    (if (null? in)
        acc
        (if (eqv? (car in) last)
            (compress-helper (cdr in) acc last)
            (compress-helper (cdr in) (cons (car in) acc) (car in)))))
  (reverse (compress-helper tail (list head) head)))

; Fibonacci
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

; Flatten (tail recursive)
(define (flatten l)
  (define (flatten-helper in acc)
    (if (null? in)
        acc
        (if (list? (car in))
            (flatten-helper (cdr in) (flatten-helper (car in) acc))
            (flatten-helper (cdr in) (cons (car in) acc)))))
  (reverse (flatten-helper l '())))

; Map
(define (my-map f l)
  (if (null? l)
      '()
      (cons (f (car l)) (my-map f (cdr l)))))

; Exercises:
; 1. Rewrite the map implementation using a named let
; 2. Write the function "takewhile", which takes 2 elements (a function and
;    a list) and returns the longest prefix of the list for which the
;    function returns "true"
; 3. Write a function that takes successive equal elements from a list and 
;    puts them into a sublist
;    e.g., (1 1 1 2 3 3 4) -> ((1 1 1) (2) (3 3) (4))
; 4. Use the previous function to encode a list: each sequence of equal elements
;    is converted into a list of two elements, the symbol and the number of instances
;    e.g., (1 1 1 2 3 3 4) -> ((3 1) (1 2) (2 3) (1 4))

; Ex. 1
(define (my-map2 f l)
  (let my-map2-helper ((fun f)
                       (in l))
    (if (null? in)
        '()
        (cons (fun (car in)) (my-map2-helper fun (cdr in))))))

; Ex. 2
(define (takewhile f l)
  (if (f (car l))
      (cons (car l) (takewhile f (cdr l)))
      '()))

; Ex. 3
(define (pack l)
  (define (pack-helper in acc sub)
    (cond ((null? in) (cons sub acc))
          ((null? sub) (pack-helper (cdr in) acc (cons (car in) sub)))
          ((eqv? (car in) (car sub)) (pack-helper (cdr in) acc (cons (car in) sub)))
          (else (pack-helper (cdr in) (cons sub acc) (list (car in))))))
  (reverse (pack-helper l '() '())))

; Ex. 4
(define (encode l)
  (define (pack-helper packed)
    (if (null? packed)
        '()
        (cons (list (len (car packed)) (car (car packed))) (pack-helper (cdr packed)))))
  (pack-helper (pack l)))
