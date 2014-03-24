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

; List length (tail recursive)
(define (len2 l)
  (define (len2-helper in acc)
    (if (null? in)
        acc
        (len2-helper (cdr in) (+ 1 acc))))
  (len2-helper l 0))

; List length (named let)
(define (len3 l)
  (let len3-helper ((in l)
                    (acc 0))
    (if (null? in)
        acc
        (len3-helper (cdr in) (+ 1 acc)))))

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

; Map (named let)
(define (my-map2 f l)
  (let my-map2-helper ((fun f)
                       (in l))
    (if (null? in)
        '()
        (cons (fun (car in)) (my-map2-helper fun (cdr in))))))

; Takewhile (returns the longest prefix of the list for which the function
; given as a parameter returns true)
; e.g., (2 4 6 7 8 9), (lambda (x) (= (remainder x 2) 0)) -> (2 4 6)
(define (takewhile f l)
  (if (f (car l))
      (cons (car l) (takewhile f (cdr l)))
      '()))

; Pack (put together in sublists successions of equal elements)
; e.g., (1 1 1 2 3 3 4) -> ((1 1 1) (2) (3 3) (4))
(define (pack l)
  (define (pack-helper in acc sub)
    (cond ((null? in) (cons sub acc))
          ((null? sub) (pack-helper (cdr in) acc (cons (car in) sub)))
          ((eqv? (car in) (car sub)) (pack-helper (cdr in) acc (cons (car in) sub)))
          (else (pack-helper (cdr in) (cons sub acc) (list (car in))))))
  (reverse (pack-helper l '() '())))

; Encode (substitute successions of equal elements with the length of the successions)
; e.g., (1 1 1 2 3 3 4) -> ((3 1) (1 2) (2 3) (1 4))
(define (encode l)
  (define (pack-helper packed)
    (if (null? packed)
        '()
        (cons (list (len (car packed)) (car (car packed))) (pack-helper (cdr packed)))))
  (pack-helper (pack l)))
