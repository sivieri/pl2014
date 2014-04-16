#lang racket

; for-expression a la Python
; (for x in '(1 2 3) (display x) (newline))
(define-syntax for
  (syntax-rules (in)
    ((_ var in lst fun ...)
     (for-each (lambda (var)
                 fun ...) lst))))

; Cartesian product
; (1 2) (a b) -> (1 a) (1 b) (2 a) (2 b)
(define (listify x)
  (if (list? x)
      x
      (list x)))

(define (cartesian-product x y)
  (let ((val '()))
    (for-each ; first list
     (lambda (a)
       (for-each ; second list
        (lambda (b)
          (set! val
                (append val
                        (list (append
                               (listify a)
                               (listify b))))))
        y))
     x)
    val))

; Let's generalize the previous example in a macro
; (list/co (* x x) from x '(1 2 3)) -> '(1 4 9)
; (list/co (* x y) from x '(1 2 3) y '(2 4 6)) -> '(2 4 6 4 8 12 6 12 18)
; (list/co (* x y) when (even? x) from x '(1 2 3) y '(2 4 6)) -> '(4 8 12)
; (list/co (append (listify x) (listify y)) from x '(1 2) y '(a b)) -> (1 a) (1 b) (2 a) (2 b)
(define (concat-map lst f)
  (apply append (map f lst)))

(define-syntax list/co
  (syntax-rules (when from)
    
    ((_ expr from v1 l1)
     (concat-map l1
                 (lambda (v1)
                   (list expr))))
    
    ((_ expr from v1 l1 v2 l2 ...)
     (concat-map l1 
                 (lambda (v1)
                   (list/co expr from v2 l2 ...))))
    
    ((_ expr when condition from v1 l1)
     (concat-map l1
                 (lambda (v1)
                   (if condition
                       (list expr)
                       '()))))
    
    ((_ expr when condition from v1 l1 v2 l2 ...)
     (concat-map l1
                 (lambda (v1)
                   (list/co expr when condition 
                            from v2 l2 ...))))))

; if without else
(define-syntax my-if
  (syntax-rules ()
    
    ((_ condition expr)
     (cond (condition expr)))
    
    ((_ condition expr else-expr)
     (if condition
         expr
         else-expr))))

; Hygienic macros
(define-syntax my-or
  (syntax-rules ()
    ((_ e1 e2)
     (let ((result e1))
       (if result
           result
           e2)))))
; (let ((result true))
;      (my-or2 false
;              result))
; which (in theory) becomes
; (let ((result true))
;      (let ((result false))
;           (if result
;               result
;               result)))
; it actually becomes
; (let ((result true))
;      (let ((g1729 false))
;           (if g1729
;               g1729
;               result)))
; or even better (let is a macro)
; (let ((b4189 true))
;      (let ((g1729 false))
;           (if g1729
;               g1729
;               b4189)))

; Same C example
(define-syntax inc
  (syntax-rules ()
    ((_ i)
     (let ((a 0))
       (set! i (+ i 1))))))

; Coroutines a la Lua
(define *queue* '())

(define (empty-queue?)
  (null? *queue*))

(define (enqueue x)
  (set! *queue* (append *queue* (list x))))

(define (dequeue)
  (let ((x (car *queue*)))
    (set! *queue* (cdr *queue*))
    x))

(define (fork proc)
  (call/cc
   (lambda (k)
     (enqueue k)
     (proc))))

(define (yield)
  (call/cc
   (lambda (k)
     (enqueue k)
     ((dequeue)))))

(define (thread-exit)
  (if (empty-queue?)
      (exit)
      ((dequeue))))

(define (do-stuff-n-print str max)
  (lambda ()
    (let loop ([n 0])
      (display str)(display " ")(display n)(newline)
      (yield)
      (if 
       (< n max)
       (loop (+ 1 n))
       (thread-exit)))))

(define (coroutines-test)
  (begin
    (fork (do-stuff-n-print "This is A" 4))
    (fork (do-stuff-n-print "This is B" 5))
    (thread-exit)))

; "break" in a loop
(define (break-test)
  (call/cc (lambda (break)
             (for-each
              (lambda (i)
                (if (= (remainder i 2) 0)
                    (break)
                    (begin
                      (display i)
                      (newline))))
              '(1 2 3 4 5 6)))))

; "continue" in a loop
(define (continue-test)
  (for-each
   (lambda (i)
     (call/cc (lambda (continue)
                (if (= (remainder i 2) 0)
                    (continue)
                    (begin
                      (display i)
                      (newline))))))
   '(1 2 3 4 5 6)))
