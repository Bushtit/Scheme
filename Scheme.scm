#lang scheme

; Write a recursive Scheme program (add-rest lst) that returns the list with each element replaced with the sum of the element with the remaining elements of the list.  For example:
; (add-rest ‘(1 2 3)) would return (6, 5, 3)

(define add-rest
  (lambda (lst)
      (if (or (not(list? lst)) (null? lst))
         '()
         (cons (apply + lst) (add-rest(cdr lst))))))



; What does the following procedure do?  Explain how it works.
(define (sum-diff a b)
  (let ((+ -)
        (- +))
    (+ (- a a) (- b b))))
; This procedure assigns the + procedure to - instead, and the - procedure to + instead. It then adds a to itself, b to itself, and subtracts the two (a - b).



; Define a recursive Scheme procedure (cube-list1 lst) which cubes each item in list lst.
(define cube-list1
  (lambda (lst)
    (if(or (not(list? lst)) (null? lst))
       '()
       (cons (* (car lst) (car lst) (car lst)) (cube-list1(cdr lst))))))



; Define a Scheme procedure that uses a higher-order procedure (cube-list2 lst) which cubes each item in list lst
(define cube-list2
  (lambda (lst)
    (cons (map (* (car lst) (car lst) (car lst)) lst) '() )))



; Define a Scheme procedure (mult-position lst) which multiplies each item in list lst by its position in the list.  Assume the car of the list is position 1.
(define counter 0)

(define mult-position
  (lambda (lst counter)
    (cond
      ((null? lst) '() )
      (else (cons (* (car lst) counter) (mult-position(cdr lst) (+ counter 1)))))))



; Write a GPA calculator in Scheme.  The procedure should take a list of letter grades as its parameter and return the GPA on a 4.0 scale.  For example:  (gpa ‘(A B A B)) -> 3.5
; Your answer should be in decimal format.

(define Grades
  (lambda (x)
    (cond
      ((eq? x 'A) 4.0)
      ((eq? x 'B) 3.0)
      ((eq? x 'C) 2.0)
      ((eq? x 'D) 1.0))))


(define cum-points
  (lambda (lst)
    (if (null? lst)
        0
        (+ (Grades(car lst)) (cum-points(cdr lst))))))


(define GPA
  (lambda (lst)
    (/ (cum-points lst) (length lst))))



; Write a Scheme procedure (clean-list lst) that returns the list with all non-numeric elements removed.  You will use this for #2 & #3.
(define clean-list
  (lambda (lst)
    (cond
      ((null? lst) '() )
      ((number? (car lst)) (cons (car lst) (clean-list(cdr lst))))
      (else(clean-list(cdr lst))))))



; Write a recursive Scheme procedure (sum-squares lst) that returns the sum of the squares of all elements of lst.  Non-numeric data members should be ignored.
(define sum-squares
  (lambda (lst)
    (if(null? lst)
    0
    (+ (square-list(car lst)) (sum-squares(cdr lst))))))

(define square-list
  (lambda (x)
    (* x x)))



; Convert the following let structure to a lambda procedure that returns the same result
; (let
;    ((a 3)
;     (b 7))
;  (+ (* a a) b))

((lambda (a b)
  (+ (* a a) b)) 3 7)



; Convert the following scheme form to use a let structure instead of a lambda procedure
; ((lambda (a b c) (- (* b b) (* 4 a c))) 1 3 2)

(let
    ((a 1) (b 3)(c 2))
       (-(* b b) (* 4 a c)))
