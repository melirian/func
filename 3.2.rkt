#lang racket
;Лабораторна робота 3
;ФІТ ПІ-41 Музиченко
;Вариант 14. Обчислити значення функції у, розвинувши функцію у ряд Тейлора.
;Аргумент х змінюється від -2 до 2 з кроком 0.5. Визначити похибку.
;y = sqrt(x-1)+sqrt(x+1), -2<=x<=0
;y = 1/sqrt(x^2-1),        0<x<=2
(define ka 17) ;колическтво членов ряда тейлора
(define dx (expt 10 (- 0 ka))) ;dx=10^-ka
 ;степень
(define (deriv g n x) ;нахождение n-й производной
  (cond
    ((= n 0) (g x))
    ((= n 1) (/  (- (g (- x dx)) (g x)) dx))
    (else (/ (- (deriv g (- n 1) (- x dx) ) (deriv g (- n 1) x)) dx))  
  ))
(define (funct x) ;f(x)
  (cond
    ((= x 1) (/ 1.0 0.0)) 
    ((and (<= -2 (+ x (* dx ka))) (<= x 0)) (+ (expt (- x 1) 1/2) (expt (+ x 1) 1/2)))
    ((and (< 0 x) (<= x 2)) (/ 1 (expt (- (* x x) 1) 1/2)))
    (else (error "Error value"))
    ))
(define (taylor f k a n fact)
  (if (< n k)
    (+ (/ (* (deriv f n a) (expt dx n)) fact) (taylor f k a (+ n 1) (* fact (+ n 1))))
    0
  )
)
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (lab3v14 funct accuracy left right)
  (if (>= left right)
    (list (taylor funct accuracy left 0 1))
    (append
      (list (taylor funct accuracy left 0 1))
      (lab3v14 funct accuracy (+ left 1/2) right))
  )
)
(list (lab3v14 funct 16 -2 2) 'Похибка=10^-16)