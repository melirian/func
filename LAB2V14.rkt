#lang racket


(define (var14 n)
  (define (arg) (if (<= n 2) 0 (random 2 n)))
  (define (fun simple a n r)
    (if (= n 1)
      (= r a)
      (fun simple a (- n 1) (remainder (* r a) simple))
    )
  )
  (define arg1 (arg))
  (define arg2 (arg))
  (and (fun n arg1 n arg1) (fun n arg2 n arg2))
)

(var14 1)
(var14 2)
(var14 3)
(var14 4)
(var14 113)
(var14 117)