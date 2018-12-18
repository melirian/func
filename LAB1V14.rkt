#lang racket
(define (var14 a b c d)
 +
    (/
      (expt a 2)
      (abs (- 1 b))
    )  
    (/
      (expt b 2)
      (abs (- 1 d))
    )
    (/ 
      (expt c 2)
      (log a)
    )
 )
