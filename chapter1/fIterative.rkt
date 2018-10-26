#lang racket
(define (fIter f1 f2 f3 n)
  (cond ((< n 3) n)
        ((= n 3) f1)
        (else (fIter (+ f1 (* 2 f2) (* 3 f3)) f1 f2 (- n 1)))))

(define (f n)
  (fIter 4 2 1 n))

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
(f 6)
  
