;; 判断变量优先级
#lang racket
(provide (all-defined-out))

;; 优先级列表
(define priority (make-hash (list (cons 'x 0) (cons 'y 1) (cons 'z 2))))

;; 判断优先级
(define (higher-priority? x y)
  (let ((n1 (hash-ref priority x))
        (n2 (hash-ref priority y)))
    (> n1 n2)))
