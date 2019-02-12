;; Exercise 2.87~2.92
;; 测试多项式包
#lang racket
(require "install-polynomial-package.rkt")
(require "install-scheme-number-package.rkt")
(require "apply-generic.rkt")

(define n1 (make-scheme-number 1))
(define n2 (make-scheme-number 2))
(define n3 (make-scheme-number 3))
(define n4 (make-scheme-number 4))
  
(define p1 (make-polynomial-from-dense 'x (list n1 n3 n4)))
(define p2 (make-polynomial-from-dense 'x (list n1 n3 n2)))
(define p3 (make-polynomial-from-parse 'x (list
                                           (list 100 n3)
                                           (list 3 n4)
                                           (list 1 n1))))
(define p4 (make-polynomial-from-parse 'x (list
                                           (list 5 n3)
                                           (list 3 n1)
                                           (list 1 n1))))

(display "测试稠密表示法\n")
(apply-generic 'add p1 p2)
(apply-generic 'sub p1 p2)
(apply-generic 'mul p1 p2)

(display "测试稀疏表示法\n")
(apply-generic 'add p3 p4)
(apply-generic 'sub p3 p4)
(apply-generic 'mul p3 p4)

(display "测试混合类型计算\n")
(define p5 (make-polynomial-from-dense 'x (list n2 n3 n4)))
(define p6 (make-polynomial-from-parse 'x (list
                                           (list 4 n3)
                                           (list 2 n4)
                                           (list 1 n1))))
(apply-generic 'add p5 p6)
(apply-generic 'sub p5 p6)
(apply-generic 'mul p5 p6)

(display "测试除法\n")
(define n-1 (make-scheme-number -1))
(define p7 (make-polynomial-from-parse 'x (list
                                           (list 5 n1)
                                           (list 0 n-1))))
(define p8 (make-polynomial-from-parse 'x (list
                                           (list 2 n1)
                                           (list 0 n-1))))
(apply-generic 'div p7 p8)

(display "测试多变量\n")
(define p9 (make-polynomial-from-parse 'x (list
                                           (list 5 n1)
                                           (list 0 n-1))))
(define p10 (make-polynomial-from-parse 'y (list
                                            (list 2 n1)
                                            (list 0 n-1))))
(apply-generic 'add p9 p10)
(apply-generic 'mul p9 p10)


