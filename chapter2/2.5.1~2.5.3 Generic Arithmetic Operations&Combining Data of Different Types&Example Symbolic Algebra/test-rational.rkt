;; Exercise 2.93~2.97
;; 多项式有理数测试
#lang racket
(require "install-polynomial-parse-package.rkt")
(require "install-rational-package.rkt")
(require "install-scheme-number-package.rkt")
(require "apply-generic.rkt")
(require (file "../2.4.3 Data-Directed Programming and Additivity/funcs-table.rkt"))

(display "测试基本功能\n")
(define n1 (make-scheme-number 1))
(define p1 (make-polynomial-parse 'x (list
                                      (list 2 n1)
                                      (list 0 n1))))
(define p2 (make-polynomial-parse 'x (list
                                      (list 3 n1)
                                      (list 0 n1))))
(define rf (make-rational p2 p1))
(apply-generic 'add rf rf)

(display "\n测试最大公因子\n")
(define p3 (make-polynomial-parse 'x (list
                                      (list 4 (make-scheme-number 1))
                                      (list 3 (make-scheme-number -1))
                                      (list 2 (make-scheme-number -2))
                                      (list 1 (make-scheme-number 2)))))
(define p4 (make-polynomial-parse 'x (list
                                      (list 3 (make-scheme-number 1))
                                      (list 1 (make-scheme-number -1)))))
(apply-generic 'greatest-common-divisor p3 p4) 
                                     
(define p5 (make-polynomial-parse 'x (list
                                      (list 2 (make-scheme-number 1))
                                      (list 1 (make-scheme-number -2))
                                      (list 0 (make-scheme-number 1)))))
(define p6 (make-polynomial-parse 'x (list
                                      (list 2 (make-scheme-number 11))
                                      (list 0 (make-scheme-number 7)))))
(define p7 (make-polynomial-parse 'x (list
                                      (list 1 (make-scheme-number 13))
                                      (list 0 (make-scheme-number 5)))))
(define p8 (apply-generic 'mul p5 p6))
(define p9 (apply-generic 'mul p5 p7))
(apply-generic 'greatest-common-divisor p8 p9)
(apply-generic 'reduce p8 p9)

(display "\n测试约分\n")
(define p11 (make-polynomial-parse 'x (list (list 1 (make-scheme-number 1)) (list 0 (make-scheme-number 1)))))
(define p12 (make-polynomial-parse 'x (list (list 3 (make-scheme-number 1)) (list 0 (make-scheme-number -1)))))
(define p13 (make-polynomial-parse 'x (list (list 1 (make-scheme-number 1)))))
(define p14 (make-polynomial-parse 'x (list (list 2 (make-scheme-number 1)) (list 0 (make-scheme-number -1)))))

(define rf1 (make-rational p11 p12))
(define rf2 (make-rational p13 p14))

(apply-generic 'add rf1 rf2)


