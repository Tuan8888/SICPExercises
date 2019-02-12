;; 数字包
#lang racket
(require (file "../2.4.3 Data-Directed Programming and Additivity/funcs-table.rkt"))
(require "tag-datum.rkt")
(require "install-rational-package.rkt")
(require "install-polynomial-parse-package.rkt")
(provide (all-defined-out))

;; 数字包
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  ;; 判断数字是否为0
  (define (number-zero? n)
    (= n 0))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'square '(scheme-number)
       (lambda (x) (* x x)))
  (put 'sqrt '(scheme-number)
       (lambda (x) (sqrt x)))
  (put 'sin '(scheme-number)
       (lambda (x) (sin x)))
  (put 'cos '(scheme-number)
       (lambda (x) (cos x)))
  (put 'atan '(scheme-number)
       (lambda (x) (atan x)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  ;; 判断数字是否相等
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'zero? '(scheme-number) number-zero?)
  ;; 求负数
  (put 'negation '(scheme-number)
       (lambda (x) (tag (- x))))
  ;; 指数计算
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  ;; 转换为多项式
  (put 'to-polynomial '(symbol scheme-number)
       (lambda (var x) (make-polynomial-parse var (list (list 0 x)))))
  ;; 返回pseudo的因子
  (put 'pseudo-factor '(scheme-number scheme-number scheme-number)
       (lambda (c o1 o2)
         (tag (expt c (+ 1 o1 o2)))))
  ;; 求一组数的最大公约数
  (put 'gcd 'scheme-number
       (lambda (L)
         (let ((numbers (map contents L)))
           (tag (apply gcd numbers)))))
  ;; 数字标记
  (put 'isNumber? 'scheme-number true)
  ;; 两个数约分
  (put 'reduce '(scheme-number scheme-number)
       (lambda (n d)
         (let ((g (gcd n d)))
           (list (/ n g) (/ d g)))))
  'done)

;; 构造函数
(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))

(install-scheme-number-package)
