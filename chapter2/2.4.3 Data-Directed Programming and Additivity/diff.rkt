;; Exercise 2.73
;; 数据导向的求导程序
#lang racket
(require "tag-datum.rkt")
(require "funcs-table.rkt")
(provide (all-defined-out))

;; 判断是否等于某个数字
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; sum数据类型
(define (install-sum-package)
  ;; 内部函数
  ;; 构造函数
  (define (make-sum x y)
    (cond ((=number? x 0) y)
          ((=number? y 0) x)
          ((and (number? x) (number? y)) (+ x y))
          (else (attach-tag '+ (list x y)))))
  ;; 获取第一个加数
  (define (addend s)
    (car s))
  ;; 获取第二个加数
  (define (augend s)
    (cadr s))
  ;; 求导
  (define (diff-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  ;; 注册函数
  (put 'deriv '+ diff-sum)
  (put 'make  '+ make-sum)) 

;; product数据类型
(define (install-product-package)
  ;; 内部函数
  ;; 构造函数
  (define (make-product x y)
    (cond ((=number? x 1) y)
          ((=number? y 1) x)
          ((or (=number? x 0) (=number? y 0)) 0)
          ((and (number? x) (number? y)) (+ x y))
          (else (attach-tag '* (list x y)))))
  ;; 获取第一个乘数
  (define (multiplier s)
    (car s))
  ;; 获取第二个乘数
  (define (multiplicand s)
    (cadr s))
  ;; 求导
  (define (diff-product operands var)
    (make-sum (make-product
               (multiplier operands)
               (deriv (multiplicand operands) var))
              (make-product
               (deriv (multiplier operands) var)
               (multiplicand operands))))
  ;; 接口
  (put 'deriv '* diff-product)
  (put 'make  '* make-product))

;; product数据类型
(define (install-product-package)
  ;; 内部函数
  ;; 构造函数
  (define (make-product x y)
    (cond ((=number? x 1) y)
          ((=number? y 1) x)
          ((or (=number? x 0) (=number? y 0)) 0)
          ((and (number? x) (number? y)) (+ x y))
          (else (attach-tag '* (list x y)))))
  ;; 获取第一个乘数
  (define (multiplier s)
    (car s))
  ;; 获取第二个乘数
  (define (multiplicand s)
    (cadr s))
  ;; 求导
  (define (diff-product operands var)
    (make-sum (make-product
               (multiplier operands)
               (deriv (multiplicand operands) var))
              (make-product
               (deriv (multiplier operands) var)
               (multiplicand operands))))
  ;; 接口
  (put 'deriv '* diff-product)
  (put 'make  '* make-product))

;; 指数数据类型
(define (install-expt-package)
  ;; 内部函数
  ;; 构造函数
  (define (make-expt x y)
    (cond ((=number? y 0) 1)
          ((=number? y 1) x)
          ((and (number? x) (number? y)) (expt x y))
          (else (attach-tag 'expt (list x y)))))
  ;; 获取基数
  (define (base s)
    (car s))
  ;; 获取指数
  (define (exponent s)
    (cadr s))
  ;; 求导
  (define (diff-expt operands var)
    (let ((b (base operands))
          (e (exponent operands)))
      (make-product e
                    (make-product (deriv e var)
                                  (make-expt b (make-sum e -1))))))
  ;; 接口
  (put 'deriv 'expt diff-expt)
  (put 'make  'expt make-expt))

;; 安装模块包
(install-sum-package)
(install-product-package)
(install-expt-package)

;; 构造函数
(define make-sum (get 'make '+))
(define make-product (get 'make '*))
(define make-expt (get 'make 'expt))

;; 判断是不是相同变量
(define (same-variable? v1 v2)
  (and (symbol? v1) (symbol? v2) (eq? v1 v2)))

;; 求导
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((symbol? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; 测试
(deriv ' (+ (* 3 x) (+ y (* x x))) 'x)
