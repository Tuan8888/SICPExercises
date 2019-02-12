;; Exercise 2.79
;; 算法
#lang racket
(require "install-complex-package.rkt")
(require "install-scheme-number-package.rkt")
(require "install-rational-package.rkt")
(require "install-integer-package.rkt")
(require "install-real-package.rkt")
(require "apply-generic.rkt")
(require "drop.rkt")

;; 安装
(install-complex-package)
(install-scheme-number-package)
(install-rational-package)

;; 测试各包
(display "测试各包基本功能")
(newline)
(define c1 (make-complex-from-mag-ang 3 1))
(define c2 (make-complex-from-real-imag 2 5))
(define c4 (make-complex-from-real-imag 2 5))
(define c3 (apply-generic 'add c1 c2))
(apply-generic 'real-part c3)
(apply-generic 'imag-part c3)
(apply-generic 'equ? c1 c2)
(apply-generic 'equ? c2 c4)
(apply-generic 'equ? 3 4)
(apply-generic 'equ? 4 4)
(apply-generic 'equ? (make-rational 3 4) (make-rational 2 5))
(apply-generic 'equ? (make-rational 3 4) (make-rational 3 4))

(apply-generic 'zero? (make-complex-from-mag-ang 0 0))
(apply-generic 'zero? (make-complex-from-real-imag 3 0))
(apply-generic 'zero? 0)
(apply-generic 'zero? 3)
(apply-generic 'zero? (make-rational 0 3))
(apply-generic 'zero? (make-rational 2 4))
(newline)

;; 测试类型转换
(display "测试类型转换")
(newline)
(apply-generic 'add 2 c2)
(apply-generic 'exp 2 2)
(newline)

;; raise测试
(display "raise测试")
(newline)
(install-integer-package)
(install-real-package)
(apply-generic 'raise (make-integer 3))
(apply-generic 'raise (make-rational 3 4))
(apply-generic 'raise (make-real 3))
(apply-generic-with-raise 'add (make-integer 3) c2)
(apply-generic 'project (make-real 3.45))
(apply-generic 'project (make-rational 3 4))
(apply-generic 'project (make-complex-from-real-imag 3 4))
(newline)

;; drop测试
(display "drop测试")
(newline)
(apply-drop (make-complex-from-real-imag 3 4))
(apply-drop (make-complex-from-real-imag 3 0))
(apply-drop (make-real 3.4))
(apply-drop (make-real 3.0))
(apply-drop (make-rational 3 1))
(newline)
