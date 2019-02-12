;; 测试新复数包
#lang racket
(require "install-new-complex-package.rkt")
(require "install-rational-package.rkt")
(require "install-scheme-number-package.rkt")
(require "apply-generic.rkt")

(install-new-complex-package)
(install-scheme-number-package)
(install-rational-package)
(define c1 (make-complex-from-real-imag (make-scheme-number 3) (make-scheme-number 4)))
(define c2 (make-complex-from-real-imag (make-scheme-number 7) (make-scheme-number 5)))
(apply-generic 'add c1 c2)

