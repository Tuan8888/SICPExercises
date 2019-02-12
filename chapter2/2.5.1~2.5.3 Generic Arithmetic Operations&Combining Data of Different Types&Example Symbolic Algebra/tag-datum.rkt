;; Exercise 2.78
;; 带标签数据，可处理系统自带number和symbol
#lang racket
(provide (all-defined-out))

;; 给数据打标签
(define (attach-tag type-tag contents)
  (cons type-tag contents))

;; 获取数据标签
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((symbol? datum) 'symbol)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

;; 获取数据内容
(define (contents datum)
  (cond ((or (number? datum) (symbol? datum)) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))
