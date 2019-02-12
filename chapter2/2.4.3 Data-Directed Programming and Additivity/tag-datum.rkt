;; 带标签数据
#lang racket
(provide (all-defined-out))

;; 给数据打标签
(define (attach-tag type-tag contents)
  (cons type-tag contents))

;; 获取数据标签
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

;; 获取数据内容
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
