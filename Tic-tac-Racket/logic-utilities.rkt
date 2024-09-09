#lang racket

;; Creates a n lenght list with a given value
(define (get-list n value)
  (cond ((< n 1) '())
        (else (cons value (get-list (- n 1) value)))))

;; Creates a matrix of given rows and columns with a given value
(define (get-matrix rows columns value)
  (cond ((< rows 1) '())
        (else (cons (get-list columns value) (get-matrix (- rows 1) columns value)))))

;; Returns the list with the value setted at the given index
(define (list-set-at list index value)
  (cond ((null? list) '())
        ((equal? index 0) (cons value (list-set-at (cdr list) (- index 1) value)))
        (else (cons (car list) (list-set-at (cdr list) (- index 1) value)))))

;; Returns the matrix with the value setted at the given row and column
(define (matrix-set-at matrix row column value)
  (cond ((null? matrix) '())
        ((equal? row 0) (cons (list-set-at (car matrix) column value) (matrix-set-at (cdr matrix) (- row 1) column value)))
        (cons (cons (car matrix) (matrix-set-at (cdr matrix) (- row 1) column value)))))

(provide (all-defined-out))