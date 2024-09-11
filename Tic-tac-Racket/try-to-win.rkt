#lang racket

(require "logic-utilities.rkt")

;; Function to sum a list of numbers (replaces apply +)
(define (sum-list lst)
  (if (null? lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))

;; Recursive map function (replaces map)
(define (map-list fn lst)
  (if (null? lst)
      '()
      (cons (fn (car lst)) (map-list fn (cdr lst)))))

;; Function to detect if a player is about to win and block their move
(define (detect-blocking-opportunity grid numRows numColumns symbol)
  (define (count-in-line line)
    (cond
      ((and (not (memv 'x line)) 
            (equal? (length (filter (λ (x) (equal? x symbol)) line)) (- numColumns 1)))
       10)
      (else 0)))
  
  ;; Check rows and columns manually without map
  (define rows (map-list count-in-line grid))
  (define cols (map-list count-in-line (transpose grid)))
  (define diag1 (count-in-line (diagonal1 grid numRows numColumns)))
  (define diag2 (count-in-line (diagonal2 grid numRows numColumns)))

  ;; Sum the potential threats across rows, columns, and diagonals
  (+ (sum-list rows) (sum-list cols) diag1 diag2))

;; Helper function for matrix transpose (replaces apply map list)
(define (transpose m)
  (if (null? (car m))
      '()
      (cons (map car m) (transpose (map cdr m)))))

;; Helper function to extract diagonal1
(define (diagonal1 grid numRows numColumns)
  (define (helper i)
    (if (= i numRows)
        '()
        (cons (list-ref (list-ref grid i) i) (helper (+ i 1)))))
  (helper 0))

;; Helper function to extract diagonal2
(define (diagonal2 grid numRows numColumns)
  (define (helper i)
    (if (= i numRows)
        '()
        (cons (list-ref (list-ref grid i) (- numColumns i 1)) (helper (+ i 1)))))
  (helper 0))

;; Heuristic function to evaluate the grid for the current player ('x)
(define (evaluate-grid grid numRows numColumns symbol)
  (define (count-in-line line)
    (cond
      ((and (not (memv 'o line)) 
            (equal? (length (filter (λ (x) (equal? x symbol)) line)) (- numColumns 1)))
       10)
      ((memv 'o line) -10)
      (else 0)))

  ;; Evaluate rows, columns, and diagonals using map-list
  (define rows (map-list count-in-line grid))
  (define cols (map-list count-in-line (transpose grid)))
  (define diag1 (count-in-line (diagonal1 grid numRows numColumns)))
  (define diag2 (count-in-line (diagonal2 grid numRows numColumns)))
  
  (+ (sum-list rows) (sum-list cols) diag1 diag2))

;; Greedy algorithm with blocking priority
(define (get-computer-next-move grid numRows numColumns)
  ;; Helper function to find the best move
  (define (find-best-move grid best-move best-score i j)
    (define (check-blocking i j best-move)
      (if (equal? (list-ref (list-ref grid i) j) '_)
          (let ([new-grid (matrix-set-at grid i j 'o)]) ;; Inline definition of new-grid
            (define block-score (detect-blocking-opportunity new-grid numRows numColumns 'o))
            (if (> block-score 0)
                (list i j)
                best-move))
          best-move))
    
    (define (check-greedy i j best-move best-score)
      (if (equal? (list-ref (list-ref grid i) j) '_)
          (let ([new-grid (matrix-set-at grid i j 'x)]) ;; Inline definition of new-grid
            (define score (evaluate-grid new-grid numRows numColumns 'x))
            (if (> score best-score)
                (find-best-move new-grid (list i j) score i j)
                best-move))
          best-move))

    ;; Check for blocking moves first
    (define blocking-move (check-blocking i j best-move))

    ;; If no block move found, proceed with the greedy approach
    (if (equal? blocking-move '(-1 -1))
        (check-greedy i j best-move best-score)
        blocking-move))
  
  ;; Recursive function to iterate over the grid
  (define (find-move-loop grid numRows numColumns best-move best-score i j)
    (if (>= i numRows)
        best-move
        (if (>= j numColumns)
            (find-move-loop grid numRows numColumns best-move best-score (+ i 1) 0)
            (find-move-loop grid numRows numColumns (find-best-move grid best-move best-score i j) best-score i (+ j 1)))))

  ;; Initial call
  (find-move-loop grid numRows numColumns '(-1 -1) -inf.0 0 0))

(provide (all-defined-out))
