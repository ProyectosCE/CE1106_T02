#lang racket

(require "logic-utilities.rkt")

;; Function to detect if a player is about to win and block their move
(define (detect-blocking-opportunity grid numRows numColumns symbol)
  (define (count-in-line line)
    (cond
      ;; If the line has the opponent's symbols but one spot left, it's a threat
      ((and (not (memv 'x line)) (equal? (length (filter (λ (x) (equal? x symbol)) line)) (- numColumns 1))) 10)
      ;; Otherwise, it's not a threat
      (else 0)))
  
  ;; Check rows for blocking opportunity
  (define rows (map count-in-line grid))
  (define cols (map count-in-line (apply map list grid))) ; Transpose matrix for columns
  (define diag1 (count-in-line (for/list ([i (in-range numRows)]) (list-ref (list-ref grid i) i))))
  (define diag2 (count-in-line (for/list ([i (in-range numRows)]) (list-ref (list-ref grid i) (- numColumns i 1)))))
  
  ;; Sum the potential threats across rows, columns, and diagonals
  (+ (apply + rows) (apply + cols) diag1 diag2))

;; Heuristic function to evaluate the grid for the current player ('x)
;; Returns a score based on the number of immediate wins or threats.
(define (evaluate-grid grid numRows numColumns symbol)
  (define (count-in-line line)
    (cond
      ;; If a line is all symbols of the current player and no opponent, it's a potential win
      ((and (not (memv 'o line)) (equal? (length (filter (λ (x) (equal? x symbol)) line)) (- numColumns 1))) 10)
      ;; If a line contains an opponent symbol, it's less valuable
      ((memv 'o line) -10)
      ;; Otherwise, it's neutral
      (else 0)))
  
  ;; Evaluate rows, columns, and diagonals
  (define rows (map count-in-line grid))
  (define cols (map count-in-line (apply map list grid))) ; Transpose matrix for columns
  (define diag1 (count-in-line (for/list ([i (in-range numRows)]) (list-ref (list-ref grid i) i))))
  (define diag2 (count-in-line (for/list ([i (in-range numRows)]) (list-ref (list-ref grid i) (- numColumns i 1)))))
  
  (+ (apply + rows) (apply + cols) diag1 diag2))

;; Greedy algorithm with blocking priority
(define (get-computer-next-move grid numRows numColumns)
  (define best-move '(-1 -1)) ;; Initial dummy move
  (define best-score -inf.0) ;; Start with negative infinity

  ;; First: Check if the opponent has a near-winning move and block it
  (for* ([i (in-range numRows)] [j (in-range numColumns)])
    (when (equal? (list-ref (list-ref grid i) j) '_) ;; Empty space '_
      (let* ([new-grid (matrix-set-at grid i j 'o)] ;; Hypothetical move for 'o (opponent)
             [block-score (detect-blocking-opportunity new-grid numRows numColumns 'o)]) ;; Check for blocking need
        (when (> block-score 0) ;; If there is a blocking need, select this move
          (set! best-move (list i j))))))
  
  ;; Second: If no blocking is needed, apply the greedy algorithm to maximize 'x's progress
  (when (equal? best-move '(-1 -1)) ;; If no block move was found
    (for* ([i (in-range numRows)] [j (in-range numColumns)])
      (when (equal? (list-ref (list-ref grid i) j) '_) ;; Empty space '_
        (let* ([new-grid (matrix-set-at grid i j 'x)] ;; Hypothetical move for 'x (computer)
               [score (evaluate-grid new-grid numRows numColumns 'x)]) ;; Score this move
          (when (> score best-score) ;; If this is the best move, save it
            (set! best-score score)
            (set! best-move (list i j)))))))
  
  best-move)

(provide (all-defined-out))
