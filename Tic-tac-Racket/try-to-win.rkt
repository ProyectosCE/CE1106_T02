#lang racket

(require "logic-utilities.rkt")

;; Greedy algorithm for determining the next move

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

;; Greedy move finder: Selects the best move based on immediate rewards (maximizing 'x symbols or blocking 'o)
(define (get-computer-next-move grid numRows numColumns)
  (define best-move '(-1 -1)) ;; Initial dummy move
  (define best-score -inf.0) ;; Start with negative infinity

  ;; Iterate through the grid and evaluate each possible move
  (for* ([i (in-range numRows)] [j (in-range numColumns)])
    (when (equal? (list-ref (list-ref grid i) j) '_) ;; Empty space '_
      (let* ([new-grid (matrix-set-at grid i j 'x)] ;; Hypothetical move for 'x
             [score (evaluate-grid new-grid numRows numColumns 'x)]) ;; Score this move
        (when (> score best-score) ;; If this is the best move, save it
          (set! best-score score)
          (set! best-move (list i j))))))
  
  best-move)

(provide (all-defined-out))
