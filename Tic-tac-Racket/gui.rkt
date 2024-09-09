#lang racket/gui

(require "logic-utilities.rkt")
(require "game-status.rkt")
(require "try-to-win.rkt")

;; Global Variables
(define global-rows 3)
(define global-columns 3)
(define blue (make-object color% 102 140 255 1))
(define red (make-object color% 255 102 102 1))

(define game-grid (get-matrix global-rows global-columns '_))

;; Track the current selection (row, column)
(define current-row 0)
(define current-column 0)

;; Helper function to update a list at a specific index
(define (update-list lst index value)
  (append (take lst index) (list value) (drop lst (+ index 1))))

;; Helper function to set a value in the matrix
(define (matrix-set-at matrix row column value)
  (update-list matrix row (update-list (list-ref matrix row) column value)))

;; Window to play the game
(define game-frame (new frame%
                   [label "Tic Tac Racket"]
                   [width 600]
                   [height 600]))

;; Canvas-box class
(define canvas-box%
  (class canvas%
    (init-field [character #\space]
                [row 0]
                [column 0]
                [color blue])
    (inherit get-dc)

    ;; Event listener for keyboard navigation and selection
    (define/override (on-char e)
  (define key (send e get-key-code))  ;; Get the key code

  ;; Debugging: Print key for reference
  (displayln (format "Key pressed: ~a" key))

  ;; Check for arrow keys and the 'q' key for selection
  (cond
    [(equal? key #\w) (set! current-row (max 0 (- current-row 1)))]
    [(equal? key #\s) (set! current-row (min (- global-rows 1) (+ current-row 1)))]
    [(equal? key #\a) (set! current-column (max 0 (- current-column 1)))]
    [(equal? key #\d) (set! current-column (min (- global-columns 1) (+ current-column 1)))]
    [(equal? key #\k)
     ;; Mark the current cell with 'x' regardless of its previous state
     (set! game-grid (matrix-set-at game-grid current-row current-column 'x))
     ;; Update the character displayed in the selected canvas
     (send (list-ref canvases (+ current-column (* current-row global-columns))) set-character #\x)
     (send (list-ref canvases (+ current-column (* current-row global-columns))) refresh)
     (displayln game-grid)

     ;; Check for win or draw after player move
     (when (draw? game-grid)
       (message-box "Draw" "It's a draw!")
       (reset-game))

     (when (winner? game-grid)
       (message-box "Win" "You won!")
       (reset-game))

     ;; Computer's turn, only if the game isn't over
     (when (and (not (draw? game-grid)) (not (winner? game-grid)))
       (let ((move (get-computer-next-move game-grid global-rows global-columns)))
         ;; Set computer move and refresh the canvas
         (set-computer-move move)))

     ;; Check again for a win after the computer's move
     (when (winner? game-grid)
       (message-box "Win" "The computer won!")
       (reset-game))

     ;; Check for a draw after the computer's move
     (when (draw? game-grid)
       (message-box "Draw" "It's a draw!")
       (reset-game))])

  ;; Refresh the display to update the current selection
  (send game-frame refresh)
  (send this refresh))


    ;; Ensure canvas focus when clicked or activated
    (define/override (on-event e)
      (send this focus))  ;; Force focus on the canvas for keyboard input

    ;; Draw the canvas, with a highlight if this is the current selection
    (define/override  (on-paint)
      (let ((dc (get-dc)))
        (send dc clear) ;; Clear the canvas before drawing
        (let-values (((x y) (send this get-size)))
          (when (and (= row current-row) (= column current-column))
            (send dc set-brush (make-object brush% "yellow" 'solid))  ;; Highlight current selection
            (send dc draw-rectangle 0 0 x y))
          (send dc set-text-foreground (get-canvas-color character))
          (send dc set-font (make-object font% (+ 60 (* -2 (+ global-rows global-columns))) 'default))
          (send dc draw-text (string character) (/ (- x (+ 60 (* -2 (+ global-rows global-columns)))) 2) (/ (- y (+ 93 (* -3 (+ global-rows global-columns)))) 2)))))

    (define/public (set-color c)
      (set! color c))
   
    (define/public (set-character char)
      (set! character char))
   
    (super-new)))

;; Returns the color associated with the playable character
(define (get-canvas-color character)
  (cond ((equal? character #\x) blue)
        (else red)))

;; Panes
;; Main Pane
(define main-pane (new vertical-pane% [parent game-frame] [vert-margin 5] [horiz-margin 5] [spacing 5]))

;; Creates and links the game grid panes to hold the canvases
(define (get-panes row)
  (cond
    ((>= row global-rows) '())
    (else (cons (new horizontal-pane% [parent main-pane] [spacing 5])
                (get-panes (+ row 1))))))

;; Creates and links the game grid canvases to the panes
(define (get-panes-canvases panes row)
  (cond
    ((null? panes) '())
    (else (append (get-pane-canvases (car panes) row 0) (get-panes-canvases (cdr panes) (+ row 1))))))

;; Creates and links the canvases to a single pane
(define (get-pane-canvases pane row column)
  (cond
    ((>= column global-columns) '())
    (else (cons (new canvas-box%
                   [row row]
                   [column column]
                   [parent pane])
                (get-pane-canvases pane row (+ column 1))))))


; Declare default panes and its canvases
(define panes (get-panes 0))
(define canvases (get-panes-canvases panes 0))

;; Resets all game setup for a new game
(define (reset-game)
  ; reset canvas characters
  (for [(canvas  canvases)]
      (send canvas set-character #\Space)
      (send canvas set-color (make-object color% "gray"))
      (send canvas refresh))
  ; reset game grid
  (set! game-grid (get-matrix global-rows global-columns '_))
  (send game-frame refresh))

;; Sets a computer move
(define (set-computer-move row-and-column)
  (displayln row-and-column)
  (set! game-grid (matrix-set-at game-grid (car row-and-column) (cadr row-and-column) 'o))
  (send (list-ref canvases (+ (cadr row-and-column) (* (car row-and-column) global-columns))) set-character #\o)
  (send (list-ref canvases (+ (cadr row-and-column) (* (car row-and-column) global-columns))) refresh))

;;----------------------- Input Frame -------------------------

;; Window to input the game grid's rows and columns
(define input-frame (new frame%
                      [label "Tic Tact Racket - Dimensions"]
                      [width 400]
                      [height 200]))

;; Input dimensions explanatory message
(define message (new message%
                     (parent input-frame)
                     (label "Tic Tac Racket grid dimensions:")
                     [vert-margin 15]))

;; List-control to select the number of rows
(define rows-input (new choice%
                     (label "Rows       ")
                     (parent input-frame)
                     (choices (list "3" "4" "5" "6" "7" "8" "9" "10"))))

;; List-control to select the number of columns
(define columns-input (new choice%
                        (label "Columns ")
                        (parent input-frame)
                        (choices (list "3" "4" "5" "6" "7" "8" "9" "10"))))

;; Button to set the grid dimensions and start playing the game
(define play-btn (new button%
                   [parent input-frame]
                   [label "Play"]
                   [min-width 75]
                   [min-height 50]
                   [vert-margin 15]
                   [callback (λ (b e)
                     ; Overriding the default game-frame contents
                     (set! global-rows (string->number (send rows-input get-string-selection)))
                     (set! global-columns (string->number (send columns-input get-string-selection)))
                     (set! game-frame (new frame% [label "Tic Tac Racket"] [width 600] [height 600]))
                     (send game-frame center 'both)
                     (set! game-grid (get-matrix global-rows global-columns '_))
                     (set! main-pane (new vertical-pane% [parent game-frame] [vert-margin 5] [horiz-margin 5] [spacing 5]))
                     (set! panes (get-panes 0))
                     (set! canvases (get-panes-canvases panes 0))

                     ; Switching from input-frame to game-frame
                     (send game-frame show #t)
                     (send input-frame show #f) )]))

;; Starts the game by showing the window to input the grid dimensions
(define (TicTacToe)
  ; Center both frames as default
  (send game-frame center 'both)
  (send input-frame center 'both)
  (send input-frame show #t))

(TicTacToe)
