#lang racket/gui

(require "game_logic.rkt")

; Crea una lista de longitud n con un valor dado
(define (get-list n value)
  (cond ((< n 1) '())
        (else (cons value (get-list (- n 1) value)))))

; Crear una matriz de filas y columnas dadas con un valor dado
(define (get-matrix rows columns value)
  (cond ((< rows 1) '())
        (else (cons (get-list columns value) (get-matrix (- rows 1) columns value)))))

; Retorna la lista con el valor establecido en el índice dado
(define (list_set_at list index value)
  (cond ((null? list) '())
        ((equal? index 0) (cons value (list_set_at (cdr list) (- index 1) value)))
        (else (cons (car list) (list_set_at (cdr list) (- index 1) value)))))

(define global-columns 3)
(define global-rows 3)
(define blue (make-object color% 0 0 255))
(define red (make-object color% 255 0 0))
(define game-grid (get-matrix global-rows global-columns '_))

(define current-row 0)
(define current-column 0)

; Funcion para actualizar una lista en un índice específico
(define (update-list lst index value)
  (append (take lst index) (list value) (drop lst (+ index 1))))

; Fincion para establecer un valor en la matriz
(define (matrix-set-at matrix row column value)
  (update-list matrix row (update-list (list-ref matrix row) column value)))

; Ventana para jugar el juego
(define game-frame (new frame%
                   [label "Tic Tac Toe"]
                   [width 600]
                   [height 600]))

; Clase para crear un canvas con un color y un carácter específico
(define canvas-box%
  (class canvas%
    (init-field [character #\space]
                [row 0]
                [column 0]
                [color blue])
    (inherit get-dc)

    ; Nagevación con teclado y selección
    (define/override (on-char e)
  (define key (send e get-key-code))  

  ; Debug: Imprime la tecla presionada
  ;(displayln (format "Key pressed: ~a" key))

  ; Verifica las teclas de flecha y la tecla 'q' para la selección
  (cond
    [(equal? key #\w) (set! current-row (max 0 (- current-row 1)))]
    [(equal? key #\s) (set! current-row (min (- global-rows 1) (+ current-row 1)))]
    [(equal? key #\a) (set! current-column (max 0 (- current-column 1)))]
    [(equal? key #\d) (set! current-column (min (- global-columns 1) (+ current-column 1)))]
    [(equal? key #\k)
     ; Marca la celda actual con 'x' independientemente de su estado anterior
     (set! game-grid (matrix-set-at game-grid current-row current-column 'x))
     ; Actualiza el carácter mostrado en el canvas seleccionado
     (send (list-ref canvases (+ current-column (* current-row global-columns))) set-character #\x)
     (send (list-ref canvases (+ current-column (* current-row global-columns))) refresh)
     (displayln game-grid)


    ; ======================================== CAMBIAR ========================================
     ; Verifica si el jugador ganó o si hay un empate después del movimiento del jugador
     (when (draw? game-grid)
       (message-box "Draw" "It's a draw!")
       (reset-game))

     (when (winner? game-grid)
       (message-box "Win" "You won!")
       (reset-game))])

#|
     ; Turno del computador, solo si el juego no ha terminado
     (when (and (not (draw? game-grid)) (not (winner? game-grid)))
       (let ((move (get-computer-next-move game-grid global-rows global-columns)))
         ;; Set computer move and refresh the canvas
         (set-computer-move move)))

     ; Verifica otra vez si el jugador ganó después del movimiento del computador
     (when (winner? game-grid)
       (message-box "Win" "The computer won!")
       (reset-game))

     ; Verifica si hay un empate después del movimiento del computador
     (when (draw? game-grid)
       (message-box "Draw" "It's a draw!")
       (reset-game))])

  |#

  ; Actualiza la pantalla para mostrar la selección actual
  (send game-frame refresh)
  (send this refresh))

    ; Forzar el foco en el canvas para la entrada del teclado
    (define/override (on-event e)
      (send this focus)) 

  ; Dibuja el canvas, con un resaltado si esta es la selección actual  
    (define/override  (on-paint)
      (let ((dc (get-dc)))
        (send dc clear)
        (let-values (((x y) (send this get-size)))
          (when (and (= row current-row) (= column current-column))
            (send dc set-brush (make-object brush% "yellow" 'solid))  
            (send dc draw-rectangle 0 0 x y))
          (send dc set-text-foreground (get-canvas-color character))
          (send dc set-font (make-object font% (+ 60 (* -2 (+ global-rows global-columns))) 'default))
          (send dc draw-text (string character) (/ (- x (+ 60 (* -2 (+ global-rows global-columns)))) 2) (/ (- y (+ 93 (* -3 (+ global-rows global-columns)))) 2)))))

    (define/public (set-color c)
      (set! color c))
   
    (define/public (set-character char)
      (set! character char))
   
    (super-new)))

; Retorna el color asociado con el carácter jugable
(define (get-canvas-color character)
  (cond ((equal? character #\x) blue)
        (else red)))

; Paneles
; Panel principal
(define main-pane (new vertical-pane% [parent game-frame] [vert-margin 5] [horiz-margin 5] [spacing 5]))

; Crea y vincula los paneles de la cuadrícula de juego para contener los lienzos
(define (get-panes row)
  (cond
    ((>= row global-rows) '())
    (else (cons (new horizontal-pane% [parent main-pane] [spacing 5])
                (get-panes (+ row 1))))))

; Crea y vincula los lienzos de la cuadrícula de juego a un solo panel
(define (get-panes-canvases panes row)
  (cond
    ((null? panes) '())
    (else (append (get-pane-canvases (car panes) row 0) (get-panes-canvases (cdr panes) (+ row 1))))))

; Crea y vincula los lienzos de la cuadrícula de juego a un solo panel
(define (get-pane-canvases pane row column)
  (cond
    ((>= column global-columns) '())
    (else (cons (new canvas-box%
                   [row row]
                   [column column]
                   [parent pane])
                (get-pane-canvases pane row (+ column 1))))))

; Declarar paneles predeterminados y sus lienzos
(define panes (get-panes 0))
(define canvases (get-panes-canvases panes 0))

; Resetea el juego
(define (reset-game)
  (for [(canvas  canvases)]
      (send canvas set-character #\Space)
      (send canvas set-color (make-object color% "gray"))
      (send canvas refresh))
  (set! game-grid (get-matrix global-rows global-columns '_))
  (send game-frame refresh))

; Define el movimiento del computador
(define (set-computer-move row-and-column)
  (displayln row-and-column)
  (set! game-grid (matrix-set-at game-grid (car row-and-column) (cadr row-and-column) 'o))
  (send (list-ref canvases (+ (cadr row-and-column) (* (car row-and-column) global-columns))) set-character #\o)
  (send (list-ref canvases (+ (cadr row-and-column) (* (car row-and-column) global-columns))) refresh))

; Funcion principal
(define (TTT rows columns)
    (cond 
        ((and (< rows 3) (< columns 3)) (message-box "Error" "The minimum size is 3x3"))
        ((and (> rows 10) (> columns 10)) (message-box "Error" "The maximum size is 10x10"))
        (else 
            (set! global-rows rows)
            (set! global-columns columns)

           
            (set! game-frame (new frame% [label "Tic Tac Racket"] [width 600] [height 600]))
            (send game-frame center 'both)
            (set! game-grid (get-matrix global-rows global-columns '_))
            (set! main-pane (new vertical-pane% [parent game-frame] [vert-margin 5] [horiz-margin 5] [spacing 5]))
            (set! panes (get-panes 0))
            (set! canvases (get-panes-canvases panes 0))
            (send game-frame center 'both)
            (send game-frame show #t))))

(TTT 5 5)