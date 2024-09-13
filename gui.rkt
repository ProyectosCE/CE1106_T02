#lang racket/gui

(require "game_logic.rkt")

#|
============================================================================================= 
MIT License
Copyright (c) 2024 José Bernardo Barquero Bonilla,
                   Jose Eduardo Campos Salazar,
                   Jimmy Feng Feng,
                   Alexander Montero Vargas
Consulta el archivo LICENSE para más detalles.
=============================================================================================
|#

(define global-columns 3)
(define global-rows 3)
(define blue (make-object color% 0 0 255))
(define red (make-object color% 255 0 0))
(define game-grid (get-matrix global-rows global-columns '_))

(define current-row 0)
(define current-column 0)


;; Función para mostrar mensajes cortos de gane o empate
(define (mostrar-mensaje-simple titulo texto icono)
  (define mi-dialogo (new dialog%
                          [label titulo]
                          [width 200]
                          [height 100]))

  ;; Cambiar el ícono de la ventana
  (send mi-dialogo set-icon (read-bitmap icono))

  ;; Crear el mensaje usando message%
  (new message%
       [parent mi-dialogo]
       [label texto])

  ;; Agregar un botón "OK" para cerrar el diálogo
  (new button%
       [parent mi-dialogo]
       [label "OK"]
       [callback (lambda (btn event) (send mi-dialogo show #f))])

  ;; Mostrar el diálogo
  (send mi-dialogo show #t))


  ;; Función para mostrar contenidos largos con editor-canvas%
(define (mostrar-mensaje-con-editor titulo texto icono)
  (define mi-dialogo (new dialog%
                          [label titulo]
                          [width 400]
                          [height 200]))

  ;; Cambiar el ícono de la ventana
  (send mi-dialogo set-icon (read-bitmap icono))

  ;; Crear el editor y canvas para texto largo
  (define mi-editor (new text%))
  (send mi-editor insert texto)
  (new editor-canvas%
       [parent mi-dialogo]
       [editor mi-editor]
       [min-width 350]
       [min-height 100])

  ;; Agregar un botón "OK" para cerrar el diálogo
  (new button%
       [parent mi-dialogo]
       [label "OK"]
       [callback (lambda (btn event) (send mi-dialogo show #f))])

  ;; Mostrar el diálogo
  (send mi-dialogo show #t))

;; Función para mostrar la pantalla de ayuda
(define (mostrar-ayuda)
  (mostrar-mensaje-con-editor "Ayuda" "Use las teclas de flecha o WASD para mover\nUse 'espacio' o 'enter' para seleccionar." "assets/help.png"))

;; Función para mostrar la pantalla de información
(define (mostrar-informacion)
  (mostrar-mensaje-con-editor "Información" 
                   "Juego de Tic Tac Toe desarrollado en Racket \nusando paradigma funcional.\n\
Este programa está bajo licencia MIT \nCopyright (c) 2024\n\
Desarrollado por: José Bernardo Barquero Bonilla,\nJose Eduardo Campos Salazar,\n\
Jimmy Feng Feng, \nAlexander Montero Vargas"
                   "assets/information.png"))
;; Función para cambiar el tamaño del tablero
(define (cambiar-tamano)
  (send game-frame show #f)
  (send game-frame center 'both)
  (send input-frame show #t))

;; Ventana para jugar el juego
(define game-frame (new frame%
                        [label "Tic Tac Toe"]
                        [width 600]
                        [height 600]))

;; Clase para crear un canvas con un color y un carácter específico
(define canvas-box%
  (class canvas%
    (init-field [character #\space]
                [row 0]
                [column 0]
                [color (make-color 49 152 183)]) ; Color inicial del jugador en formato RGB
    (inherit get-dc)

    (define/override (on-char e)
      (define key (send e get-key-code))

      ;; Verifica las teclas de flecha, AWSD (mayúsculas y minúsculas) y espacio o enter para la selección
      (cond
        [(or (equal? key #\w) (equal? key #\W) (equal? key 'up)) 
         (set! current-row (max 0 (- current-row 1)))]
        [(or (equal? key #\s) (equal? key #\S) (equal? key 'down)) 
         (set! current-row (min (- global-rows 1) (+ current-row 1)))]
        [(or (equal? key #\a) (equal? key #\A) (equal? key 'left)) 
         (set! current-column (max 0 (- current-column 1)))]
        [(or (equal? key #\d) (equal? key #\D) (equal? key 'right)) 
         (set! current-column (min (- global-columns 1) (+ current-column 1)))]
        [(or (equal? key #\k) (equal? key #\space) (equal? key #\return))
         ;; Solo marca la celda si está vacía
         (when (cell-empty? current-row current-column)
           (set! game-grid (matrix-set-at game-grid current-row current-column 'x))
           ;; Actualiza el carácter mostrado en el canvas seleccionado
           (send (list-ref canvases (+ current-column (* current-row global-columns))) set-character #\x)
           (send (list-ref canvases (+ current-column (* current-row global-columns))) refresh)
           (displayln game-grid)

           ;; Verifica si el jugador ganó o si hay un empate después del movimiento del jugador
           (when (winner? game-grid)
             (mostrar-mensaje-simple "Ganador" "¡Has ganado!" "assets/trofeo.png") ; Usar función personalizada
             (reset-game))
           
           (when (draw? game-grid)
             (mostrar-mensaje-simple "Empate" "¡Es un empate!" "assets/empate.png") ; Usar función personalizada
             (reset-game))

           ;; Turno del greedy-bot, solo si el juego no ha terminado
           (when (and (not (draw? game-grid)) (not (winner? game-grid)))
             (let ((move (greedy-bot game-grid 'o))) ; Usa greedy-bot para obtener el movimiento
               ;; Establece el movimiento del greedy-bot y refresca el canvas
               (set-computer-move move)))

           ;; Verifica otra vez si el jugador ganó después del movimiento del greedy-bot
           (when (winner? game-grid)
             (mostrar-mensaje-simple "Ganador" "¡El greedy-bot ganó!" "assets/bot.png") ; Usar función personalizada
             (reset-game))

           ;; Verifica si hay un empate después del movimiento del greedy-bot
           (when (draw? game-grid)
             (mostrar-mensaje-simple "Empate" "¡Es un empate!" "assets/empate.png") ; Usar función personalizada
             (reset-game)))])

      ; Actualiza la pantalla para mostrar la selección actual
      (send game-frame refresh)
      (send this refresh))

    ; Forzar el foco en el canvas para la entrada del teclado
    (define/override (on-event e)
      (send this focus)) 

    ; Dibuja el canvas, con un resaltado si esta es la selección actual  
    (define/override (on-paint)
      (let ((dc (get-dc)))
        (send dc clear)
        (let-values (((x y) (send this get-size)))
          (when (and (= row current-row) (= column current-column))
            (send dc set-brush (make-object brush% (make-color 144 238 144) 'solid)) ; Color verde claro (light green) en formato RGB
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
  (cond ((equal? character #\x) (make-color 49 152 183)) ; Color del jugador en RGB
        (else (make-color 229 91 91)))) ; Color del greedy-bot en RGB


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

#|
===================================================================

Algunas funciones extras que se necesitan para el juego

===================================================================
|#

; Resetea el juego
(define (reset-game)
  (for [(canvas  canvases)]
      (send canvas set-character #\Space)
      (send canvas set-color (make-object color% "gray"))
      (send canvas refresh))
  (set! game-grid (get-matrix global-rows global-columns '_))
  (set! current-row 0) ; Reinicia la posición actual del jugador
  (set! current-column 0)
  (send game-frame refresh))

; Define el movimiento del computador
(define (set-computer-move row-and-column)
  (displayln row-and-column)
  (set! game-grid (matrix-set-at game-grid (car row-and-column) (cadr row-and-column) 'o))
  (send (list-ref canvases (+ (cadr row-and-column) (* (car row-and-column) global-columns))) set-character #\o)
  (send (list-ref canvases (+ (cadr row-and-column) (* (car row-and-column) global-columns))) refresh))

; Verifica si la celda está vacía
(define (cell-empty? row col)
  (equal? (list-ref (list-ref game-grid row) col) '_))

#|
===================================================================

Función principal

===================================================================
|#

; Ventana de entrada para seleccionar dimensiones del tablero
(define input-frame (new frame%
                      [label "Tic Tac Toe - Tamaño"]
                      [width 400]
                      [height 200]))

;; Input dimensions explanatory message
(define message (new message%
                     (parent input-frame)
                     (label "Seleccione el tamaño del tablero:")
                     [vert-margin 15]))

;; List-control to select the number of rows
(define rows-input (new choice%
                     (label "Filas             ")
                     (parent input-frame)
                     (choices (list "3" "4" "5" "6" "7" "8" "9" "10"))))

;; List-control to select the number of columns
(define columns-input (new choice%
                        (label "Columnas   ")
                        (parent input-frame)
                        (choices (list "3" "4" "5" "6" "7" "8" "9" "10"))))

;; Botón para establecer las dimensiones de la cuadrícula y comenzar a jugar
(define play-btn (new button%
                      [parent input-frame]
                      [label "Jugar"]
                      [min-width 75]
                      [min-height 50]
                      [vert-margin 15]
                      [callback (λ (b e)
                                  ; Definir el tamaño del tablero basado en la selección del usuario
                                  (define rows (string->number (send rows-input get-string-selection)))
                                  (define cols (string->number (send columns-input get-string-selection)))

                                  ; Llamar a la función TTT con las dimensiones seleccionadas
                                  (TTT rows cols))]))
;; Starts the game by showing the window to input the grid dimensions
(define (TicTacToe)
  ; Center both frames as default

(define my-logo (read-bitmap "assets/icon.png"))

(send input-frame set-icon my-logo)


  (send game-frame center 'both)
  (send input-frame center 'both)
  (send input-frame show #t))

;; Función para iniciar el juego con las dimensiones dadas
(define (TTT rows cols)
  (set! global-rows rows)
  (set! global-columns cols)

  ;; Crear la ventana de juego con la barra de menú
  (set! game-frame (new frame% [label "Tic Tac Toe"] [width 600] [height 600]))

  ;; Crear la barra de menú para la nueva ventana
  (define game-menu-bar (new menu-bar% [parent game-frame]))

  ;; Crear los menús y los elementos de menú
  (define menu-opciones (new menu% [parent game-menu-bar] [label "Opciones"]))
  (new menu-item% 
       [parent menu-opciones] 
       [label "Cambiar Tamaño"] 
       [callback (lambda (item event) (cambiar-tamano))])
  (new menu-item% 
       [parent menu-opciones] 
       [label "Información"] 
       [callback (lambda (item event) (mostrar-informacion))])
  (new menu-item% 
       [parent menu-opciones] 
       [label "Ayuda"] 
       [callback (lambda (item event) (mostrar-ayuda))])

  ;; Centrar la ventana
  (send game-frame center 'both)

  ;; Crear el grid y configurar el juego
  (set! game-grid (get-matrix global-rows global-columns '_))
  (set! main-pane (new vertical-pane% [parent game-frame] [vert-margin 5] [horiz-margin 5] [spacing 5]))
  (set! panes (get-panes 0))
  (set! canvases (get-panes-canvases panes 0))

  ;; Configurar el ícono de la ventana
  (define my-logo (read-bitmap "assets/icon.png"))
  (send game-frame set-icon my-logo)

  ;; Mostrar la ventana de juego y ocultar la ventana de entrada
  (send game-frame show #t)
  (send input-frame show #f))
