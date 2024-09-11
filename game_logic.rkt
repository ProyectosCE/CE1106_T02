#lang racket

(provide (all-defined-out))

#|

Funciones esenciales para el juego

|#

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

; Funcion para actualizar una lista en un índice específico
(define (update-list lst index value)
  (append (take lst index) (list value) (drop lst (+ index 1))))

; Fincion para establecer un valor en la matriz
(define (matrix-set-at matrix row column value)
  (update-list matrix row (update-list (list-ref matrix row) column value)))

#|
=============================================================================================

Condiciones de victoria del juego

draw?, función que toma una cuadrícula para analizar y devuelve verdadero si hay un empate en el juego.
empty?, función que toma una cuadrícula para analizar y devuelve verdadero si la cuadrícula está vacía.
three-in-a-row?, función auxiliar que verifica si hay tres elementos seguidos en una lista, excluyendo '_'.
winner?, función que toma una cuadrícula para analizar y devuelve verdadero si hay un ganador en el juego.
-> winner?-h, función auxiliar que verifica si hay tres elementos seguidos en una fila, excluyendo '_'.
-> winner?-v, función auxiliar que verifica si hay tres elementos seguidos en una columna, excluyendo '_'.
-> winner?-d, función auxiliar que verifica si hay un ganador en las diagonales.

=============================================================================================
|#

; Verifica si hay un empate en el juego
(define (draw? grid)
   (cond ((null? grid)
           #t)
          ((draw?-aux (car grid))
           #f)
          (else
           (draw? (cdr grid)))))

(define (draw?-aux list)
   (cond ((null? list)
          #f)
         ((equal? '_ (car list))
          #t)
         (else
          (draw?-aux (cdr list)))))

; Verifica si la matrix está vacía
(define (empty? grid)
   (cond ((null? grid)
           #t)
          ((empty?-aux (car grid))
           #f)
          (else
           (empty? (cdr grid)))))

(define (empty?-aux list)
   (cond ((null? list)
          #f)
         ((not (equal? '_ (car list)))
          #t)
         (else
          (empty?-aux (cdr list)))))

; Verifica si hay un ganador en el juego
(define (winner? grid)
   (cond ((or (winner?-h grid) (winner?-v grid) (winner?-d grid))
          #t)
         (else
          #f)))

(define (three-in-a-row? lst)
    (cond
      ((< (length lst) 3) #f) ; Menos de 3 elementos
      ((and (not (equal? (car lst) '_))
            (equal? (car lst) (cadr lst))
            (equal? (car lst) (caddr lst)))
       #t) ; Tres elementos iguales, excluyendo '_'
      (else (three-in-a-row? (cdr lst))))) ; Verifica el resto de la lista

; ====================================== Verificacion Horizontal ======================================

; Verifica si hay tres elementos seguidos en una fila, excluyendo '_'
(define (winner?-h grid)
  (cond
    ((null? grid) #f) ; Si la cuadrícula está vacía
    ((three-in-a-row? (car grid)) #t) ; Verifica la primera fila
    (else (winner?-h (cdr grid))))) ; Verifica las demás filas

; ====================================== Verificacion Vertical ======================================

; Verifica si hay tres elementos seguidos en una columna, excluyendo '_'
(define (winner?-v grid)
  (cond
    ((or (null? grid) (null? (car grid))) #f) ; Si la cuadrícula está vacía
    ((winner?-v-aux1 grid) #t) ; Verifica si hay tres 'x' o tres 'o' seguidos
    (else (winner?-v (winner?-v-aux2 grid))))) ; Elimina la primera columna y sigue verificando

; Verifica si hay tres elementos seguidos en una columna, excluyendo '_'
(define (winner?-v-aux1 grid)
  (define (extract-column grid col)
    (cond
      ((null? grid) '()) ; Si la cuadrícula está vacía
      ((null? (car grid)) '()) ; Si la fila está vacía
      (else (cons (car (car grid)) (extract-column (cdr grid) col))))) ; Extrae la columna

  (define (check-column grid col)
    (cond
      ((>= col (length (car grid))) #f) ; Si se han revisado todas las columnas
      ((three-in-a-row? (extract-column grid col)) #t) ; Verifica la columna actual
      (else (check-column grid (+ col 1))))) ; Verifica la siguiente columna

  (check-column grid 0))

; Elimina la primera columna de cada fila
(define (winner?-v-aux2 grid)
  (cond
    ((null? grid) '()) ; Si la cuadrícula está vacía, devuelve la nueva cuadrícula
    (else (cons (cdr (car grid)) (winner?-v-aux2 (cdr grid)))))) ; Elimina la primera columna de cada fila


; ====================================== Verificacion Diagonal ======================================

; Función principal que verifica si hay un ganador en las diagonales
(define (winner?-d grid)
  (define (extract-diagonals grid)
    (define (get-diagonal-down-right x y)
      (define (helper x y acc)
        (cond
          [(or (>= x (length grid)) (>= y (length (car grid)))) (reverse acc)]
          [else (helper (+ x 1) (+ y 1) (cons (list-ref (list-ref grid x) y) acc))]))
      (helper x y '()))

    (define (get-diagonal-down-left x y)
      (define (helper x y acc)
        (cond
          [(or (>= x (length grid)) (< y 0)) (reverse acc)]
          [else (helper (+ x 1) (- y 1) (cons (list-ref (list-ref grid x) y) acc))]))
      (helper x y '()))

    (define (is-valid-diagonal diag)
      (>= (length diag) 3))
    
    (define (extract-all-diagonals)
      (define (extract-diagonals-from-start x y)
        (filter is-valid-diagonal
                (list (get-diagonal-down-right x y)
                      (get-diagonal-down-left x y))))
    
      (define (helper x y acc)
        (cond
          [(>= x (length grid)) acc]
          [(>= y (length (car grid))) (helper (+ x 1) 0 acc)]
          [else (helper x (+ y 1) (append acc (extract-diagonals-from-start x y)))]))
    
      (helper 0 0 '()))

    (extract-all-diagonals))

  (define (check-diagonals diagonals)
    (cond
      ((null? diagonals) #f)
      ((three-in-a-row? (car diagonals)) #t)
      (else (check-diagonals (cdr diagonals)))))

  (check-diagonals (extract-diagonals grid)))


#| 
================================================================================================= 

Algortimo Codicioso

=================================================================================================
|# 

; Función para obtener todas las posiciones vacías
; 1. Conjunto de Candidatos: Obtiene todas las posiciones vacías en la matriz donde el bot puede realizar un movimiento.
(define (empty-positions matrix)
  (define (empty-positions-row row r-index)
    (define (check-pos lst col-index)
      (cond ((null? lst) '())
            ((equal? (car lst) '_)
             (cons (list r-index col-index)
                   (check-pos (cdr lst) (+ col-index 1))))
            (else (check-pos (cdr lst) (+ col-index 1))))) ; No está vacío, avanzar
    (check-pos row 0))

  (define (check-rows mat r-index)
    (cond ((null? mat) '())
          (else (append (empty-positions-row (car mat) r-index)
                        (check-rows (cdr mat) (+ r-index 1)))))) ; Buscar en cada fila
  (check-rows matrix 0))

; Función para verificar si un movimiento lleva a una victoria
; 4. Función Objetivo: Asigna un valor a una solución o solución parcial, indicando su calidad.
(define (winning-move? matrix row col player)
  (define (check-line line player)
    (cond ((< (length line) 3) #f)
          ((and (equal? (car line) player)
                (equal? (cadr line) player)
                (equal? (caddr line) player))
           #t)
          (else (check-line (cdr line) player))))

  ; Chequea fila
  (define (check-horizontal)
    (check-line (take-line (list-ref matrix row) 0) player))
  
  ; Chequea columna
  (define (check-vertical)
    (check-line (column-line matrix col 0) player))
  
  ; Chequea diagonales
  (define (check-diagonals)
    (or (check-line (main-diagonal matrix (min (length matrix) (length (car matrix))) 0 0) player)
        (check-line (anti-diagonal matrix (min (length matrix) (length (car matrix))) 0 (- (length (car matrix)) 1)) player)))
  
  (or (check-horizontal) (check-vertical) (check-diagonals)))

; Función auxiliar para obtener una línea horizontal
; 5. Función de Solución: Define cuándo se ha alcanzado una solución completa, en este caso, cuando se selecciona un movimiento que lleva a la victoria.
(define (take-line line start)
  (if (< (length line) 3)
      '()
      (cons (car line) (take-line (cdr line) (+ start 1)))))

; Función auxiliar para obtener una línea vertical
; 5. Función de Solución: Define cuándo se ha alcanzado una solución completa, en este caso, cuando se selecciona un movimiento que lleva a la victoria.
(define (column-line matrix col row)
  (if (>= row (length matrix))
      '()
      (cons (list-ref (list-ref matrix row) col) (column-line matrix col (+ row 1)))))

; Función auxiliar para obtener la diagonal principal
; 5. Función de Solución: Define cuándo se ha alcanzado una solución completa, en este caso, cuando se selecciona un movimiento que lleva a la victoria.
(define (main-diagonal matrix length row col)
  (if (or (>= row length) (>= col length))
      '()
      (cons (list-ref (list-ref matrix row) col) 
            (main-diagonal matrix length (+ row 1) (+ col 1)))))

; Función auxiliar para obtener la diagonal inversa
; 5. Función de Solución: Define cuándo se ha alcanzado una solución completa, en este caso, cuando se selecciona un movimiento que lleva a la victoria.
(define (anti-diagonal matrix length row col)
  (if (or (>= row length) (< col 0))
      '()
      (cons (list-ref (list-ref matrix row) col)
            (anti-diagonal matrix length (+ row 1) (- col 1)))))


; Función para seleccionar una posición aleatoria de la lista
; 2. Función de Selección: Selecciona un movimiento aleatorio de la lista de posiciones vacías si no se encuentran movimientos ganadores ni de bloqueo.
(define (random-move positions)
  (if (null? positions)
      '()
      (list-ref positions (random (length positions))))) ; Selecciona una posición aleatoria

; Función principal que selecciona el mejor movimiento para el bot
(define (greedy-bot matrix player)
  (define opponent (if (equal? player 'o) 'x 'o)) ; Determina el oponente
  
  ; Evaluar si el bot puede ganar (Función de Selección)
  (define (find-winning-move positions)
    (cond
      ((null? positions) '()) ; No hay más posiciones para evaluar
      (else
       (define pos (car positions))
       (define row (car pos))
       (define col (cadr pos))
       (if (winning-move? (matrix-set-at matrix row col player) row col player) ; Si es un movimiento ganador
           (list row col) ; Retorna el movimiento ganador
           (find-winning-move (cdr positions)))))) ; Evaluar siguiente candidato
  
  ; Evaluar si el oponente puede ganar y necesita ser bloqueado (Función de Selección)
  (define (find-blocking-move positions)
    (cond
      ((null? positions) '()) ; No hay más posiciones para evaluar
      (else
       (define pos (car positions))
       (define row (car pos))
       (define col (cadr pos))
       (if (winning-move? (matrix-set-at matrix row col opponent) row col opponent) ; Si es un movimiento del oponente que necesita bloqueo
           (list row col) ; Retorna el movimiento para bloquear
           (find-blocking-move (cdr positions)))))) ; Evaluar siguiente candidato
  
  (define positions (empty-positions matrix)) ; Obtiene todas las posiciones vacías (Conjunto de Candidatos)
  
  ; Selecciona un movimiento aleatorio si es el primer movimiento (Función de Solución)
  (if (null? (find-winning-move positions))
      (if (null? (find-blocking-move positions))
          (random-move positions) ; Primer movimiento vacío
          (find-blocking-move positions)) ; Bloqueo
      (find-winning-move positions))) ; Movimiento ganador