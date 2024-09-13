#lang racket

#|
================================== LICENCIA ================================================== 
MIT License
Copyright (c) 2024 José Bernardo Barquero Bonilla,
                   Jose Eduardo Campos Salazar,
                   Jimmy Feng Feng,
                   Alexander Montero Vargas
Consulta el archivo LICENSE para más detalles.
=============================================================================================
|#
#|
==================================REFERENCIAS================================================
Para este archivo game_logic.rkt se tomó como referencia general las siguientes fuentes:

  Documentación propia de racket lang: https://docs.racket-lang.org/
  Helo-Guzmán, J. E. (2001). Introducción a la programación con Scheme (2.a ed.). Editorial Tecnológica.
 ===========================================================================================
|#



;INSTRUCCIÓN PARA PODER USAR ESTE CÓDIGO EN OTROS MODULOS, COMO EL GUI
(provide (all-defined-out))

#|
=======================Funciones esenciales para el juego==================================
|#

; Crea una lista de longitud n con un valor dado
; Entradas: n (entero), value (cualquier tipo)
; Salidas: Lista de longitud n con el valor especificado
; Restricciones: n debe ser un entero mayor o igual a 0
(define (get-list n value)
  (cond ((< n 1) '())
        (else (cons value (get-list (- n 1) value)))))

; Crea una matriz de filas y columnas dadas con un valor dado
; Entradas: rows (entero), columns (entero), value (cualquier tipo)
; Salidas: Matriz representada como una lista de listas
; Restricciones: rows y columns deben ser enteros mayores o iguales a 0
(define (get-matrix rows columns value)
  (cond ((< rows 1) '())
        (else (cons (get-list columns value) (get-matrix (- rows 1) columns value)))))

; Retorna la lista con el valor establecido en el índice dado
; Entradas: list (lista), index (entero), value (cualquier tipo)
; Salidas: Lista con el valor actualizado en el índice especificado
; Restricciones: index debe ser un entero mayor o igual a 0
(define (list_set_at list index value)
  (cond ((null? list) '())
        ((equal? index 0) (cons value (list_set_at (cdr list) (- index 1) value)))
        (else (cons (car list) (list_set_at (cdr list) (- index 1) value)))))

; Función para actualizar una lista en un índice específico
; Entradas: lst (lista), index (entero), value (cualquier tipo)
; Salidas: Lista con el valor actualizado en el índice especificado
; Restricciones: index debe ser un entero mayor o igual a 0
(define (update-list lst index value)
  (append (take lst index) (list value) (drop lst (+ index 1))))

; Función para establecer un valor en la matriz
; Entradas: matrix (matriz como lista de listas), row (entero), column (entero), value (cualquier tipo)
; Salidas: Matriz con el valor actualizado en la posición especificada
; Restricciones: row y column deben ser enteros mayores o iguales a 0
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


Los metodos con un "?" se esprera retornen un #t o #f (Documentación de Racket Lang)
=============================================================================================
|#

; Función para verificar si hay un empate en el juego
; Entradas: grid (lista de listas)
; Salidas: #t si hay un empate, #f de lo contrario
; Restricciones: grid debe ser una lista de listas
(define (draw? grid)
   (cond ((null? grid) #t)
          ((draw?-aux (car grid)) #f)
          (else (draw? (cdr grid)))))

; Función auxiliar para verificar si una lista contiene '_'
; Entradas: list (lista)
; Salidas: #t si encuentra '_', #f de lo contrario
; Restricciones: list debe ser una lista
(define (draw?-aux list)
   (cond ((null? list) #f)
         ((equal? '_ (car list)) #t)
         (else (draw?-aux (cdr list)))))

; Función para verificar si la cuadrícula está vacía
; Entradas: grid (lista de listas)
; Salidas: #t si la cuadrícula está vacía, #f de lo contrario
; Restricciones: grid debe ser una lista de listas
(define (empty? grid)
   (cond ((null? grid) #t)
          ((empty?-aux (car grid)) #f)
          (else (empty? (cdr grid)))))

; Función auxiliar para verificar si una lista no contiene '_'
; Entradas: list (lista)
; Salidas: #t si no encuentra '_', #f de lo contrario
; Restricciones: list debe ser una lista
(define (empty?-aux list)
   (cond ((null? list) #f)
         ((not (equal? '_ (car list))) #t)
         (else (empty?-aux (cdr list)))))

; Verifica si hay un ganador en el juego
; Entradas: grid (lista de listas)
; Salidas: #t si hay un ganador, #f de lo contrario
; Restricciones: grid debe ser una lista de listas
(define (winner? grid)
   (cond ((or (winner?-h grid) (winner?-v grid) (winner?-d grid))
          #t)
         (else
          #f)))

; Función auxiliar para verificar si hay tres elementos seguidos en una lista, excluyendo '_'
; Entradas: lst (lista)
; Salidas: #t si hay tres elementos seguidos, #f de lo contrario
; Restricciones: lst debe ser una lista
(define (three-in-a-row? lst)
    (cond
      ((< (length lst) 3) #f) ; Menos de 3 elementos
      ((and (not (equal? (car lst) '_))
            (equal? (car lst) (cadr lst))
            (equal? (car lst) (caddr lst)))
       #t) ; Tres elementos iguales, excluyendo '_'
      (else (three-in-a-row? (cdr lst))))) ; Verifica el resto de la lista



#|
  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
Recorridos de la matriz basados en las siguientes fuentes:
Muñoz Guerrero, L. E. (2021). 
  Programación de datos con Racket. 
  Corporación Centro Internacional de Marketing Territorial para la Educación y el Desarrollo. 
  https://memoriascimted.com/wp-content/uploads/2021/04/Programaci%C3%B3n-de-datos-con-Racket.pdf
Usuario de Stack Overflow 0x2b3bfa0. (2020). 
  ¿Cómo puedo recorrer una matriz de forma diagonal en Java? 
  Stack Overflow en español. 
  https://es.stackoverflow.com/a/407672
Racket Documentation. (s.f). 
  Matrices. The Racket Language. 
  https://docs.racket-lang.org/math/matrices.html

  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
|#



; ====================================== Verificación Horizontal ======================================

; Verifica si hay tres elementos seguidos en una fila, excluyendo '_'
; Entradas: grid (lista de listas)
; Salidas: #t si hay tres elementos seguidos en alguna fila, #f de lo contrario
; Restricciones: grid debe ser una lista de listas
(define (winner?-h grid)
  (cond
    ((null? grid) #f) ; Si la cuadrícula está vacía
    ((three-in-a-row? (car grid)) #t) ; Verifica la primera fila
    (else (winner?-h (cdr grid))))) ; Verifica las demás filas

; ====================================== Verificación Vertical ======================================

; Verifica si hay tres elementos seguidos en una columna, excluyendo '_'
; Entradas: grid (lista de listas)
; Salidas: #t si hay tres elementos seguidos en alguna columna, #f de lo contrario
; Restricciones: grid debe ser una lista de listas
(define (winner?-v grid)
  (cond
    ((or (null? grid) (null? (car grid))) #f) ; Si la cuadrícula está vacía
    ((winner?-v-aux1 grid) #t) ; Verifica si hay tres 'x' o tres 'o' seguidos
    (else (winner?-v (winner?-v-aux2 grid))))) ; Elimina la primera columna y sigue verificando

; Verifica si hay tres elementos seguidos en una columna, excluyendo '_'
; Entradas: grid (lista de listas)
; Salidas: #t si hay tres elementos seguidos en alguna columna, #f de lo contrario
; Restricciones: grid debe ser una lista de listas
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
; Entradas: grid (lista de listas)
; Salidas: Nueva cuadrícula sin la primera columna de cada fila
; Restricciones: grid debe ser una lista de listas
(define (winner?-v-aux2 grid)
  (cond
    ((null? grid) '()) ; Si la cuadrícula está vacía, devuelve la nueva cuadrícula
    (else (cons (cdr (car grid)) (winner?-v-aux2 (cdr grid)))))) ; Elimina la primera columna de cada fila


; ====================================== Verificación Diagonal ======================================

; Función principal que verifica si hay un ganador en las diagonales
; Entradas: grid (lista de listas)
; Salidas: #t si hay un ganador en alguna diagonal, #f de lo contrario
; Restricciones: grid debe ser una lista de listas
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

    ; Función que revisa si una diagonal tiene al menos 3 elementos
    (define (valid-diagonal? diag)
      (>= (length diag) 3))

    ; Función recursiva para agregar las diagonales válidas
    (define (add-if-valid diag acc)
      (if (valid-diagonal? diag)
          (cons diag acc)
          acc))

    ; Función recursiva que extrae las diagonales y las agrega si son válidas
    (define (extract-diagonals-from-start x y acc)
      (cond
        [(>= x (length grid)) acc]
        [(>= y (length (car grid))) (extract-diagonals-from-start (+ x 1) 0 acc)]
        [else
         (extract-diagonals-from-start
          x (+ y 1)
          (add-if-valid (get-diagonal-down-right x y)
                        (add-if-valid (get-diagonal-down-left x y) acc)))]))

    (extract-diagonals-from-start 0 0 '()))

  (define (check-diagonals diagonals)
    (cond
      [(null? diagonals) #f]
      [(three-in-a-row? (car diagonals)) #t]
      [else (check-diagonals (cdr diagonals))]))

  (check-diagonals (extract-diagonals grid)))

#|

TEST 

(define test 
  '((x o o)
   (x x x)
   (o x o)))

(displayln (draw? test))
(displayln (winner? test))

(define test1
  '((x _ _)
    (_ x _)
    (_ _ x)))

(define test2
  '((_ _ x)
    (_ x _)
    (x _ _)))


(displayln (winner?-d test1))
(displayln (winner?-d test2))

|#

#|
=================================================================================================

Algoritmo Codicioso

Referencias:
  Shil, A. (s.f.). 
    Greedy algorithms. 
    Department of Computer Science and Engineering, University at Buffalo. 
    https://cse.buffalo.edu/~shil/courses/CSE531/Slides/Greedy-NA.pdf
  GeeksforGeeks. (s.f.). 
    Greedy algorithms. 
    https://www.geeksforgeeks.org/greedy-algorithms/
  Baeldung. (s.f.).
    Greedy algorithms in Java. 
    https://www.baeldung.com/java-greedy-algorithms


=================================================================================================
|#

; Función para obtener todas las posiciones vacías
; Entradas: matrix (matriz como lista de listas)
; Salidas: Lista de coordenadas de las posiciones vacías en la matriz
; Restricciones: matrix debe ser una lista de listas
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
; Entradas: matrix (matriz como lista de listas), row (entero), col (entero), player (símbolo)
; Salidas: #t si el movimiento lleva a una victoria, #f de lo contrario
; Restricciones: matrix debe ser una lista de listas, row y col deben ser enteros, player debe ser 'x' o 'o'
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
; Entradas: line (lista), start (entero)
; Salidas: Sublista de tres elementos consecutivos de la lista original
; Restricciones: line debe ser una lista, start debe ser un entero
(define (take-line line start)
  (if (< (length line) 3)
      '()
      (cons (car line) (take-line (cdr line) (+ start 1)))))

; Función auxiliar para obtener una línea vertical
; Entradas: matrix (matriz como lista de listas), col (entero), row (entero)
; Salidas: Lista de elementos en la columna especificada
; Restricciones: matrix debe ser una lista de listas, col y row deben ser enteros
(define (column-line matrix col row)
  (if (>= row (length matrix))
      '()
      (cons (list-ref (list-ref matrix row) col) (column-line matrix col (+ row 1)))))

; Función auxiliar para obtener la diagonal principal
; Entradas: matrix (matriz como lista de listas), length (entero), row (entero), col (entero)
; Salidas: Lista de elementos en la diagonal principal
; Restricciones: matrix debe ser una lista de listas, length, row y col deben ser enteros
(define (main-diagonal matrix length row col)
  (if (or (>= row length) (>= col length))
      '()
      (cons (list-ref (list-ref matrix row) col) 
            (main-diagonal matrix length (+ row 1) (+ col 1)))))

; Función auxiliar para obtener la diagonal inversa
; Entradas: matrix (matriz como lista de listas), length (entero), row (entero), col (entero)
; Salidas: Lista de elementos en la diagonal inversa
; Restricciones: matrix debe ser una lista de listas, length, row y col deben ser enteros
(define (anti-diagonal matrix length row col)
  (if (or (>= row length) (< col 0))
      '()
      (cons (list-ref (list-ref matrix row) col)
            (anti-diagonal matrix length (+ row 1) (- col 1)))))

; Función para seleccionar una posición aleatoria de la lista
; Entradas: positions (lista de coordenadas)
; Salidas: Coordenada seleccionada aleatoriamente
; Restricciones: positions debe ser una lista de coordenadas
(define (random-move positions)
  (if (null? positions)
      '()
      (list-ref positions (random (length positions))))) ; Selecciona una posición aleatoria

; Función principal que selecciona el mejor movimiento para el bot
; Entradas: matrix (matriz como lista de listas), player (símbolo)
; Salidas: Coordenadas del mejor movimiento
; Restricciones: matrix debe ser una lista de listas, player debe ser 'x' o 'o'
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
