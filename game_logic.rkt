#lang racket

(provide (all-defined-out))

#|
=============================================================================================

Condiciones de victoria del juego

draw?, función que toma una cuadrícula para analizar y devuelve verdadero si hay un empate en el juego.
empty?, función que toma una cuadrícula para analizar y devuelve verdadero si la cuadrícula está vacía.

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
   (cond ((or (winner?-h grid) (winner?-v grid)) ;(winner?-d grid)) 
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

; HORIZONTAL check - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; Verifica si hay tres elementos seguidos en una fila, excluyendo '_'
(define (winner?-h grid)
  (cond
    ((null? grid) #f) ; Si la cuadrícula está vacía
    ((three-in-a-row? (car grid)) #t) ; Verifica la primera fila
    (else (winner?-h (cdr grid))))) ; Verifica las demás filas

; VERTICAL check - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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


; DIAGONAL check - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
