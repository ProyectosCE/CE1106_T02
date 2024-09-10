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
    ((or (winner?-v-aux1 grid 'x) (winner?-v-aux1 grid 'o)) #t) ; Verifica si hay tres 'x' o tres 'o' seguidos
    (else (winner?-v (winner?-v-aux2 grid '()))))) ; Elimina la primera columna y sigue verificando

; Verifica si hay tres elementos seguidos en la primera columna de cada fila, excluyendo '_'
(define (winner?-v-aux1 grid element)
  (define (extract-column col grid)
    (map (lambda (row) (list-ref row col)) grid))

  (define (check-columns grid)
    (let loop ((i 0))
      (cond
        ((>= i (length (car grid))) #f) ; Si se han revisado todas las columnas
        ((three-in-a-row? (extract-column i grid)) #t) ; Verifica la columna actual
        (else (loop (+ i 1))))))
  
  (check-columns grid))

; Elimina la primera columna de cada fila
(define (winner?-v-aux2 grid nGrid)
  (cond
    ((null? grid) nGrid) ; Si la cuadrícula está vacía, devuelve la nueva cuadrícula
    (else (winner?-v-aux2 (cdr grid) (cons (cdr (car grid)) nGrid))))) ; Elimina la primera columna de cada fila


; DIAGONAL check - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
