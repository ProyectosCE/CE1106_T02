#lang racket

(require "logic-utilities.rkt")


;;Loop for finding a list length
(define (get-list-len-aux currentList currentLength)
  (cond
    ((null? currentList) currentLength)
    (else (get-list-len-aux (cdr currentList) (+ currentLength 1)))))

;;Gets the length of a list
(define (get-list-len currentList)
  (get-list-len-aux currentList 0))

;;Adds element to the back of the list instead of the beginning
(define (cons-at-tail element currentList)
  (cond
    ((null? currentList) (list element))
    (else (cons (car currentList) (cons-at-tail element (cdr currentList)))))) 

;;Finds if an element is a member of a list
(define (miembro element elementList)
  (cond ((null? elementList) #f)
        ((equal? element (car elementList)) #t)
        (else (miembro element (cdr elementList)))))

;;Gets all the symbol of a row with their n and m coordinates
(define (get-symbol-from-row rowList currentRow currentColumn symbol currentPairs prevOriginList)
  (cond
    ((null? rowList) currentPairs)
    ((equal? (car rowList) '_) (get-symbol-from-row (cdr rowList) currentRow (+ currentColumn 1) symbol currentPairs prevOriginList))
    ((miembro (list currentRow currentColumn) prevOriginList) (get-symbol-from-row (cdr rowList) currentRow (+ currentColumn 1) symbol currentPairs prevOriginList))
    ((equal? (car rowList) symbol) (get-symbol-from-row (cdr rowList) currentRow (+ currentColumn 1) symbol (cons-at-tail (list currentRow currentColumn) currentPairs) prevOriginList))
    (else (get-symbol-from-row (cdr rowList) currentRow (+ currentColumn 1) symbol currentPairs prevOriginList))))

;;Loop for finding all possible pairs for a point given said point as input
(define (get-pairs-aux grid currentRow symbol currentPairs prevOriginList)
  (cond
    ((null? grid) (list currentPairs prevOriginList))
    (else (get-pairs-aux (cdr grid) (+ currentRow 1) symbol (get-symbol-from-row (car grid) (+ currentRow 1) 0 symbol currentPairs prevOriginList) prevOriginList))))

(define (get-pairs-without-origin grid symbol)
  (get-pairs-aux grid -1 symbol '() '()))


;;Finds all possible pairs of a point in a grid
(define (get-pairs grid origin symbol prevOriginList)
  (get-pairs-aux grid -1 symbol '() (cons origin prevOriginList)))


;;Loop for get-value-in-list-by-index function which searches within a list with a given index
(define (get-value-in-list-by-index-aux lista index current)
  (cond
    ((null? lista) -1)
    ((= index current) (car lista))
    (else (get-value-in-list-by-index-aux (cdr lista) index (+ current 1)))))

;;Gets a value within a list given an index
(define (get-value-in-list-by-index lista index)
  (get-value-in-list-by-index-aux lista index 0))
  

;;Loop for getting value within a matrix based on an index
(define (get-value-base-on-index-aux matrix targetRow targetColumn currentRow currentAnswer)
  (cond
    ((null? matrix) -1)
    ((not (equal? currentAnswer -1)) currentAnswer)
    ((= targetRow currentRow) (get-value-in-list-by-index (car matrix) targetColumn))
    (else (get-value-base-on-index-aux (cdr matrix) targetRow targetColumn (+ currentRow 1) -1))))

;;Gets the value on an index in a given matrix
(define (get-value-base-on-index matrix row column)
  (get-value-base-on-index-aux matrix row column 0 -1))
  


;;Verifies that the line is a guaranteed win or cut off, in case of aggresive play it plays anyway
(define (verify-single-solution resultList isAggresive)
  (cond
    ((null? resultList) -1)
    ((equal? isAggresive #t) (car resultList)) 
    ((= 1 (get-list-len resultList)) (car resultList))
    (else -1)))

;;Loop for placing in the next diagonal possible
(define (place-next-diagonal-aux grid numColumns currentRow currentColumn symbol otherSymbol resultList isAggresive)
  (cond
    ((>= currentColumn numColumns) (verify-single-solution resultList isAggresive))
    ((equal? (get-value-base-on-index grid currentRow currentColumn)  '_)
     (place-next-diagonal-aux grid numColumns (+ currentRow 1) (+ currentColumn 1) symbol otherSymbol (cons-at-tail (list currentRow currentColumn) resultList) isAggresive))
    ((equal? (get-value-base-on-index grid currentRow currentColumn)  otherSymbol) -1)
    (else (place-next-diagonal-aux grid numColumns (+ currentRow 1) (+ currentColumn 1) symbol otherSymbol resultList isAggresive))))

;;Places in the next diagonal possible
(define (place-next-diagonal grid numColunms startingPoint symbol otherSymbol isAggresive)
  (place-next-diagonal-aux grid numColunms (car startingPoint) (cadr startingPoint) symbol otherSymbol '() isAggresive))

;;Loop for placing in the next antidiagonal possible
(define (place-next-antidiagonal-aux grid numColumns currentRow currentColumn symbol otherSymbol resultList isAggresive)
  (cond
    ((<= currentColumn 0) (verify-single-solution resultList isAggresive))
    ((equal? (get-value-base-on-index grid currentRow currentColumn)  '_)
     (place-next-antidiagonal-aux grid numColumns (+ currentRow 1) (- currentColumn 1) symbol otherSymbol (cons-at-tail (list currentRow currentColumn) resultList) isAggresive))
    ((equal? (get-value-base-on-index grid currentRow currentColumn)  otherSymbol) -1)
    (else (place-next-antidiagonal-aux grid numColumns (+ currentRow 1) (- currentColumn 1) symbol otherSymbol resultList isAggresive))))

;;Places in the next antidiagonal possible
(define (place-next-antidiagonal grid numColumns startingPoint symbol otherSymbol isAggresive)
  (place-next-antidiagonal-aux grid numColumns (car startingPoint) (cadr startingPoint) symbol otherSymbol '() isAggresive))

;;Gets the starting point for a diagonal
(define (get-diagonal-starting-point start)
  (cond
    ((zero? (cadr start)) start)
    ((zero? (car start)) start)
    (else (get-diagonal-starting-point (list (- (car start) 1) (- (cadr start) 1))))))

;;Gets the starting point for an antidiagonal
(define (get-antidiagonal-starting-point numColumns start)
  (cond
    ((= (cadr start) (- numColumns 1)) start)
    ((zero? (car start)) start)
    (else (get-antidiagonal-starting-point numColumns (list (- (car start) 1) (+ (cadr start) 1))))))

;;Analyzes the grid for diagonals and antidiagonals
(define (analize-diagonal grid numColumns symbol start finish isAggresive)
  (cond
    ((= (- (car start) (car finish)) (- (cadr start) (cadr finish))) (place-next-diagonal grid numColumns (get-diagonal-starting-point start) symbol (get-other-symbol symbol) isAggresive))
    ((= (- (car start) (car finish)) (- (cadr finish) (cadr start))) (place-next-antidiagonal grid numColumns (get-antidiagonal-starting-point numColumns start) symbol (get-other-symbol symbol) isAggresive))
    (else -1)))


;;Loop for detecting the possibilty of horizontal lines within the grid
(define (place-next-n-aux grid numColumns n symbol otherSymbol currentM resultList isAggresive)
  (cond
    ((>= currentM numColumns) (verify-single-solution resultList isAggresive))
    ((equal? (get-value-base-on-index grid n currentM)  '_) (place-next-n-aux grid numColumns n symbol otherSymbol (+ currentM 1) (cons-at-tail (list n currentM) resultList) isAggresive))
    ((equal? (get-value-base-on-index grid n currentM) otherSymbol) -1)
    (else (place-next-n-aux grid numColumns n symbol otherSymbol (+ currentM 1) resultList isAggresive))))

;;Gets the n and m coordinates for placing a symbol to complete a horizontal line
(define (place-next-n grid numColumns n symbol otherSymbol isAggresive)
  (place-next-n-aux grid numColumns n symbol otherSymbol 0 '() isAggresive))

;;Loop for detecting the possibility of vertical lines within the grid
(define (place-next-m-aux grid numRows m symbol otherSymbol currentN resultList isAggresive)
  (cond
    ((>= currentN numRows) (verify-single-solution resultList isAggresive))
    ((equal? (get-value-base-on-index grid currentN m) '_) (place-next-m-aux grid numRows m symbol otherSymbol (+ currentN 1) (cons-at-tail (list currentN m) resultList) isAggresive))
    ((equal? (get-value-base-on-index grid currentN m) otherSymbol) -1)
    (else (place-next-m-aux grid numRows m symbol otherSymbol (+ currentN 1) resultList isAggresive))))

;;Gets the n and m coordinates for placing a symbol to complete a vertical line
(define (place-next-m grid numRows m symbol otherSymbol isAggresive)
  (place-next-m-aux grid numRows m symbol otherSymbol 0 '() isAggresive))
  

;;Gets the opposite symbol
(define (get-other-symbol symbol)
  (cond
    ((equal? symbol 'x) 'o)
    ((equal? symbol 'o) 'x)))

;;Creates a antidiagonal line based on a starting point
(define (create-a-antidiagonal-line grid numColumns symbol otherSymbol start currentPos)
  (cond
    ((<= (cadr currentPos) 0) -1)
    ((equal? (get-value-base-on-index grid (car currentPos) (cadr currentPos)) '_) currentPos)
    ((equal? (get-value-base-on-index grid (car currentPos) (cadr currentPos)) otherSymbol) -1)
    (else (create-a-antidiagonal-line numColumns symbol otherSymbol start (list (+ (car currentPos) 1) (- (cadr currentPos) 1))))))

;;Creates a diagonal line based on a starting point
(define (create-a-diagonal-line grid numColumns symbol otherSymbol start currentPos)
  (cond
    ((>= (cadr currentPos) numColumns) (create-a-antidiagonal-line grid numColumns symbol otherSymbol start (get-antidiagonal-starting-point numColumns start)))
    ((equal? (get-value-base-on-index grid (car currentPos) (cadr currentPos)) '_) currentPos)
    ((equal? (get-value-base-on-index grid (car currentPos) (cadr currentPos)) otherSymbol) (create-a-antidiagonal-line grid numColumns symbol otherSymbol start (get-antidiagonal-starting-point numColumns start)))
    (else (create-a-diagonal-line grid numColumns symbol otherSymbol start (list (+ (car currentPos) 1) (+ (cadr currentPos) 1))))))

;;Creates a vertical line based on a starting point
(define (create-a-vertical-line grid numRows numColumns symbol otherSymbol start currentPos)
  (cond
    ((>= (car currentPos) numRows) (create-a-diagonal-line grid numColumns symbol otherSymbol start (get-diagonal-starting-point start)))
    ((equal? (get-value-base-on-index grid (car currentPos) (cadr currentPos)) '_) currentPos)
    ((equal? (get-value-base-on-index grid (car currentPos) (cadr currentPos)) otherSymbol) (create-a-diagonal-line grid numColumns symbol otherSymbol start (get-diagonal-starting-point start)))
    (else (create-a-vertical-line grid numRows numColumns symbol otherSymbol start (list (+ (car currentPos) 1) (cadr currentPos))))))
  

;;Gets the starting position for searching a vertical line
(define (get-vertical-starting-point start)
  (cond
    ((zero? (car start)) start)
    (else (get-vertical-starting-point (list (- (car start) 1) (cadr start))))))

;;Creates a horizontal line based on a starting point
(define (create-a-horizontal-line grid numRows numColumns symbol otherSymbol start currentPos)
  (cond
    ((>= (cadr currentPos) numColumns) (create-a-vertical-line grid numRows numColumns symbol otherSymbol start (get-vertical-starting-point start)))
    ((equal? (get-value-base-on-index grid (car currentPos) (cadr currentPos)) '_) currentPos)
    ((equal? (get-value-base-on-index grid (car currentPos) (cadr currentPos)) otherSymbol) (create-a-vertical-line grid numRows numColumns symbol otherSymbol start (get-vertical-starting-point start)))
    (else (create-a-horizontal-line grid numRows numColumns symbol otherSymbol start (list (car currentPos) (+ (cadr currentPos) 1))))))
    

;;Gets the starting position for searching a horizontal line
(define (get-horizontal-starting-point start)
  (cond
    ((zero? (cadr start)) start)
    (else (get-horizontal-starting-point (list (car start) (- (cadr start) 1))))))

;;Creates a solution based on a single point in the grid
(define (create-a-solution grid numRows numColumns symbol otherSymbol start)
  (create-a-horizontal-line grid numRows numColumns symbol otherSymbol start (get-horizontal-starting-point start)))  

;;Gets the n and m coordinates for a given move by the computer
(define (get-move grid numRows numColumns symbol start finish isAggresive)
  (cond
    ((null? finish) (create-a-solution grid numRows numColumns symbol (get-other-symbol symbol) start))
    ((= (car start) (car finish)) (place-next-n grid numColumns (car start) symbol (get-other-symbol symbol) isAggresive))
    ((= (cadr start) (cadr finish)) (place-next-m grid numRows (cadr start) symbol (get-other-symbol symbol) isAggresive))
    (else (analize-diagonal grid numColumns symbol start finish isAggresive))))

;;Helper function for storing values in recursive calls for get-solution-for-symbol
(define (get-solution-for-symbol-aux-helper grid numRows numColumns symbol origin possiblePairsList lastResult isAggresive)
  (cond
    ((and (null? possiblePairsList) (equal? -1 lastResult)) -1)
    ((not (equal? -1 lastResult)) lastResult)
    (else (get-solution-for-symbol-aux-helper grid numRows numColumns symbol origin (cdr possiblePairsList) (get-move grid numRows numColumns symbol origin (car possiblePairsList) isAggresive) isAggresive))))
    
  

;;Helper function for storing values in recursive calls for get-solution-for-symbol
(define (get-solution-for-symbol-mid-helper grid  numRows numColumns symbol originList  pairsInfo isAggresive)
  (get-solution-for-symbol-aux grid numRows numColumns symbol (cdr originList) (cons (car originList) (cadr pairsInfo))
                                (get-solution-for-symbol-aux-helper grid numRows numColumns symbol (car originList) (car (get-pairs grid (car originList) symbol (cons (car originList) (cadr pairsInfo)))) -1 isAggresive) isAggresive))


;;Loop for finding solution to the grid given an specific symbol
(define (get-solution-for-symbol-aux grid numRows numColumns symbol originList prevOriginList lastResult isAggresive)
  (cond
    ((null? originList) -1)
    ((not (equal? -1 lastResult)) lastResult)
    (else (get-solution-for-symbol-mid-helper grid numRows numColumns symbol originList (get-pairs grid (car originList) symbol prevOriginList) isAggresive))))
  


;;Finds the solution to a grid given itself, the number of rows and columns and the symbol
(define (get-solution-for-symbol grid numRows numColumns symbol isAggresive)
  (get-solution-for-symbol-aux grid numRows numColumns symbol (car (get-pairs-without-origin grid symbol)) '() -1 isAggresive))

;;Loops that finds an open position within the grid
(define (computer-random-attack-aux grid numRows numColumns result)
  (cond
    ((equal? (get-value-base-on-index grid (car result) (cadr result)) '_) result)
    (else (computer-random-attack grid numRows numColumns))))

;;Randomizes a response by the computer
(define (computer-random-attack grid numRows numColumns)
  (computer-random-attack-aux grid numRows numColumns (list (random numRows) (random numColumns))))

;;Loop for finding a winning condition
(define (computer-find-solution-aux grid numRows numColumns result)
  (cond
    ((equal? -1 result) (computer-random-attack grid numRows numColumns))
    (else result)))

;;Finds the closest thing to approaching a winning condition
(define (computer-find-solution grid numRows numColumns)
  (cond
    ((null? (car (get-pairs-without-origin grid 'o))) (computer-random-attack grid numRows numColumns))
    (else (computer-find-solution-aux grid numRows numColumns (get-move grid numRows numColumns 'o (car (car (get-pairs-without-origin grid 'o))) '() #f)))))

;;Loop that determines an aggresive approach to the grid state
(define (computer-aggresive-attack-aux grid numRows numColumns result)
  (cond
    ((equal? -1 result) (computer-find-solution grid numRows numColumns))
    (else result)))

;;Finds the closest possible solution
(define (computer-aggresive-attack grid numRows numColumns)
  (computer-aggresive-attack-aux grid numRows numColumns (get-solution-for-symbol grid numRows numColumns 'o #t)))
  

;;Determines whether or not an attack is possible
(define (computer-attack-aux grid numRows numColumns result)
  (cond
    ((equal? -1 result) (computer-counter grid numRows numColumns))
    (else result)))

;;Gets a attack move by the computer
(define (computer-attack grid numRows numColumns)
  (computer-attack-aux grid numRows numColumns (get-solution-for-symbol grid numRows numColumns 'o #f)))

;;Loop that determines whether or not computer is losing
(define (computer-counter-aux grid numRows numColumns result)
  (cond
    ((equal? -1 result) (computer-aggresive-attack grid numRows numColumns))
    (else result)))

;;Gets a counter move by the computer to avoid losing
(define (computer-counter grid numRows numColumns)
  (computer-counter-aux grid numRows numColumns (get-solution-for-symbol grid numRows numColumns 'x #f)))
  
  

;;Gets the next possible state of the grid
(define (get-computer-next-move grid numRows numColumns)
  (computer-attack grid numRows numColumns))
  


;;Test cases
;;(get-pairs-without-origin (list (list 'x '_ '_) (list 'x '_ '_) (list '_ '_ '_)) 'x)
;;Testing for diagonals
;;(get-computer-next-move (list (list 'x '_ '_ '_) (list '_ 'x '_ '_) (list '_ '_ 'x '_) (list '_ '_ '_ '_)) 4 4)
;;(get-computer-next-move (list (list '_ '_ '_ 'x) (list '_ '_ 'x '_) (list '_ 'x '_ '_) (list '_ '_ '_ '_)) 4 4)
;;((_ o x) (x x o) (_ _ _))
;;(get-computer-next-move (list (list '_ 'o 'x) (list 'x 'x 'o) (list '_ '_ '_)) 3 3)
;;(get-computer-next-move (list (list '_ 'o 'x '_) (list 'x 'x 'o '_) (list '_ '_ '_ '_) (list '_ '_ '_ '_)) 4 4)
;;((x o x o o) (o x o _ x) (x o x x x) (_ _ x o x) (o o x _ o))
;;(get-computer-next-move (list (list 'x 'o 'x 'o 'o) (list 'o 'x 'o '_ 'x) (list 'x 'o 'x 'x 'x) (list '_ '_ 'x 'o 'x) (list 'o 'o 'x '_ 'o)) 5 5)
;;((_ _ _) (o x _) (x _ o))
;;(get-computer-next-move (list (list '_ '_ '_) (list 'o 'x '_) (list 'x '_ 'o)) 3 3)
;;(get-computer-next-move (list (list '_ '_ '_) (list 'o 'x '_) (list 'o '_ 'x)) 3 3)
;;((o _ _ _) (_ o x _) (_ _ _ x) (_ _ _ _))
;;(get-computer-next-move (list (list '_ '_ '_ '_) (list '_ 'o '_ '_) (list '_ '_ '_ 'x) (list '_ '_ '_ '_)) 4 4)
;;(get-computer-next-move (list (list 'x '_ '_) (list '_ '_ '_) (list '_ '_ '_)) 3 3)
;;((o o o x o) (x o o x _) (_ x x x _) (x x _ x _) (o x o o _))
;;(get-computer-next-move (list (list 'o 'o 'o 'x 'o) (list 'x 'o 'o 'x '_) (list '_ 'x 'x 'x '_) (list 'x 'x '_ 'x '_) (list 'o 'x 'o 'o '_)) 5 5)


(provide (all-defined-out))