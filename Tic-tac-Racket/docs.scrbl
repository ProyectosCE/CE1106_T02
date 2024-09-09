#lang scribble/manual

@(require scribble/manual)

@title{Documentación para Tic Tac Racket}

@section{Descripción General}

Este programa implementa el juego de Tic Tac Toe (Tres en Raya) usando Racket y la librería `racket/gui`. Los jugadores pueden jugar contra la computadora usando el teclado para seleccionar celdas y hacer sus movimientos. La computadora responde después de cada movimiento del jugador.

@section{Archivos Adicionales}

El programa también depende de otros tres archivos que contienen funciones auxiliares y lógicas del juego:

- @filepath{"logic-utilities.rkt"}: Utilidades para manipulación de listas y matrices.
- @filepath{"game-status.rkt"}: Funciones relacionadas con el estado del juego.
- @filepath{"try-to-win.rkt"}: Lógica del juego para que la computadora realice su movimiento.

@section{Variables Globales}

@subsection{global-rows}
Número de filas del tablero de juego. El valor inicial es @racket[3].

@subsection{global-columns}
Número de columnas del tablero de juego. El valor inicial es @racket[3].

@subsection{blue}
Color azul, utilizado para el carácter @racket{'x'} (jugador humano).

@subsection{red}
Color rojo, utilizado para el carácter @racket{'o'} (jugador computadora).

@subsection{game-grid}
Matriz que representa el estado del tablero de juego. Inicialmente está vacía y tiene dimensiones de @racket[global-rows] por @racket[global-columns].

@subsection{current-row}
La fila seleccionada actualmente en el tablero por el jugador.

@subsection{current-column}
La columna seleccionada actualmente en el tablero por el jugador.

@section{Funciones Auxiliares}

@defproc[(update-list [lst list?] [index exact-integer?] [value any/c]) list?]
Actualiza una lista en un índice específico con el valor dado. Retorna una nueva lista con el valor actualizado.

@defproc[(matrix-set-at [matrix list?] [row exact-integer?] [column exact-integer?] [value any/c]) list?]
Actualiza la matriz en la posición especificada por fila y columna con el valor dado. Utiliza la función @racket[update-list] para modificar el contenido de la fila correspondiente.

@section{Funciones en logic-utilities.rkt}

@defproc[(get-list [n exact-integer?] [value any/c]) (listof any/c)]
Crea una lista de longitud @racket[n] con el valor dado.

@defproc[(get-matrix [rows exact-integer?] [columns exact-integer?] [value any/c]) (listof (listof any/c))]
Crea una matriz de dimensiones @racket[rows] por @racket[columns] con el valor dado.

@defproc[(list-set-at [list (listof any/c)] [index exact-integer?] [value any/c]) (listof any/c)]
Devuelve una lista con el valor en el índice especificado actualizado al valor dado.

@defproc[(matrix-set-at [matrix (listof (listof any/c))] [row exact-integer?] [column exact-integer?] [value any/c]) (listof (listof any/c))]
Devuelve una matriz con el valor en la fila y columna especificada actualizado al valor dado.

@section{Funciones en game-status.rkt}

@defproc[(draw? [grid (listof (listof any/c))]) boolean?]
Determina si el juego ha terminado en empate.

@defproc[(winner? [grid (listof (listof any/c))]) boolean?]
Determina si hay un ganador en el tablero actual.

@section{Funciones en try-to-win.rkt}

@defproc[(get-list-len [currentList (listof any/c)]) exact-integer?]
Obtiene la longitud de una lista.

@defproc[(cons-at-tail [element any/c] [currentList (listof any/c)]) (listof any/c)]
Agrega un elemento al final de una lista.

@defproc[(miembro [element any/c] [elementList (listof any/c)]) boolean?]
Determina si un elemento está presente en una lista.

@defproc[(get-computer-next-move [grid (listof (listof any/c))] [numRows exact-integer?] [numColumns exact-integer?]) (listof exact-integer?)]
Obtiene el siguiente movimiento del jugador computadora en el tablero, dado el estado actual del tablero y sus dimensiones.

@defproc[(computer-counter [grid (listof (listof any/c))] [numRows exact-integer?] [numColumns exact-integer?]) (listof exact-integer?)]
Obtiene un movimiento para que la computadora evite perder (contraataque).

@section{Clases y GUI}

@subsection{Clase canvas-box%}

@defproc[(canvas-box% [parent any/c]) (is-a?/c canvas%)]
Clase que representa una celda del tablero de juego. Cada instancia de esta clase es un lienzo donde se dibuja un carácter (ya sea un espacio, 'x', o 'o') y donde se capturan los eventos del teclado.

@defproc[(on-char [e event%]) void?]
Captura eventos de teclado para navegar y seleccionar celdas en el tablero. Las teclas @racket{w}, @racket{s}, @racket{a}, y @racket{d} se usan para moverse por las celdas, y @racket{k} se usa para hacer un movimiento.

@defproc[(on-paint) void?]
Dibuja el contenido del lienzo, resaltando si esta celda está actualmente seleccionada.

@defproc[(set-color [c color%]) void?]
Establece el color de la celda (usado principalmente para resaltar la celda seleccionada).

@defproc[(set-character [char char?]) void?]
Establece el carácter que se dibuja en el lienzo. El carácter puede ser un espacio vacío, @racket{'x'}, o @racket{'o'}.

@section{Ventanas del Juego}

@defproc[(TicTacToe) void?]
Inicializa el juego mostrando primero la ventana donde se eligen las dimensiones del tablero y luego la ventana principal del juego.

@defproc[(reset-game) void?]
Reinicia el juego, limpiando el tablero y restaurando las variables globales. Resetea la matriz @racket{game-grid} y refresca la interfaz gráfica.

@defproc[(set-computer-move [row-and-column (listof exact-integer?)]) void?]
Realiza el movimiento de la computadora, actualizando la matriz @racket{game-grid} y el carácter mostrado en la celda correspondiente.

@defproc[(get-canvas-color [character char?]) color%]
Retorna el color asociado con el carácter proporcionado. Si el carácter es @racket{'x'}, retorna azul; para cualquier otro carácter, retorna rojo.

@section{Funciones Relacionadas con la Interfaz}

@defproc[(get-panes [row exact-integer?]) (listof horizontal-pane%)]
Crea un conjunto de panes horizontales para el tablero del juego, uno por cada fila.

@defproc[(get-panes-canvases [panes (listof horizontal-pane%)] [row exact-integer?]) (listof canvas-box%)]
Crea y enlaza los lienzos (@racket{canvas-box%}) a los panes proporcionados. Cada lienzo representa una celda en el tablero de juego.

@defproc[(get-pane-canvases [pane horizontal-pane%] [row exact-integer?] [column exact-integer?]) (listof canvas-box%)]
Crea y enlaza los lienzos a un solo pane horizontal. Cada lienzo corresponde a una celda en la fila.
