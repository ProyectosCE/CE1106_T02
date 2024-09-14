#lang scribble/manual
@(require scribble/base racket/draw)

@title{Documentación del Proyecto Tic Tac Toe}

@section{Estructura del Proyecto}

El proyecto consta de dos módulos principales:

@itemlist[
  @item{@racketmodname[game_logic]: Módulo que contiene la lógica del juego.}
  @item{@racketmodname[gui]: Módulo que maneja la interfaz gráfica del usuario.}
]

@section{Ejemplo General de Uso}

Para iniciar el juego con un tablero de 3 filas y 4 columnas, ejecute:
@racket[(TTT 3 4)]

@section{Módulos}

@subsection{game_logic}

@defmodule["game_logic.rkt"]{
  Este módulo contiene la lógica del juego. Se encarga de manejar la lógica de detección de ganadores, empates y los movimientos del bot.
}

@subsection{Funciones del Módulo game_logic}

@defproc[(winner? [grid (listof (listof symbol?))]) boolean?]
{Verifica si hay un ganador en el tablero actual representado por @racket[grid].
Devuelve @racket[#t] si hay un ganador, y @racket[#f] en caso contrario.}

@defproc[(draw? [grid (listof (listof symbol?))]) boolean?]
{Determina si el juego es un empate. Un empate ocurre cuando todas las celdas están ocupadas y no hay un ganador.
Devuelve @racket[#t] si es un empate, y @racket[#f] en caso contrario.}

@defproc[(greedy-bot [grid (listof (listof symbol?))] [player symbol?]) (list integer? integer?)]
{Calcula el mejor movimiento para el bot utilizando una estrategia de 'greedy' (codiciosa).
El bot intenta ocupar la mejor posición disponible en el tablero para el jugador @racket[player].
Devuelve una lista con las coordenadas (fila, columna) del movimiento seleccionado.}

@defproc[(get-matrix [rows integer?] [cols integer?] [init symbol?]) (listof (listof symbol?))]
{Genera una matriz de tamaño @racket[rows] x @racket[cols] inicializada con el valor @racket[init].
Esta matriz representa el estado del tablero del juego.}

@subsection{Descripción de la Lógica del Juego}

@subsubsection{Detección de Ganador}

La función @racket[winner?] es responsable de verificar si alguno de los jugadores ha ganado el juego. Se comprueban todas las filas, columnas y diagonales del tablero para ver si alguna contiene el mismo símbolo (X o O) en todas sus celdas.

@subsubsection{Detección de Empate}

La función @racket[draw?] determina si el juego ha terminado en empate. Esto sucede cuando todas las celdas del tablero están ocupadas y no hay un ganador.

@subsubsection{Movimiento del Bot}

La función @racket[greedy-bot] utiliza una estrategia codiciosa para determinar el mejor movimiento del bot. Analiza el tablero y selecciona la primera celda disponible que maximiza su probabilidad de ganar o bloquea al oponente.

@subsubsection{Generación de la Matriz}

La función @racket[get-matrix] genera una matriz que representa el estado inicial del tablero del juego, donde todas las celdas están vacías o contienen un valor inicial específico.

@section{gui}

@defmodule["gui.rkt"]{
  Este módulo maneja la interfaz gráfica del usuario. Proporciona funciones para mostrar mensajes, manejar eventos de usuario, y actualizar la interfaz del juego.
}

@subsection{Funciones del Módulo gui}

@defproc[(TTT [rows integer?] [cols integer?]) void?]
{Inicia el juego de Tic Tac Toe con un tablero de tamaño @racket[rows] x @racket[cols].
Configura la ventana del juego, el menú y el tablero, y muestra la interfaz de usuario.}

@defproc[(TicTacToe) void?]
{Muestra la ventana de entrada para seleccionar las dimensiones del tablero y permite al usuario comenzar el juego con las dimensiones elegidas.
}

@defproc[(mostrar-mensaje-simple [titulo string?] [texto string?] [icono path-string?]) void?]
{Muestra un mensaje corto con un botón "OK". Utilizado para notificar al usuario sobre el resultado del juego, como una victoria o empate.}

@defproc[(mostrar-mensaje-con-editor [titulo string?] [texto string?] [icono path-string?]) void?]
{Muestra un mensaje largo utilizando un editor de texto dentro de un diálogo, con un botón "OK". Utilizado para mostrar contenido extenso, como información de ayuda o créditos del juego.}

@defproc[(cambiar-tamano) void?]
{Permite al usuario cambiar el tamaño del tablero, mostrando nuevamente la ventana de entrada de dimensiones.}

@subsection{Clases y Objetos en gui}

@subsection{Descripción de la Clase canvas-box%}

La clase `canvas-box%` representa una celda en el tablero del juego Tic Tac Toe. Maneja la entrada del usuario, dibuja caracteres en las celdas y controla la interacción del jugador con la celda.

@defproc[(canvas-box%) any]
{Construye un nuevo objeto de tipo `canvas-box%`.

  **Argumentos:**
  - @racket[character] (char?): Caracter inicial mostrado en la celda (por defecto: @racket[#\space]).
  - @racket[row] (integer?): Fila de la celda en el tablero (por defecto: @racket[0]).
  - @racket[column] (integer?): Columna de la celda en el tablero (por defecto: @racket[0]).
  - @racket[color] (color%): Color inicial del lienzo (por defecto: azul RGB, por ejemplo, @racket[(make-color 49 152 183)]).
}

@defproc[(set-color [c color%]) void?]
{Establece el color del lienzo.}

@defproc[(set-character [char char?]) void?]
{Establece el carácter (X o O) que se mostrará en la celda.}

@subsection{Funciones Auxiliares en gui}

@defproc[(reset-game) void?]
{Reinicia el estado del juego, limpiando el tablero y restableciendo la posición inicial del jugador.}

@defproc[(set-computer-move [row-and-column (list integer? integer?)]) void?]
{Define el movimiento del computador en el tablero. Utiliza la lógica de juego para seleccionar la mejor celda disponible para jugar.}

@defproc[(cell-empty? [row integer?] [col integer?]) boolean?]
{Verifica si una celda específica en el tablero está vacía.}
