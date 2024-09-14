#lang scribble/manual
@(require scribble/base racket/draw)

@title{Documentación Interna del Proyecto TicTacToe - Tarea 2 - CE1106}

La presente documentación interna tiene como objetivo dar una idea de cómo funcionan todas las funciones presentes en el código del proyecto.

@section{Estructura del Proyecto}

El proyecto consta de dos módulos principales:

@itemlist[
  @item{@racketmodname[game_logic]: Módulo que contiene la lógica del juego.}
  @item{@racketmodname[gui]: Módulo que maneja la interfaz gráfica del usuario.}
]

@section{Ejemplo General de Uso}

Para iniciar el juego con un tablero de 3 filas y 4 columnas, ejecute:
@racket[(TTT 3 4)]

@section{Lógica del Juego}

@subsection{game_logic}

@defmodule["game_logic.rkt"]{
  Este módulo contiene la lógica del juego. Se encarga de manejar la lógica de detección de ganadores, empates y los movimientos del bot.
}

@subsection{Funciones del Módulo game_logic}

@defproc[(winner? [grid (listof (listof symbol?))]) boolean?]
Descripción: Verifica si hay un ganador en el tablero actual representado por @racket[grid]. Se analizan filas, columnas y diagonales para detectar un posible ganador.

Entradas: 
- @racket[grid]: Matriz bidimensional de símbolos que representa el tablero del juego.

Salidas:
- Devuelve @racket[#t] si hay un ganador.
- Devuelve @racket[#f] en caso contrario.

Restricciones: El tablero debe tener al menos 3 filas y 3 columnas para que pueda haber un ganador válido.

@defproc[(draw? [grid (listof (listof symbol?))]) boolean?]
Descripción: Determina si el juego es un empate. Un empate ocurre cuando todas las celdas están ocupadas y no hay un ganador.

Entradas: 
- @racket[grid]: Matriz bidimensional de símbolos que representa el tablero del juego.

Salidas:
- Devuelve @racket[#t] si el tablero está lleno y no hay un ganador.
- Devuelve @racket[#f] en caso contrario.

Restricciones: El tablero debe estar completamente lleno y no haber un ganador para que se declare un empate.

@defproc[(greedy-bot [grid (listof (listof symbol?))] [player symbol?]) (list integer? integer?)]
Descripción: Calcula el mejor movimiento para el bot utilizando una estrategia 'greedy' (codiciosa). Intenta ocupar la mejor posición disponible en el tablero para el jugador especificado.

Entradas: 
- @racket[grid]: Matriz bidimensional de símbolos que representa el tablero del juego.
- @racket[player]: El símbolo (@racket['X] o @racket['O]) del jugador que está jugando.

Salidas:
- Devuelve una lista con las coordenadas (fila, columna) del mejor movimiento seleccionado por el bot.

Restricciones: El bot selecciona la primera celda que maximiza sus posibilidades de ganar o evita que el jugador contrario gane.

@defproc[(get-matrix [rows integer?] [cols integer?] [init symbol?]) (listof (listof symbol?))]
Descripción: Genera una matriz de tamaño @racket[rows] x @racket[cols] inicializada con el valor @racket[init], que representa el estado inicial del tablero.

Entradas:
- @racket[rows]: Número de filas de la matriz.
- @racket[cols]: Número de columnas de la matriz.
- @racket[init]: Símbolo con el que se inicializan todas las celdas (por ejemplo, @racket['_]).

Salidas:
- Devuelve una matriz bidimensional donde cada celda contiene el valor @racket[init].

Restricciones: Las dimensiones de la matriz deben ser positivas.

@subsection{Descripción de la Lógica del Juego}

@subsubsection{Detección de Ganador}

La función @racket[winner?] es responsable de verificar si alguno de los jugadores ha ganado el juego. Se comprueban todas las filas, columnas y diagonales del tablero para ver si alguna contiene el mismo símbolo (X o O) en todas sus celdas.

@subsubsection{Detección de Empate}

La función @racket[draw?] determina si el juego ha terminado en empate. Esto sucede cuando todas las celdas del tablero están ocupadas y no hay un ganador.

@subsubsection{Movimiento del Bot}

La función @racket[greedy-bot] utiliza una estrategia codiciosa para determinar el mejor movimiento del bot. Analiza el tablero y selecciona la primera celda disponible que maximiza su probabilidad de ganar o bloquea al oponente.

@subsubsection{Generación de la Matriz}

La función @racket[get-matrix] genera una matriz que representa el estado inicial del tablero del juego, donde todas las celdas están vacías o contienen un valor inicial específico.

@section{Interfaz Gráfica (GUI)}

@defmodule["gui.rkt"]{
  Este módulo maneja la interfaz gráfica del usuario. Proporciona funciones para mostrar mensajes, manejar eventos de usuario, y actualizar la interfaz del juego.
}

@subsection{Funciones del Módulo gui}

@defproc[(TTT [rows integer?] [cols integer?]) void?]
Descripción: Inicia el juego de Tic Tac Toe con un tablero de tamaño @racket[rows] x @racket[cols]. Configura la ventana del juego, el menú y el tablero, y muestra la interfaz de usuario.

Entradas:
- @racket[rows]: Número de filas del tablero.
- @racket[cols]: Número de columnas del tablero.

Salidas: No tiene salida (es de tipo @racket[void?]).

Restricciones: El tamaño del tablero debe estar entre 3x3 y 10x10.

@defproc[(TicTacToe) void?]
Descripción: Muestra la ventana de entrada para seleccionar las dimensiones del tablero y permite al usuario comenzar el juego con las dimensiones elegidas.

Entradas: No tiene argumentos.

Salidas: No tiene salida (es de tipo @racket[void?]).

Restricciones: Ninguna.

@defproc[(mostrar-mensaje-simple [titulo string?] [texto string?] [icono path-string?]) void?]
Descripción: Muestra un mensaje corto con un botón "OK". Utilizado para notificar al usuario sobre el resultado del juego, como una victoria o empate.

Entradas:
- @racket[titulo]: Título del mensaje.
- @racket[texto]: Cuerpo del mensaje.
- @racket[icono]: Ruta al ícono que se mostrará en el mensaje.

Salidas: No tiene salida (es de tipo @racket[void?]).

Restricciones: La ruta del ícono debe ser válida.

@defproc[(mostrar-mensaje-con-editor [titulo string?] [texto string?] [icono path-string?]) void?]
Descripción: Muestra un mensaje largo utilizando un editor de texto dentro de un diálogo, con un botón "OK". Utilizado para mostrar contenido extenso, como información de ayuda o créditos del juego.

Entradas:
- @racket[titulo]: Título del mensaje.
- @racket[texto]: Cuerpo del mensaje.
- @racket[icono]: Ruta al ícono que se mostrará en el mensaje.

Salidas: No tiene salida (es de tipo @racket[void?]).

Restricciones: La ruta del ícono debe ser válida.

@defproc[(cambiar-tamano) void?]
Descripción: Permite al usuario cambiar el tamaño del tablero, mostrando nuevamente la ventana de entrada de dimensiones.

Entradas: No tiene argumentos.

Salidas: No tiene salida (es de tipo @racket[void?]).

Restricciones: Ninguna.

@subsection{Clases y Objetos en gui}

@subsection{Descripción de la Clase canvas-box%}

La clase `canvas-box%` representa una celda en el tablero del juego Tic Tac Toe. Maneja la entrada del usuario, dibuja caracteres en las celdas y controla la interacción del jugador con la celda.

@defproc[(canvas-box%) any]
Descripción: Construye un nuevo objeto de tipo `canvas-box%` para representar una celda en el tablero del juego.

Entradas:
- @racket[character] (char?): Caracter inicial mostrado en la celda (por defecto: @racket[#\space]).
- @racket[row] (integer?): Fila de la celda en el tablero (por defecto: @racket[0]).
- @racket[column] (integer?): Columna de la celda en el tablero (por defecto: @racket[0]).
- @racket[color] (color%): Color inicial del lienzo (por defecto: azul RGB, por ejemplo, @racket[(make-color 49 152 183)]).

Salidas: Devuelve una nueva instancia de `canvas-box%`.

Restricciones: Los valores de fila y columna deben ser números enteros no negativos.

@defproc[(set-color [c color%]) void?]
Descripción: Establece el color del lienzo de la celda.

Entradas:
- @racket[c]: El color a aplicar en el lienzo.

Salidas: No tiene salida (es de tipo @racket[void?]).

Restricciones: El argumento debe ser un color válido.

@defproc[(set-character [char char?]) void?]
Descripción: Establece el carácter (X o O) que se mostrará en la celda.

Entradas:
- @racket[char]: Carácter a mostrar en la celda.

Salidas: No tiene salida (es de tipo @racket[void?]).

Restricciones: El carácter debe ser una X o una O.

@subsection{Funciones Auxiliares en gui}

@defproc[(reset-game) void?]
Descripción: Reinicia el estado del juego, limpiando el tablero y restableciendo la posición inicial del jugador.

Entradas: No tiene argumentos.

Salidas: No tiene salida (es de tipo @racket[void?]).

Restricciones: Ninguna.

@defproc[(set-computer-move [row-and-column (list integer? integer?)]) void?]
Descripción: Define el movimiento del computador en el tablero. Utiliza la lógica de juego para seleccionar la mejor celda disponible para jugar.

Entradas:
- @racket[row-and-column]: Lista que contiene las coordenadas (fila, columna) del movimiento del computador.

Salidas: No tiene salida (es de tipo @racket[void?]).

Restricciones: Las coordenadas deben ser válidas y estar dentro de los límites del tablero.

@defproc[(cell-empty? [row integer?] [col integer?]) boolean?]
Descripción: Verifica si una celda específica en el tablero está vacía.

Entradas: 
- @racket[row]: Fila de la celda a verificar.
- @racket[col]: Columna de la celda a verificar.

Salidas:
- Devuelve @racket[#t] si la celda está vacía.
- Devuelve @racket[#f] en caso contrario.

Restricciones: La fila y la columna deben estar dentro de los límites del tablero.
