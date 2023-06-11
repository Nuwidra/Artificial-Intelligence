#lang racket/gui
;; ======================================================================
;; Instituto Tecnológico de Costa Rica
;; Sede Regional de Alajuela
;; ======================================================================
;; MinMax Dara Game
;; Proyecto 1
;; ======================================================================
;; Autores:  Jonathan Quesada Salas
;;           Rodolfo Cruz Vega
;; ======================================================================
;; Descripción: El juego Nigeriano de Dara es un juego de estrategia
;; popular en África occidental y central. El objetivo del juego es
;; capturar las fichas del oponente y obtener más fichas que el oponente
;; al final del juego. El juego se juega en un tablero de juego con dos
;; filas de 12 agujeros y dos agujeros grandes en ambos extremos,
;; llamados "casas". Cada jugador comienza con 24 fichas, y las fichas
;; se colocan en cada agujero, excepto en las casas.
;; ======================================================================

;; ======================================================================
;; Bibliotecas necesarias para el funcionamiento del programa
;; ======================================================================
(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)
(require br/cond)
(require racket/base)
(require table-panel)
(require racket/dict)
(require racket/list)

;; ======================================================================
;; Constantes del programa
;; ======================================================================
(define BOARD-ROWS 5) ;; Numero de filas del tablero
(define BOARD-COLS 6) ;; Numero de columnas del tablero
(define PLAYER1 1) ;; Numero que representa al jugador 1
(define PLAYER2 2) ;; Numero que representa al jugador 2
(define EMPTY 0) ;; Numero que representa un espacio vacio


;; ======================================================================
;; Representación del tablero de juego por medio de un vector de vectores
;; ======================================================================
(define board (vector (vector 0 0 0 0 0 0) (vector 0 0 0 0 0 0) (vector 0 0 0 0 0 0) (vector 0 0 0 0 0 0) (vector 0 0 0 0 0 0))) ;; 5 rows and 6 columns

;; ======================================================================
;; Funcion de copy-board que copia el tablero de juego
;; ======================================================================
(define (copy-board board)
    (define new (vector (vector 0 0 0 0 0 0) (vector 0 0 0 0 0 0) (vector 0 0 0 0 0 0) (vector 0 0 0 0 0 0) (vector 0 0 0 0 0 0)))
    ;; ======================================================================
    ;; Copia los valores del tablero viejo al nuevo tablero
    ;; ======================================================================
    (define (copy-board-iter board new row col)
        ;; ======================================================================
        ;; Si se llega al final de la fila se pasa a la siguiente
        ;; ======================================================================
        (if (= row 5)
            new
            ;; ======================================================================
            ;; Si se llega al final de la columna se pasa a la siguiente
            ;; ======================================================================
            (if (= col 6)
                ;; ======================================================================
                ;; Copia el valor de la celda del tablero viejo al tablero nuevo
                ;; ======================================================================
                (copy-board-iter board new (+ row 1) 0)
                (begin
                    ;; ======================================================================
                    ;; Se setea el valor de la celda del tablero nuevo
                    ;; ======================================================================
                    (vector-set! (vector-ref new row) col (vector-ref (vector-ref board row) col))
                    (copy-board-iter board new row (+ col 1))))))
    (copy-board-iter board new 0 0)
    new)

;; ======================================================================
;; VALIDACIONES DEL JUEGO DE DARA
;; ======================================================================
;; La funcio de valid-move? verifica si el movimiento es valido
;; ======================================================================
(define (valid-move? board row col)
    ;; ======================================================================
    ;; Si la fila y la columna estan dentro del tablero y la celda esta vacia
    ;; el movimiento es valido
    ;; ======================================================================
    (and (>= row 0) (< row BOARD-ROWS) (>= col 0) (< col BOARD-COLS) (= (get-cell board row col) 0)))

;; ======================================================================
;; Revisa si el movimiento esta fuera de los limites del tablero
;; ======================================================================
(define (move-out-of-bounds? row col)
    (or (< row 0) (>= row BOARD-ROWS) (< col 0) (>= col BOARD-COLS)))

;; ======================================================================
;; Revisa si la celda esta vacia
;; ======================================================================
(define (space-empty? board row col)
    ;; ======================================================================
    ;; Si move-out-of-bounds? es verdadero entonces la celda esta vacia
    ;; ======================================================================
    (if (move-out-of-bounds? row col)
        (displayln "Invalid move space not empty")
        (= (get-cell board row col) 0)))

;; ======================================================================
;; Revisa si no hay mas de 2 fichas en una fila
;; ======================================================================
(define (no-more-than-2-in-a-row? board row col player)
    ;; ======================================================================
    ;; Si el movimiento esta fuera de los limites del tablero entonces
    ;; no hay mas de 2 fichas en una fila
    ;; ======================================================================
    (if (move-out-of-bounds? row col)
        (displayln "Invalid move no more than 2 in a row")
        (if (and (= (get-cell board row col) player) (= (get-cell board row (+ col 1)) player))
            #f
            #t)))

;; HORA DE JUGAR
;; ======================================================================
;; Se define los getter y setter para el tablero de juego
;; ======================================================================
;; Obtiene el valor de una celda del tablero
;; ======================================================================
(define (get-cell board row col)
    (if (move-out-of-bounds? row col)
        (displayln "Get cell Invalid move")
        (vector-ref (vector-ref board row) col)))
;; ======================================================================
;; Setea el valor de una celda del tablero
;; ======================================================================
(define (set-cell! board row col player)
    (if (move-out-of-bounds? row col)
        (displayln "Set cell Invalid move")
        (vector-set! (vector-ref board row) col player)))

;; ======================================================================
;; Se optiene la posicion de cada ficha del tablero
;; ======================================================================
(define (get-positions board player)
    (define (get-positions-iter board player row col positions)
        ;; ======================================================================
        ;; Si se llega al final de la fila se pasa a la siguiente
        ;; ======================================================================
        (if (= row 5)
            positions
            ;; ======================================================================
            ;; Si se llega al final de la columna se pasa a la siguiente
            ;; ======================================================================
            (if (= col 6)
                ;; ======================================================================
                ;; Se obtiene la posicion de la ficha
                ;; ======================================================================
                (get-positions-iter board player (+ row 1) 0 positions)
                ;; ======================================================================
                ;; Si la ficha es del jugador se obtiene la posicion
                ;; ======================================================================
                (if (= (get-cell board row col) player)
                    (get-positions-iter board player row (+ col 1) (cons (list row col) positions))
                    (get-positions-iter board player row (+ col 1) positions)))))
    (get-positions-iter board player 0 0 '()))

;; ======================================================================
;; Reviza si el tablero esta lleno
;; ======================================================================
(define (board-full? board)
    (define (board-full?-iter board row col)
        ;; ======================================================================
        ;; Si se llega al final de la fila se pasa a la siguiente
        ;; ======================================================================
        (if (= row 5)
            #t
            ;; ======================================================================
            ;; Si se llega al final de la columna se pasa a la siguiente
            ;; ======================================================================
            (if (= col 6)
                ;; ======================================================================
                ;; Se revisa si el table esta lleno
                ;; ======================================================================
                (board-full?-iter board (+ row 1) 0)
                (if (= (get-cell board row col) 0)
                    #f
                    (board-full?-iter board row (+ col 1))))))
    (board-full?-iter board 0 0))

;; ======================================================================
;; Revisa si el tablero esta vacio
;; ======================================================================
(define (board-empty? board)
    (define (board-empty?-iter board row col)
        ;; ======================================================================
        ;; Si se llega al final de la fila se pasa a la siguiente
        ;; ======================================================================
        (if (= row 5)
            #t
            ;; ======================================================================
            ;; Si se llega al final de la columna se pasa a la siguiente
            ;; ======================================================================
            (if (= col 6)
                ;; ======================================================================
                ;; Se revisa si el table esta vacio
                ;; ======================================================================
                (board-empty?-iter board (+ row 1) 0)
                ;; ======================================================================
                ;; Si la celda esta vacia se pasa a la siguiente
                ;; ======================================================================
                (if (= (get-cell board row col) 0)
                    (board-empty?-iter board row (+ col 1))
                    #f))))
    (board-empty?-iter board 0 0))

;; ======================================================================
;; Lugar una ficha en el tablero
;; ======================================================================
(define (place-piece board row col player)
    ;; ======================================================================
    ;; Si el movimiento es valido se coloca la ficha
    ;; ======================================================================
    (if (valid-move? board row col)
        (set-cell! board row col player)
        (displayln "Plac piece Invalid move")))

;; ======================================================================
;; Obtiene la ficha de una celda del tablero
;; ======================================================================
(define (get-piece board row col)
    ;; ======================================================================
    ;; Si el movimiento es valido se obtiene la ficha
    ;; ======================================================================
    (if (move-out-of-bounds? row col)
        (displayln "Get piece Invalid move")
        (vector-ref (vector-ref board row) col)))

;; ======================================================================
;; Obtiene el jugador opuesto al jugador actual
;; ======================================================================
(define (get-opponent player)
    (if (= player PLAYER1)
        PLAYER2
        PLAYER1))

;; ======================================================================
;; Cuenta el número de piezas de un tipo dado en una línea en el tablero, dado un vector de dirección
;; ======================================================================
(define (count-pieces-in-line board row col dx dy player)
    ;; ======================================================================
    ;; Si el movimiento esta fuera de los limites del tablero entonces
    ;; ======================================================================
    (if (move-out-of-bounds? row col)
        0
        ;; ======================================================================
        ;; Si la ficha es del jugador se cuenta la ficha y se pasa a la siguiente
        ;; ======================================================================
        (if (= (get-piece board row col) player)
            (+ 1 (count-pieces-in-line board (+ row dx) (+ col dy) dx dy player))
            0)))

;; ======================================================================
;; Cuenta el número de piezas de un tipo dado en una fila o columna en el tablero desde una celda dada
;; ======================================================================
(define (count-pieces-in-line-dir board row col player dir-row dir-col)
    ;; ======================================================================
    ;; Si el movimiento esta fuera de los limites del tablero entonces
    ;; ======================================================================
    (if (move-out-of-bounds? row col)
        0
        ;; ======================================================================
        ;; Un cond para cada direccion
        ;; ======================================================================
        (cond
            [(= dir-row 0) (count-pieces-in-line board row col 0 1 player)] ; Contar desde la celda actual hacia la derecha
            [(= dir-col 0) (count-pieces-in-line board row col 1 0 player)] ; contar desde la celda actual hacia la abajo
            [(= dir-row 1) (count-pieces-in-line board row col 0 -1 player)] ; Contar desde la celda actual hacia la izquierda
            [(= dir-col 1) (count-pieces-in-line board row col -1 0 player)] ; contar desde la celda actual hacia la arriba
            [else 0])))

;; ======================================================================
;; Remover una pieza del tablero
;; ======================================================================
(define (remove-piece board row col) ;; Fila de 0 a 4 y columna de 0 a 5
    ;; ======================================================================
    ;; Si el movimiento esta fuera de los limites del tablero entonces
    ;; ======================================================================
    (if (move-out-of-bounds? row col)
        (displayln "Move out of bounds")
        (vector-set! (vector-ref board row) col 0)))

;; ======================================================================
;; Mover una pieza del tablero a una nueva celda quitar la pieza de la celda anterior
;, si el movimiento es valido
;; ======================================================================
(define (move-piece board row col new-row new-col player)
    ;; ======================================================================
    ;; Si el movimiento es valido se coloca la ficha
    ;; ======================================================================
    (if (and (valid-move? board new-row new-col) (= (get-piece board row col) player))
        (begin
            ;; ======================================================================
            ;; Se coloca la ficha en la nueva celda
            ;; ======================================================================
            (set-cell! board new-row new-col player)
            (remove-piece board row col))
        (displayln "You don{t own this piece or the move is invalid")))

;; ======================================================================
;; Cuenta las piezas restantes para un jugador
;; ======================================================================
(define (count-pieces board player)
    ;; ======================================================================
    ;; La funcion de count-pieces-iter recibe el tablero, la fila, la columna y el contador
    ;; ======================================================================
    (define (count-pieces-iter board row col count)
        ;; ======================================================================
        ;; Si se llega al final de la fila se pasa a la siguiente
        ;; ======================================================================
        (if (= row 5)
            count
            ;; ======================================================================
            ;; Si se llega al final de la columna se pasa a la siguiente
            ;; ======================================================================
            (if (= col 6)
                (count-pieces-iter board (+ row 1) 0 count)
                (if (= (get-piece board row col) player)
                    ;; ======================================================================
                    ;; Count-pieces-iter recibe el tablero, la fila, la columna y el contador
                    ;; ======================================================================
                    (count-pieces-iter board row (+ col 1) (+ count 1))
                    (count-pieces-iter board row (+ col 1) count)))))
    (count-pieces-iter board 0 0 0))

;; ======================================================================
;; Revisa si un jugador tiene menos de 3 piezas
;; ======================================================================
(define (less-than-3-pieces? board player)
    (if (< (count-pieces board player) 3)
        #t
        #f))

;; ======================================================================
;; La funcion de can-move?-iter recibe el tablero, la fila, la columna y el contador para saber si se puede mover
;; ======================================================================
(define (can-move? board player)
    (define (can-move?-iter board row col)
        ;; ======================================================================
        ;; Si se llega al final de la fila se pasa a la siguiente
        ;; ======================================================================
        (if (= row 5)
            #f
            ;; ======================================================================
            ;; Si se llega al final de la columna se pasa a la siguiente
            ;; ======================================================================
            (if (= col 6)
                ;; ======================================================================
                ;; can-move?-iter para la siguiente fila
                ;; ======================================================================
                (can-move?-iter board (+ row 1) 0)
                ;; ======================================================================
                ;; si el movimiento es valido se retorna #t
                ;; ======================================================================
                (if (valid-move? board row col)
                    #t
                    (can-move?-iter board row (+ col 1))))))
    (can-move?-iter board 0 0))

;; ======================================================================
;; Revisar si el jugador tiene 2 piezas en el tablero por lo que ha perdido
;; ======================================================================
(define (lost? board player)
    (if (or (less-than-3-pieces? board player) (not (can-move? board player)))
        #t
        #f))

;; ======================================================================
;; Revisar quien ha ganado
;; ======================================================================
(define (who-won board)
    (cond
        [(lost? board PLAYER1) PLAYER2]
        [(lost? board PLAYER2) PLAYER1]
        [else 0]))

;; ======================================================================
;; Cambiador de jugador
;; ======================================================================
(define (switch-player player)
    (if (= player PLAYER1)
        PLAYER2
        PLAYER1))

;; ======================================================================
;; Obtiene una lista de las celdas vacias
;; ======================================================================
(define (get-empty-cells board)
    (define (get-empty-cells-iter board row col empty-cells)
    ;; ======================================================================
    ;; Si se llega al final de la fila se pasa a la siguiente
    ;; ======================================================================
        (if (= row 5)
            empty-cells
            ;; ======================================================================
            ;; Si se llega al final de la columna se pasa a la siguiente
            ;; ======================================================================
            (if (= col 6)
                (get-empty-cells-iter board (+ row 1) 0 empty-cells)
                ;; ======================================================================
                ;; Si get-piece retorna 0 entonces la celda esta vacia
                ;; ======================================================================
                (if (= (get-piece board row col) 0)
                    ;; ======================================================================
                    ;; get-empty-cells-iter recibe el tablero, la fila, la columna y la lista de celdas vacias
                    ;; ======================================================================
                    (get-empty-cells-iter board row (+ col 1) (cons (list row col) empty-cells))
                    (get-empty-cells-iter board row (+ col 1) empty-cells)))))
    (get-empty-cells-iter board 0 0 '()))

;; ======================================================================
;; Comprobar si el movimiento hace 3 piezas en una fila o columna
;; ======================================================================
(define (move-makes-3-in-a-row? board row col player)
    ;; ======================================================================
    ;; La funcion de count-pieces-in-line-dir recibe el tablero, la fila, la columna, el jugador, el incremento de la fila y el incremento de la columna
    ;; ======================================================================
    (if (or (>= (count-pieces-in-line-dir board row col player 0 1) 3) ; Revisa a la derecha
            (>= (count-pieces-in-line-dir board row col player 1 0) 3) ; Revisa hacia abajo
            (>= (count-pieces-in-line-dir board row col player -1 0) 3) ; Revisa hacia arriba
            (>= (count-pieces-in-line-dir board row col player 0 -1) 3) ; Revisa hacia la izquierda
            ;; ======================================================================
            ;; Verificar desde una columna hacia atrás
            ;; ======================================================================
            (>= (count-pieces-in-line-dir board row (- col 1) player 0 1) 3) ; Revise hacia la derecha
            ;; ======================================================================
            ;; Verificar de una columna hacia adelante
            ;; ======================================================================
            (>= (count-pieces-in-line-dir board row (+ col 1) player 0 1) 3) ; Revise hacia la derecha
            ;; ======================================================================
            ;; Comprobar desde una fila hacia atrás
            ;; ======================================================================
            (>= (count-pieces-in-line-dir board (- row 1) col player 1 0) 3) ; Revisa hacia abajo
            ;; ======================================================================
            ;; Verificar desde una fila hacia adelante
            ;; ======================================================================
            (>= (count-pieces-in-line-dir board (+ row 1) col player 1 0) 3) ; Revise hacia abajo
            ;; ======================================================================
            ;; Verifique desde dos columnas hacia atrás
            ;; ======================================================================
            (>= (count-pieces-in-line-dir board row (- col 2) player 0 1) 3) ; Revise hacia la derecha
            ;; ======================================================================
            ;; Verificación de dos columnas hacia adelante
            ;; ======================================================================
            (>= (count-pieces-in-line-dir board row (+ col 2) player 0 1) 3) ; Revise hacia la derecha
            ;; ======================================================================
            ;; Comprobar desde dos filas hacia atrás
            ;; ======================================================================
            (>= (count-pieces-in-line-dir board (- row 2) col player 1 0) 3) ; Revise hacia abajo
            ;; ======================================================================
            ;; Verificar desde dos filas hacia adelante
            ;; ======================================================================
            (>= (count-pieces-in-line-dir board (+ row 2) col player 1 0) 3) ); Revise hacia abajo
        #t
        #f))

;; ======================================================================
;; Coloca la pieza aleatoriamente no más de 2 piezas en una fila o columna
;; ======================================================================
(define (place-piece-random board piece)
    ;; ======================================================================
    ;; Se define place-piece-random-iter que recibe el tablero, la pieza y la lista de celdas vacias
    ;; ======================================================================
    (define (place-piece-random-iter board piece empty-cells)
        (if (null? empty-cells)
            (displayln "No more empty cells")
            (let ([cell (random-element empty-cells)])
                ;; ======================================================================
                ;; Si el movimiento hace 3 piezas en una fila o columna se coloca la pieza
                ;; ======================================================================
                (if (and (<= (count-pieces-in-line-dir board (car cell) (cadr cell) piece 0 1) 1)
                         (<= (count-pieces-in-line-dir board (car cell) (cadr cell) piece 1 0) 1)
                         (<= (count-pieces-in-line-dir board (car cell) (cadr cell) piece 1 1) 1)
                         (<= (count-pieces-in-line-dir board (car cell) (cadr cell) piece 1 -1) 1)
                         (<= (count-pieces-in-line-dir board (car cell) (cadr cell) piece -1 1) 1)
                         (<= (count-pieces-in-line-dir board (car cell) (cadr cell) piece -1 -1) 1)
                         (<= (count-pieces-in-line-dir board (car cell) (cadr cell) piece -1 0) 1)
                         (<= (count-pieces-in-line-dir board (car cell) (cadr cell) piece 0 -1) 1)
                         ;; ======================================================================
                         ;; Verificar desde una columna hacia atrás
                         ;; ======================================================================
                         (<= (count-pieces-in-line-dir board (car cell) (- (cadr cell) 1) piece 0 1) 1)
                            ;; ======================================================================
                            ;; Verificar de una columna hacia adelante
                            ;; ======================================================================
                            (<= (count-pieces-in-line-dir board (car cell) (+ (cadr cell) 1) piece 0 1) 1)
                            ;; ======================================================================
                            ;; Comprobar desde una fila hacia atrás
                            ;; ======================================================================
                            (<= (count-pieces-in-line-dir board (- (car cell) 1) (cadr cell) piece 1 0) 1)
                            ;; ======================================================================
                            ;; Verificar desde una fila hacia adelante
                            ;; ======================================================================
                            (<= (count-pieces-in-line-dir board (+ (car cell) 1) (cadr cell) piece 1 0) 1)
                            ;; ======================================================================
                            ;; Verifique desde dos columnas hacia atrás
                            ;; ======================================================================
                            (<= (count-pieces-in-line-dir board (car cell) (- (cadr cell) 2) piece 0 1) 1)
                            ;; ======================================================================
                            ;; Verificación de dos columnas hacia adelante
                            ;; ======================================================================
                            (<= (count-pieces-in-line-dir board (car cell) (+ (cadr cell) 2) piece 0 1) 1)
                            ;; ======================================================================
                            ;; Comprobar desde dos filas hacia atrás
                            ;; ======================================================================
                            (<= (count-pieces-in-line-dir board (- (car cell) 2) (cadr cell) piece 1 0) 1)
                            ;; ======================================================================
                            ;; Revisar desde dos filas hacia adelante
                            ;; ======================================================================
                            (<= (count-pieces-in-line-dir board (+ (car cell) 2) (cadr cell) piece 1 0) 1)
                         )
                    (place-piece board (car cell) (cadr cell) piece)
                    (place-piece-random-iter board piece (remove cell empty-cells))))))
    (place-piece-random-iter board piece (get-empty-cells board)))

;; ======================================================================
;; Mueve 1 espacio hacia arriba para un jugador dado
;; ======================================================================
(define (move-one-up board row col player)
    (if (valid-move? board (- row 1) col)
        ;; ======================================================================
        ;; Revisa que la pieza pertenezca al jugador
        ;; ======================================================================
        (if (equal? (get-piece board row col) player)
            (move-piece board row col (- row 1) col player)
            (displayln "Piece does not belong to player")
            )
        (displayln "One up Invalid move")))

;; ======================================================================
;; Mover 1 espacio hacia abajo para un jugador dado
;; ======================================================================
(define (move-one-down board row col player)
    (if (valid-move? board (+ row 1) col)
    ;; ======================================================================
    ;; Revise que la pieza pertenezca al jugador
    ;; ======================================================================
        (if (equal? (get-piece board row col) player)
            (move-piece board row col (+ row 1) col player)
            (displayln "Piece does not belong to player")
            )
        (displayln "One down Invalid move")))

;; ======================================================================
;; Mover 1 espacio a la izquierda para un jugador dado
;; ======================================================================
(define (move-one-left board row col player)
    (if (valid-move? board row (- col 1))
        ;; ======================================================================
        ;; Revise que la pieza pertenezca al jugador
        ;; ======================================================================
        (if (equal? (get-piece board row col) player)
            (move-piece board row col row (- col 1) player)
            (displayln "Piece does not belong to player")
            )
        (displayln "One left Invalid move")))

;; ======================================================================
;; Mover 1 espacio a la derecha para un jugador dado
;; ======================================================================
(define (move-one-right board row col player)
    (if (valid-move? board row (+ col 1))
        ;; ======================================================================
        ;; Revise que la pieza pertenezca al jugador
        ;; ======================================================================
        (if (equal? (get-piece board row col) player)
            (move-piece board row col row (+ col 1) player)
            (displayln "Piece does not belong to player")
            )
        (displayln "One right Invalid move")))

;; ======================================================================
;; Escoge una dirección al azar y mueve la pieza en esa dirección
;; ======================================================================
(define (move-piece-dir board row col player dir)
    (cond
        [(= dir 1) (move-one-up board row col player)]
        [(= dir 2) (move-one-down board row col player)]
        [(= dir 3) (move-one-left board row col player)]
        [(= dir 4) (move-one-right board row col player)]))

;; ======================================================================
;; Retorna una dirección al azar
;; ======================================================================
(define (random-element lst)
  (let* ([len (length lst)]
         [idx (random len)])
    (list-ref lst idx)))

;; ======================================================================
;; Creacion del tablero inicial
;; ======================================================================
(define (initial-board)
  (for ([i (in-range 12)])
    (place-piece-random board PLAYER1)
    (place-piece-random board PLAYER2))
  board)

;; ======================================================================
;; Revisar si el juego termino
;; ======================================================================
(define (game-over? board)
    (if (or (lost? board PLAYER1)
            (lost? board PLAYER2))
        #t
        #f))

;; ======================================================================
;; Preguntar al jugador si desea remover una pieza del oponente
;; ======================================================================
(define (remove-opponent-piece board player)
    (displayln "3 pieces in a row or column")
    (displayln "Player ")
    (display player)
    (displayln "Enter a piece to remove")
    (displayln "Enter row")
    (define row (read))
    (displayln "Enter column")
    (define col (read))
    (remove-piece board row col)
    (refresh))

;; ======================================================================
;; Obtener una nueva fila de una pieza
;; ======================================================================
(define (get-new-row row dir)
    (cond
        [(= dir 1) (- row 1)] ;; Arriba
        [(= dir 2) (+ row 1)] ;; Abajo
        [(= dir 3) row] ;; Izquierda
        [(= dir 4) row]))  ;; Derecha

;; ======================================================================
;; Obtener una nueva columna de una pieza
;; ======================================================================
(define (get-new-col col dir)
    (cond
        [(= dir 1) col] ;; Arriba
        [(= dir 2) col] ;; Abajo
        [(= dir 3) (- col 1)] ;; Izquierda
        [(= dir 4) (+ col 1)]))  ;; Derecha

;; ======================================================================
;; Preguntar al jugador si desea mover una pieza en una dirección dada
;; ======================================================================
(define (move-piece-in-direction board player)
    (displayln "Turn of player")
    (displayln player)
    (displayln "Enter a piece to move")
    (displayln "Enter row")
    (define row (read))
    (displayln "Enter column")
    (define col (read))
    (displayln "Enter a direction to move the piece")
    (displayln "1 - up")
    (displayln "2 - down")
    (displayln "3 - left")
    (displayln "4 - right")
    (define dir (read))
    (move-piece-dir board row col player dir)
    ;; ======================================================================
    ;; Obtenemos la nueva fila y columna
    ;; ======================================================================
    (define new-row (get-new-row row dir))
    (define new-col (get-new-col col dir))
    (refresh)
    ;; ======================================================================
    ;; Si la pieza se movio a una posición que hace 3 en linea
    ;; ======================================================================
    (if (move-makes-3-in-a-row? board new-row new-col player)
        (remove-opponent-piece board player)
        board))


;; FUNCIONES PARA EL JUEGO DE IA

;; ======================================================================
;; Validar si una pieza puede moverse
;; ======================================================================
(define (valid-ia-move? board row col)
    ;; ======================================================================
    ;; Verificar que la pieza no este en la esquina
    ;; ======================================================================
    (if (equal? (get-cell board row col) EMPTY)
        #f
        (if (or (valid-move? board (- row 1) col)
            (valid-move? board (+ row 1) col)
            (valid-move? board row (- col 1))
            (valid-move? board row (+ col 1)))
        #t
        #f)))

;; ======================================================================
;; Obtener una lista de todas las piezas que pueden moverse
;; ======================================================================
(define (get-all-valid-pieces-to-moves board player)
    (define (get-all-valid-moves-iter board row col moves)
        ;; ======================================================================
        ;; Si la cantidad de filas es 5, retornamos la lista de movimientos
        ;; ======================================================================
        (if (= row 5)
            moves
            ;; ======================================================================
            ;; Si la cantidad de columnas es 6, llamamos recursivamente con la siguiente fila
            ;; ======================================================================
            (if (= col 6)
                ;; ======================================================================
                ;; Si la pieza es del jugador y puede moverse, la agregamos a la lista
                ;; ======================================================================
                (get-all-valid-moves-iter board (+ row 1) 0 moves)
                ;; ======================================================================
                ;; Si la pieza es del jugador y puede moverse, la agregamos a la lista
                ;; ======================================================================
                (if (and (valid-ia-move? board row col) (= (get-cell board row col) player))
                    (get-all-valid-moves-iter board row (+ col 1) (cons (list row col) moves))
                    (get-all-valid-moves-iter board row (+ col 1) moves)))))
    (get-all-valid-moves-iter board 0 0 '()))

;; ======================================================================
;; Obtener una lista de direcciones válidas para mover una pieza dada (1 arriba, 2 abajo, 3 izquierda, 4 derecha)
;; ======================================================================
(define (get-valid-directions board row col)
    ;; ======================================================================
    ;; Espacio arriba es válido y vacío?
    ;; ======================================================================
    (define up (and (valid-move? board (- row 1) col) (equal? (get-cell board (- row 1) col) EMPTY)))
    ;; ======================================================================
    ;; Espacio abajo es válido y vacío?
    ;; ======================================================================
    (define down (and (valid-move? board (+ row 1) col) (equal? (get-cell board (+ row 1) col) EMPTY)))
    ;; ======================================================================
    ;; Espacio izquierdo es válido y vacío?
    ;; ======================================================================
    (define left (and (valid-move? board row (- col 1)) (equal? (get-cell board row (- col 1)) EMPTY)))
    ;; ======================================================================
    ;; Espacio derecho es válido y vacío?
    ;; ======================================================================
    (define right (and (valid-move? board row (+ col 1)) (equal? (get-cell board row (+ col 1)) EMPTY)))
    (define dirs '())
    ;; ======================================================================
    ;; Agregamos las direcciones válidas a la lista
    ;; ======================================================================
    (cond
        [up (set! dirs (cons 1 dirs))])
    (cond
        [down (set! dirs (cons 2 dirs))])
    (cond
        [left (set! dirs (cons 3 dirs))])
    (cond
        [right (set! dirs (cons 4 dirs))])

    dirs)

;; ======================================================================
;; Compruebe que el movimiento no está fuera de los límites, está vacío y está un espacio arriba, abajo, a la izquierda o a la derecha
;; ======================================================================
(define (validation-for-ia-move? board row col dir)
    (cond
        ;; ======================================================================
        ;; Si la dirección es 1, verifique que la fila sea mayor que 1 y que la celda arriba esté vacía
        ;; ======================================================================
        [(= dir 1) (and (valid-move? board (- row 1) col) (equal? (get-cell board (- row 1) col) EMPTY))] ;; Arriba
        ;; ======================================================================
        ;; Si la dirección es 2, verifique que la fila sea menor que 4 y que la celda abajo esté vacía
        ;; ======================================================================
        [(= dir 2) (and (valid-move? board (+ row 1) col) (equal? (get-cell board (+ row 1) col) EMPTY))] ;; Abajo
        ;; ======================================================================
        ;; Si la dirección es 3, verifique que la columna sea mayor que 1 y que la celda a la izquierda esté vacía
        ;; ======================================================================
        [(= dir 3) (and (valid-move? board row (- col 1)) (equal? (get-cell board row (- col 1)) EMPTY))] ;; Izquierda
        ;; ======================================================================
        ;; Si la dirección es 4, verifique que la columna sea menor que 4 y que la celda a la derecha esté vacía
        ;; ======================================================================
        [(= dir 4) (and (valid-move? board row (+ col 1)) (equal? (get-cell board row (+ col 1)) EMPTY))])) ;; Derecha

;; ======================================================================
;; IA quitar pieza oponente
;; ======================================================================
(define (ia-remove-opponent-piece board player)
    ;; ======================================================================
    ;; Obteniendo todas las piezas que pueden moverse
    ;; ======================================================================
    (define moves (get-all-valid-pieces-to-moves board (switch-player player)))
    ;; ======================================================================
    ;; Obtener un movimiento aleatorio
    ;; ======================================================================
    (define opponent-piece (random-element moves))
    ;; ======================================================================
    ;; Obtener fila y columna de la pieza a mover
    ;; ======================================================================
    (define row (car opponent-piece))
    (define col (cadr opponent-piece))
    ;; ======================================================================
    ;; Remover pieza
    ;; ======================================================================
    (set-cell! board row col EMPTY)
    (refresh)
    board)


;; get best opponent piece to remove
(define (get-best-opponent-piece board moves player)
    (define (get-best-opponent-piece-iter moves best-opponent-piece)
        (if (null? moves)
            best-opponent-piece
            (let ([move (car moves)])
                (if (move-makes-3-in-a-row? board (car move) (cadr move) player)
                    (get-best-opponent-piece-iter (cdr moves) move)
                    (get-best-opponent-piece-iter (cdr moves) best-opponent-piece)))))
    (get-best-opponent-piece-iter moves (car moves)))

;; IA remove best opponent piece
(define (ia-remove-best-opponent-piece board player)
    ;; get all valid moves for player
    (define moves (get-all-valid-pieces-to-moves board (switch-player player)))
    ;; get best move
    (define best-opponent-piece (get-best-opponent-piece board moves player))
    ;; get row and column of piece to move
    (define row (car best-opponent-piece))
    (define col (cadr best-opponent-piece))
    ;; remove piece
    (set-cell! board row col EMPTY)
    (refresh)
    board)

;; ======================================================================
;; IA moviendo aleatorio
;; ======================================================================
    (define (ia-random-move board player)
    (define moves (get-all-valid-pieces-to-moves board player))
    (define move (random-element moves))
    ;; ======================================================================
    ;; Obtener fila y columna de la pieza a mover
    ;; ======================================================================
    (define row (car move))
    (define col (cadr move))
    ;; ======================================================================
    ;; Obteniendo una dirección aleatoria
    ;; La dirección aleatoria es 1 arriba, 2 abajo, 3 izquierda, 4 derecha
    ;; ======================================================================
    (define dirs (get-valid-directions board row col))
    (define dir (random-element dirs))
    ;(displayln "IA move")
    ;(displayln row)
    ;(displayln col)
    ;(displayln dir)
    ;; check if move is valid
    (displayln "Random move")
    (cond
        ;; ======================================================================
        ;; Validation for IA move se encarga de verificar que el movimiento no esté fuera de los límites, esté vacío y esté un espacio arriba, abajo, a la izquierda o a la derecha
        ;; ======================================================================
        [(validation-for-ia-move? board row col dir)
            (move-piece-dir board row col player dir)
            (define new-row (get-new-row row dir))
            (define new-col (get-new-col col dir))
            ;; ======================================================================
            ;; Si el movimiento hace 3 piezas en una fila o columna, retire la pieza oponente
            ;; ======================================================================
            (if (move-makes-3-in-a-row? board new-row  new-col  player)
                ;; ======================================================================
                ;; Remueve la pieza oponente
                ;; ======================================================================
                (ia-remove-opponent-piece board player)
                board)
            (refresh)]
        [else (ia-random-move board player)]))

;; ======================================================================
;; Movimiento inteligente de la IA
;; ======================================================================
;; Esta terminal
;; ======================================================================
(define (terminal-state? board)
    (if (or (lost? board PLAYER1)
            (lost? board PLAYER2))
        #t
        #f))

;; ======================================================================
;; Funcion Eval para el estado del tablero
;; ======================================================================
(define (evaluate-board board player)
    ;; count number of pieces for each player
    (define my-pieces (count-pieces board player))
    (define op-pieces (count-pieces board (switch-player player)))

    ;; total number of pieces
    ;(define total-pieces (+ player1-pieces player2-pieces))

    ;; average distance from center for each player
    ;(define player1-distance (average-distance board player))
    ;(define player2-distance (average-distance board (switch-player player)))

    ;; Bueno para IA valores negativos, malo para IA valores positivos
    (cond
            ;; if PLAYER1 has lost
            [(lost? board (switch-player player)) -10000]
            ;; if PLAYER2 has lost
            [(lost? board player) 10000]

            ;; if PLAYER1 has more pieces than PLAYER2
            [(> my-pieces op-pieces) -500]
            ;; if PLAYER2 has more pieces than PLAYER1
            [(<= my-pieces op-pieces) 500]




;        ;; Ratio of pieces PLAYER1/PLAYER2 is greater than 1
;        [(> (/ player1-pieces player2-pieces) 1) 30]
;
;         ;; Ratio of pieces PLAYER1/PLAYER2 is less than 1
;        [(< (/ player1-pieces player2-pieces) 1) -30]
;
;        ;; PLAYER2 has more pieces than PLAYER1
;        [(> player2-pieces player1-pieces)
;            (cond
;                ;; if PLAYER2 has 2 more than PLAYER1
;                [(= (- player2-pieces player1-pieces) 2) 49]
;                ;; if PLAYER2 has 1 more than PLAYER1
;                [(= (- player2-pieces player1-pieces) 1) 48]
;                [else 50])]
;
;        ; PLAYER1 has more pieces than PLAYER2
;        [(< player2-pieces player1-pieces)
;            (cond
;                ;; if PLAYER1 has 2 more than PLAYER2
;                [(= (- player1-pieces player2-pieces) 2) ]
;                ;; if PLAYER1 has 1 more than PLAYER2
;                [(= (- player1-pieces player2-pieces) 1) -48]
;                [else -50]
;                )]
;
;        ;; PLAYER2 has more moves than PLAYER1
;        [(> (length (get-all-valid-pieces-to-moves board (switch-player player))) (length (get-all-valid-pieces-to-moves board player))) 60]
;        ;; PLAYER1 has more moves than PLAYER2
;        [(< (length (get-all-valid-pieces-to-moves board (switch-player player))) (length (get-all-valid-pieces-to-moves board player))) -60]

        ;; PLAYER2 has less distance from center than PLAYER1
        ;[(< player2-distance player1-distance) -20]
        ;; PLAYER1 has less distance from center than
        ;[(> player2-distance player1-distance) 20]PLAYER2

        [else 0]))


;; IA mode random or smart
(define (ia-move board player ia-mode)
    (if (= ia-mode 1)
        (ia-random-move board player)
        ;(ia-smart-move board player)
        (displayln "IA mode not implemented yet")))

;; ======================================================================
;; Generar todos los tableros posibles
;; ======================================================================
(define (generate-children-boards board player)
    ;; ======================================================================
    ;; Obtener todos los movimientos posibles
    ;; ======================================================================
    (define moves (get-all-valid-pieces-to-moves board player))
    (define children-boards '())
    ;; ======================================================================
    ;; Recolectar todos los tableros posibles
    ;; ======================================================================
    (for-each (lambda (move)
        (define row (car move))
        (define col (cadr move))
        ;; ======================================================================
        ;; Obteniendo las direcciones posibles válidas para cada movimiento
        ;; ======================================================================
        (define dirs (get-valid-directions board row col))
        (for-each (lambda (dir)
            ;; ======================================================================
            ;; Generar un tablero hijo
            ;; ======================================================================
            (define child-board (generate-child-board board player row col dir))
            ;(displayln "move")
            ;(displayln row)
            ;(displayln col)
            ;(displayln dir)
            ;(displayln "child board")
            ;(displayln child-board)
            ;; ======================================================================
            ;; Si el tablero hijo es válido, agregarlo a la lista de tableros hijos
            ;; ======================================================================
            (if child-board
                (set! children-boards (cons child-board children-boards))
                ;(displayln "No child invalid move")
                #f))
            dirs))
        moves)
    children-boards)

;; ======================================================================
;; generar un tablero secundario a partir de un movimiento
;; si el movimiento es válido
;; si el movimiento hace 3 piezas en una fila o columna quitar pieza
;; si no regresa #f
;; ======================================================================
(define (generate-child-board new-board player row col dir)
    (define new-board (copy-board board)) ;; copy board
    ;; ======================================================================
    ;; Si el movimiento es válido
    ;; ======================================================================
    (cond
    ;; ======================================================================
    ;; Si el movimiento es válido en la IA
    ;; ======================================================================
    [(validation-for-ia-move? new-board row col dir) ;; Revisar si el movimiento es valido
        (define new-row (get-new-row row dir)) ;; Obtener nueva fila
        (define new-col (get-new-col col dir)) ;; Obtener nueva columna
        (move-piece-dir new-board row col player dir) ;;Mover pieza
        ;; ======================================================================
        ;; Si el movimiento hace 3 piezas en una fila o columna, retire la pieza oponente
        ;; ======================================================================
        (cond
            [(move-makes-3-in-a-row? new-board new-row  new-col  player) ;; Comprobar si el movimiento hace 3 piezas en una fila o columna
                ;(ia-remove-opponent-piece new-board player) ;; Quitar pieza oponente
                (ia-remove-best-opponent-piece new-board player) ;; remove BEST opponent piece
                new-board] ;; Devolver el tablero nuevo si el movimiento hace 3 piezas en una fila o columna
            [else new-board])] ;; Devuelve un tablero nuevo si el movimiento no hace 3 piezas en una fila o columna
    [else #f])) ;; Devolver #f si el movimiento no es válido

;;(copy-board board)
;; ======================================================================
;; minimax alfa beta
;; devuelve el mejor movimiento
;; ======================================================================
(define (minimax-alpha-beta board depth maximizing-player? alpha beta best-board)
  (cond
    ;; ======================================================================
    ;; Si el estado terminal devuelve la evaluación de la placa
    ;; ======================================================================
    [(terminal-state? board) (evaluate-board board maximizing-player?)]
    ;; ======================================================================
    ;; Si la profundidad es 0, devuelva la evaluación del tablero
    ;; ======================================================================
    [(= depth 0) (evaluate-board board maximizing-player?)]
    ;; ======================================================================
    ;; Si es maximizing el jugador
    ;; ======================================================================
    [(= maximizing-player? PLAYER2)
        ;(displayln "max")
     ;; ======================================================================
     ;; Recorrer todos los sucesores
     ;; ======================================================================
     (let loop ((successors (generate-children-boards board PLAYER2))
                (best-move '())
                (best-value alpha))
       (cond
         ;; ======================================================================
         ;; Si no hay sucesores, devolver el valor de la mejor placa
         ;; ======================================================================
         ((null? successors) best-value)
         (else
           ;; ======================================================================
           ;; minimax alfa beta para recorrer todos los sucesores
           ;; ======================================================================
           (let ((value (minimax-alpha-beta (car successors) (- depth 1) PLAYER1 alpha beta best-board)))
             (set! best-value (max best-value value))
             (set! alpha (max alpha best-value))
             (cond
               ((> best-value value)
                (set! best-move (car successors))
                ;; ======================================================================
                ;; Acutualizar el mejor tablero
                ;; ======================================================================
                (set! best-board (car successors))
                (set! best-value value)
                ;(displayln "615")
                ;(displayln best-move)
                ;(displayln best-value)
                ))
             (loop (cdr successors) best-move best-value)))))]
    ;; ======================================================================
    ;; Si minimiza el jugador
    ;; ======================================================================
    [(= maximizing-player? PLAYER1)
    ;(displayln "min")
     ;; ======================================================================
     ;; Recorrer todos los sucesores
     ;; ======================================================================
     (let loop ((successors (generate-children-boards board PLAYER1))
                (best-move '())
                (best-value beta))
       (cond
         [(null? successors) best-value]
         (else
           ;; ======================================================================
           ;; minimax alfa beta para recorrer todos los sucesores
           ;; ======================================================================
           (let ((value (minimax-alpha-beta (car successors) (- depth 1) PLAYER2 alpha beta best-board)))
             (set! best-value (min best-value value))
             (set! beta (min beta best-value))
             (cond
               [(< best-value value)
                (set! best-move (car successors))
                ;; ======================================================================
                ;; Se actualiza el mejor tablero
                ;; ======================================================================
                (set! best-board (car successors))
                (set! best-value value)
                ;(displayln "635")
                ;(displayln best-move)
                ;(displayln best-value)
                ] )
             (loop (cdr successors) best-move best-value)))))]
             ;; ======================================================================
             ;; Se retorna el mejor tablero
             ;; ======================================================================
             ))

;; ======================================================================
;; max-value para minimax alfa beta
;; ======================================================================
(define (max-value board depth alpha beta)
    (cond
        ;; ======================================================================
        ;; Si el estado terminal devuelve la evaluación de la placa
        ;; ======================================================================
        [(terminal-state? board) (evaluate-board board PLAYER2)]
        ;; ======================================================================
        ;; Si la profundidad es 0, devuelva la evaluación del tablero
        ;; ======================================================================
        [(= depth 0) (evaluate-board board PLAYER2)]
        ;; ======================================================================
        ;; Se recorre todos los sucesores
        ;; ======================================================================
        [else (let loop ((successors (generate-children-boards board PLAYER2))
                         (best-value alpha))
                (cond
                    [(null? successors) best-value]
                    [else
                        (let ((value (min-value (car successors) (- depth 1) alpha beta)))
                            ;(displayln "value")
                            ;(displayln value)
                            ;(displayln "successors")
                            ;(displayln (car successors))
                            ;; ======================================================================
                            ;; Se actualiza el mejor valor
                            ;; ======================================================================
                            (set! best-value (max best-value value))
                            ;; ======================================================================
                            ;; Se actualiza el valor alfa
                            ;; ======================================================================
                            (set! alpha (max alpha best-value))
                            ;; ======================================================================
                            ;; Se recorre todos los sucesores
                            ;; ======================================================================
                            (loop (cdr successors) best-value))]))]))

;; ======================================================================
;; min-value para minimax alfa beta
;; ======================================================================
(define (min-value board depth alpha beta)
    (cond
        ;; ======================================================================
        ;; Si el estado terminal devuelve la evaluación de la placa
        ;; ======================================================================
        [(terminal-state? board) (evaluate-board board PLAYER1)]
        ;; ======================================================================
        ;; Si la profundidad es 0, devuelva la evaluación del tablero
        ;; ======================================================================
        [(= depth 0) (evaluate-board board PLAYER1)]
        ;; ======================================================================
        ;; Se recorre todos los sucesores
        ;; ======================================================================
        [else (let loop ((successors (generate-children-boards board PLAYER1))
                         (best-value beta))
                (cond
                    ;; ======================================================================
                    ;; Si no hay sucesores, devolver el valor de la mejor placa
                    ;; ======================================================================
                    [(null? successors) best-value]
                    [else
                        (let ((value (max-value (car successors) (- depth 1) alpha beta)))
                            ;; ======================================================================
                            ;; Se actualiza el mejor valor
                            ;; ======================================================================
                            (set! best-value (min best-value value))
                            ;; ======================================================================
                            ;; Se actualiza el valor beta
                            ;; ======================================================================
                            (set! beta (min beta best-value))
                            ;; ======================================================================
                            ;; Se recorre todos los sucesores
                            ;; ======================================================================
                            (loop (cdr successors) best-value))]))]))

;; ======================================================================
;; alpha beta busca el mejor movimiento
;; ======================================================================
(define (alpha-beta-search board depth)
       ;; ======================================================================
       ;; Se obtienen los sucesores
       ;; ======================================================================
       (define successor (generate-children-boards board PLAYER2))
       ;; ======================================================================
       ;; Se inicializan los valores de alpha y beta
       ;; ======================================================================
       (define alpha -100000)
       (define beta 100000)
       ;; ======================================================================
       ;; Se inicializa el mejor tablero
       ;; ======================================================================
       (define best-board '())

       ;; ======================================================================
       ;; Por cada sucesor
       ;; ======================================================================
       (for ([child successor])
           ;; ======================================================================
           ;; Obtiene el valor del sucesor
           ;; ======================================================================
           (let ((value (minimax-alpha-beta child (- depth 1) PLAYER1 -100000 100000 board)))
               ;; ======================================================================
               ;; Si el valor es mayor que alfa
               ;; ======================================================================
               (cond
                   [(> value alpha)
                       ;; ======================================================================
                       ;; Se actualiza el valor de alfa
                       ;; ======================================================================
                       (set! alpha value)
                       ;; ======================================================================
                       ;; Se actualiza el mejor tablero
                       ;; ======================================================================
                       (set! best-board child)
                       ;(displayln "best board")
                       ;(displayln best-board)
                       ;(displayln "best value")
                       ;(displayln alpha)
                       ])))
                       ;; ======================================================================
                       ;; Retorna el mejor tablero
                       ;; ======================================================================
                       best-board)

;;(alpha-beta-search board 3)
;; ======================================================================
;; Obtiene el mejor movimiento
;; ======================================================================
(define (get-best-move board depth)
    (alpha-beta-search board depth))

;; ======================================================================
;; Distancia promedio de las fichas del jugador a la casilla central
;; ======================================================================
(define (distance-from-center row col)
    (cond
        ;; ======================================================================
        ;; Si la fila es 0, la distancia es 5 + la columna
        ;; ======================================================================
        [(= row 0) (+ 5 col)]
        [(= row 1) (+ 4 col)]
        [(= row 2) (+ 3 col)]
        [(= row 3) (+ 2 col)]
        [(= row 4) (+ 1 col)]
        [else 0]))

;; ======================================================================
;; Distancia media desde el centro del tablero hasta las piezas
;; ======================================================================
(define (average-distance board player)
    (define (average-distance-iter board row col sum)
        (cond
            ;; ======================================================================
            ;; Si la fila es 5, se retorna el promedio
            ;; ======================================================================
            [(= row 5) (/ sum 10)]
            ;; ======================================================================
            ;; Si la columna es 6, se continua con la siguiente fila
            ;; ======================================================================
            [(= col 6) (average-distance-iter board (+ row 1) 0 sum)]
            ;; ======================================================================
            ;; Si la casilla es del jugador, se suma la distancia
            ;; ======================================================================
            [(= (get-cell board row col) player) (+ (average-distance-iter board (+ row 1) 0 sum) (distance-from-center row col))]
            ;; ======================================================================
            ;; Si no, se continua con la iteración
            ;; ======================================================================
            [else (average-distance-iter board (+ row 1) 0 sum)]))
    ;; ======================================================================
    ;; Se llama a la función recursiva
    ;; ======================================================================
    (average-distance-iter board 0 0 0))

;; ======================================================================
;; INTERFAZ GRAFICA
;; ======================================================================
;; Pintar una casilla
;; ======================================================================
(define (paint-square row col player)
    (cond
        ;; ======================================================================
        ;; Si es un movimiento válido, se pinta la casilla
        ;; ======================================================================
        [(valid-move? board row col) (set-cell! board row col player)]
        [else (displayln "paiting in Invalid move")]))
;; ======================================================================
;; Color de relleno para una pieza
;; ======================================================================
(define (piece-image piece)
    (cond
        ;; ======================================================================
        ;; Si la pieza es del jugador 1, se pinta de rojo
        ;; ======================================================================
        [(= piece PLAYER1) (square 50 "solid" "red")]
        ;; ======================================================================
        ;; Si la pieza es del jugador 2, se pinta de azul
        ;; ======================================================================
        [(= piece PLAYER2) (square 50 "solid" "blue")]
        ;; ======================================================================
        ;; Si no, se pinta de blanco
        ;; ======================================================================
        [else (square 50 "solid" "white")]))

;; ======================================================================
;; Imagen del tablero
;; ======================================================================
(define (board-image size)
    (define (board-image-iter board row col)
        ;; ======================================================================
        ;; Si la fila es 5, se retorna el tablero
        ;; ======================================================================
        (if (= row 5)
            (empty-scene size size)
            ;; ======================================================================
            ;; Si la columna es 6, se continua con la siguiente fila
            ;; ======================================================================
            (if (= col 6)
                ;; ======================================================================
                ;; Se llama a la función recursiva
                ;; ======================================================================
                (board-image-iter board (+ row 1) 0)
                ;; ======================================================================
                ;; Se pinta la casilla
                ;; ======================================================================
                (place-image (piece-image (get-cell board row col)) (* col 50) (* row 50) (board-image-iter board (+ col 1) (+ col 1))))))
    ;; ======================================================================
    ;; Se llama a la función recursiva
    ;; ======================================================================
    (board-image-iter board 0 0))

;; ======================================================================
;; Frame que contiene el tablero
;; ======================================================================
(define frame (new frame% [label "DARA"]
    [width 700]
    [height 630]))

;; ======================================================================
;; Tener un mensaje en el frame
;; ======================================================================
(define msg (new message% [parent frame]
                          [label "Choose game mode"]))

;; ======================================================================
;; Hacer un botón en el frame
;; ======================================================================
(new button% [parent frame]
             [label "Player vs IA"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (game-loop-1vIA board))])

;; Start game button
(new button% [parent frame]
             [label "Player vs Player"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (game-loop-1v1 board))])

;; Start game button
(new button% [parent frame]
             [label "Random vs IA"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (game-loop-IAvIA board))])

;; ======================================================================
;; Hacer un canvas en el frame
;; ======================================================================
(define canvas (new canvas% [parent frame]
                            [paint-callback (lambda (canvas dc)
                            (let loop ([i 0] [j 0])
                                ;; ======================================================================
                                ;; Si la fila es 5, se retorna el tablero
                                ;; ======================================================================
                                (when (< i 5)
                                ;; ======================================================================
                                ;; Si la columna es 6, se continua con la siguiente fila
                                ;; ======================================================================
                                    (when (< j 6)
                                    ;; ======================================================================
                                    ;; Se pinta la casilla
                                    ;; ======================================================================
                                        (if (= (get-cell board i j) PLAYER1)
                                        ;; ======================================================================
                                        ;; Si la pieza es del jugador 1, se pinta de rojo
                                        ;; ======================================================================
                                            (send dc set-brush (make-object brush% "red"))
                                            ;; ======================================================================
                                            ;; Si la pieza es del jugador 2, se pinta de azul
                                            ;; ======================================================================
                                            (if (= (get-cell board i j) PLAYER2)
                                                ;; ======================================================================
                                                ;; Si la pieza es del jugador 2, se pinta de azul
                                                ;; ======================================================================
                                                (send dc set-brush (make-object brush% "blue"))
                                                ;; ======================================================================
                                                ;; Si no, se pinta de blanco
                                                ;; ======================================================================
                                                (send dc set-brush (make-object brush% "white"))))
                                        (send dc draw-rectangle (* j 50) (* i 50) 50 50)
                                        ;;add number to the square
                                              (let ([x (* j 50)]
              [y (* i 50)])
                                        (send dc set-brush (make-object brush% "black"))
                                         (send dc draw-text (number->string i) x y)
                                      (send dc draw-text (number->string j) x (+ y 20))

                                        ;; ======================================================================
                                        ;; Se llama a la función recursiva
                                        ;; ======================================================================
                                        (loop i (+ j 1))))
                                    ;; ======================================================================
                                    ;; Se llama a la función recursiva
                                    ;; ======================================================================
                                    (loop (+ i 1) 0))))]))



;; ======================================================================
;; Derivar una nueva clase de lienzo (una ventana de dibujo) para manejar eventos
;; ======================================================================
(define my-canvas%
  (class canvas% ; The base class is canvas%
    (send canvas set-width 300)
    ;; ======================================================================
    ;; Defina el método de anulación para manejar los eventos del mouse
    ;; ======================================================================
    (define/override (on-event event)
      (send msg set-label "Canvas mouse"))
    ;; ======================================================================
    ;; Definir método de anulación para manejar eventos de teclado
    ;; ======================================================================
    (define/override (on-char event)
      (send msg set-label "Canvas keyboard"))
    ;; ======================================================================
    ;; Llame a la superclase init, pasando todos los argumentos init
    ;; ======================================================================
    (super-new)))
;; ======================================================================
;; Actualización, descarga y rendimiento del lienzo
;; ======================================================================
(define (refresh)
            (send canvas refresh-now)
            (send canvas flush)
            (yield)
            )

;; ======================================================================
;; Version terminal
;; ======================================================================
(define (display-board board)
    (displayln "  0 1 2 3 4 5")
    ;; ======================================================================
    ;; Se recorre el tablero
    ;; ======================================================================
    (for ([i (in-range 5)])
        (display i)
        (display " ")
        ;; ======================================================================
        ;; Se recorre la fila
        ;; ======================================================================
        (for ([j (in-range 6)])
            (display (get-cell board i j))
            (display " "))
        (displayln "")))
;; ======================================================================
;; Ciclo del juego PLAYER1 v PLAYER2
;; ======================================================================
(define (game-loop-1v1 board)
    ;; ======================================================================
    ;; Si el juego termina, se muestra el mensaje
    ;; ======================================================================
    (if (game-over? board)
        (displayln "Game over")
        (begin
            (display-board board)
            (refresh)
            ;; ======================================================================
            ;; Se mueve la pieza del jugador 1
            ;; ======================================================================
            (move-piece-in-direction board PLAYER1)
            (refresh)
            ;; ======================================================================
            ;; Si el juego termina, se muestra el mensaje
            ;; ======================================================================
            (if (game-over? board)
                (displayln "Game over")
                (begin
                    (refresh)
                    ;; ======================================================================
                    ;; Se mueve la pieza del jugador 2
                    ;; ======================================================================
                    (move-piece-in-direction board PLAYER2)
                    (refresh)
                    (game-loop-1v1 board))))))
;; ======================================================================
;; Ciclo de juego con PLAYER1 v IA (alpha beta)
;; ======================================================================
    (define (game-loop-1vIA board)
    ;; ======================================================================
    ;; Si el juego termina, se muestra el mensaje
    ;; ======================================================================
    (if (game-over? board)
        (displayln "Game over")
        (begin
            (display-board board)
            (refresh)
            ;; ======================================================================
            ;; Se mueve la pieza del jugador 1
            ;; ======================================================================
            (move-piece-in-direction board PLAYER1)
            (refresh)
            ;; ======================================================================
            ;; Si el juego termina, se muestra el mensaje
            ;; ======================================================================
            (if (game-over? board)
                (displayln "Game over")
                (begin
                    (refresh)

                    ;; ======================================================================
                    ;; Inicia el timer para ver cuanto tarda en hacer el movimiento
                    ;; ======================================================================
                    (let ([start (current-inexact-milliseconds)])
                    (let ([new-board (get-best-move board 2)])
                    ;; ======================================================================
                    ;; Acutar los valores del tablero con el nuevo tablero
                    ;; ======================================================================
                    (for ([i (in-range 5)])
                        ;; ======================================================================
                        ;; Se recorre la fila
                        ;; ======================================================================
                        (for ([j (in-range 6)])
                            ;; ======================================================================
                            ;; Se actualiza el valor de la casilla
                            ;; ======================================================================
                            (set-cell! board i j (get-cell new-board i j)))))
                                        (refresh)
                      ;; ======================================================================
                      ;; Se termina el timer
                      ;; ======================================================================
                      (let ([end (current-inexact-milliseconds)])
                        ;; ======================================================================
                        ;; Se muestra el tiempo que tardó en hacer el movimiento
                        ;; ======================================================================
                        (displayln (- end start))
                                        (game-loop-1vIA board))))))))



;; ======================================================================
;; IA player1 movimientos aleatorios v IA player2 alpha beta
;; ======================================================================
(define (game-loop-IAvIA board)
    ;; ======================================================================
    ;; Si el juego termina, se muestra el mensaje
    ;; ======================================================================
    (if (game-over? board)
        (displayln "Game over")
        (begin
            (display-board board)
            (refresh)
            ;; ======================================================================
            ;; IA player1 movimientos aleatorios
            ;; ======================================================================
            (ia-random-move board PLAYER1)
            (refresh)
            ;; ======================================================================
            ;; Inicia el timer para ver cuanto tarda en hacer el movimiento
            ;; ======================================================================
            (let ([start (current-inexact-milliseconds)])
            ;; ======================================================================
            ;; Si el juego termina, se muestra el mensaje
            ;; ======================================================================
            (if (game-over? board)
                (displayln "Game over")
                (begin
                    (refresh)
                    ;; ======================================================================
                    ;; Se establece el nuevo tablero
                    ;; ======================================================================
                    (let ([new-board (get-best-move board 2)])
                    ;; ======================================================================
                    ;; Se actualiza el tablero con los valores del nuevo tablero
                    ;; ======================================================================
                    (for ([i (in-range 5)])
                    ;; ======================================================================
                    ;; Se recorre la fila
                    ;; ======================================================================
                        (for ([j (in-range 6)])
                        ;; ======================================================================
                        ;; Se actualiza el valor de la casilla
                        ;; ======================================================================
                            (set-cell! board i j (get-cell new-board i j)))))

                                        (refresh)
                                        ;; ======================================================================
                                        ;; Se termina el timer
                                        ;; ======================================================================
                                        (let ([end (current-inexact-milliseconds)])
                                        ;; ======================================================================
                                        ;; Se muestra el tiempo que tardó en hacer el movimiento
                                        ;; ======================================================================
                                        (displayln (- end start))
                                        (game-loop-IAvIA board))))))))



;; main

(initial-board)

;; test board
;(set-cell! board 0 0 PLAYER1)
;(set-cell! board 0 1 PLAYER1)
;(set-cell! board 0 3 PLAYER1)
;(set-cell! board 1 2 PLAYER2)
;(set-cell! board 1 0 PLAYER2)
;(set-cell! board 2 1 PLAYER2)
;(set-cell! board 2 1 PLAYER2)

(send frame show #t)
(send canvas refresh-now)
(send canvas flush)
(yield)

;(define copy (copy-board board))
;(get-all-valid-pieces-to-moves board PLAYER2)
;(define A (generate-children board PLAYER2))
;(game-loop-1v1 board)

;(define best-board (vector (vector 0 0 0 0 0 0) (vector 0 0 0 0 0 0) (vector 0 0 0 0 0 0) (vector 0 0 0 0 0 0) (vector 0 0 0 0 0 0))) ;; 5 rows and 6 columns

;(define A(minimax-alpha-beta board 3 PLAYER2 -100000 100000 best-board))

;(define move (alpha-beta-search board 3 PLAYER2))

;(game-loop-1vIA board)

;(game-loop-IAvIA board)