#lang racket

;; Dara-Nigerian-Game

;; This is a game of Dara-Nigerian-Game played by two players.
;; The game is played on a 5 rows and 6 columns board.
;; The players take turns moving a piece into a space. The player that gets three
;; pieces in a row or column removes a piece from the opponent.
;; If a player cannot make 3 in a row loses the game.


;; Constants
(define BOARD-ROWS 5) ;; number of rows on the board
(define BOARD-COLS 6) ;; number of columns on the board
(define PLAYER1 1) ;; number 1 presents player 1
(define PLAYER2 2) ;; pnumber 2 presents player 2
;; 0 presents an empty cell on the board

;; Board representation
(define board (vector (vector 0 0 0 0 0 0) (vector 0 0 0 0 0 0) (vector 0 0 0 0 0 0) (vector 0 0 0 0 0 0) (vector 0 0 0 0 0 0))) ;; 5 rows and 6 columns

;; Functions

;; Set a cell on the board to a player
(define (set-cell! board row col player)
  (vector-set! (vector-ref board row) col player))

;; Get a cell on the board
(define (get-cell board row col)
    (vector-ref (vector-ref board row) col))

;; Get a row on the board
(define (get-row board row) ;; row is a number
    (vector-ref board row))

;; Get a column on the board
(define (get-col board col) ;; col is a number
    (vector (vector-ref (vector-ref board 0) col) (vector-ref (vector-ref board 1) col) (vector-ref (vector-ref board 2) col) (vector-ref (vector-ref board 3) col) (vector-ref (vector-ref board 4) col)))

;; Check if a cell is empty returns #t if empty and #f if not empty
(define (check-empty board row col) ;; row from 0 to 4 and col from 0 to 5
    (if (= (get-cell board row col) 0)
        #t
        #f)) 

;; place a piece on the board randomly
(define (place-ramdomly board player)
    (define row (random 0 4))
    (define col (random 0 5))
    (if (check-empty board row col) ;; check if the cell is empty
        (set-cell! board row col player)
        (place-ramdomly board player)))

;; place a piece on the board choose by the player
(define (place-piece board player)
    (display "Player ")
    (displayln player)
    (display "Enter column: ")
    (define col (read))
    (display "Enter row: ")
    (define row (read))
    (if (check-empty board row col) ;;  if the cell is empty
        (set-cell! board row col player)
        (place-piece board player)))

;; remove a piece from the board
(define (remove-piece board row col) ;; row from 0 to 4 and col from 0 to 5
    (set-cell! board row col 0))

;; move a piece on the board to a new cell remove the piece from the old cell
(define (move-piece board row col row2 col2 player) ;; row from 0 to 4 and col from 0 to 5
    (if (check-empty board row2 col2) ;; check if the cell is empty
        (begin
            (set-cell! board row2 col2 player)
            (remove-piece board row col))
        (displayln "Invalid move")))

;; move a piece in one direction up, down, left, right.
;; row col current position of the piece row2 col2 new position of the piece
(define (move-piece-one-direction board row col row2 col2 player) ;; row from 0 to 4 and col from 0 to 5
    (if (and (>= row2 0) (>= col2 0) (<= row2 BOARD-ROWS) (<= col2 BOARD-COLS))
        (move-piece board row col row2 col2 player)
        (displayln "Invalid move")))

;; move a piece one row up
(define (move-piece-up board row col player)
    (move-piece-one-direction board row col (- row 1) col player))

;; move a piece one row down
(define (move-piece-down board row col player)
    (move-piece-one-direction board row col (+ row 1) col player))

;; move a piece one column left
(define (move-piece-left board row col player)
    (move-piece-one-direction board row col row (- col 1) player))

;; move a piece one column right
(define (move-piece-right board row col player)
    (move-piece-one-direction board row col row (+ col 1) player))

;; player choose a move to make from -> up, down, left, right
(define (choose-move board player)
    (display "Choose piece to move for player ")
    (displayln player)
    (display "Enter row 0-4: ")
    (define row (read))
    (display "Enter col 0-5: ")
    (define col (read))
    (display "Enter move: ")
    (displayln "1. Move piece up")
    (displayln "2. Move piece down")
    (displayln "3. Move piece left")
    (displayln "4. Move piece right")
    (define move (read))
    (cond
        [(= move 1) (move-piece-up board row col player)]
        [(= move 2) (move-piece-down board row col player)]
        [(= move 3) (move-piece-left board row col player)]
        [(= move 4) (move-piece-right board row col player)]
        [else (displayln "Invalid move")]))

;; funtions to check if a player has make 3 in a row or 3 in a column

;; 3 pieces in a row to the right of the piece
;; returns #t if 3 in a row and #f if not
(define (3-in-a-row-right board row col player)
    (if (and (= (get-cell board row (+ col 1)) player) (= (get-cell board row (+ col 2)) player))
        #t
        #f))

;; 3 pieces in a row to the left of the piece
;; returns #t if 3 in a row and #f if not
(define (3-in-a-row-left board row col player)
    (if (and (= (get-cell board row (- col 1)) player) (= (get-cell board row (- col 2)) player))
        #t
        #f))

;; 3 pieces in a row to the left and right of the piece
;; returns #t if 3 in a row and #f if not
(define (3-in-a-row-center board row col player)
    (if (and (= (get-cell board row (- col 1)) player) (= (get-cell board row (+ col 1)) player))
        #t
        #f))

;; 3 pieces in a column up from the piece
;; returns #t if 3 in a column and #f if not
(define (3-in-a-col-up board row col player)
    (if (and (= (get-cell board (- row 1) col) player) (= (get-cell board (- row 2) col) player))
        #t
        #f))

;; 3 pieces in a column down from the piece
;; returns #t if 3 in a column and #f if not
(define (3-in-a-col-down board row col player)
    (if (and (= (get-cell board (+ row 1) col) player) (= (get-cell board (+ row 2) col) player))
        #t
        #f))

;; 3 pieces in a column up and down from the piece
;; returns #t if 3 in a column and #f if not
(define (3-in-a-col-center board row col player)
    (if (and (= (get-cell board (- row 1) col) player) (= (get-cell board (+ row 1) col) player))
        #t
        #f))

;; check if there are 3 pieces in a row or column
;; return true if there are 3 pieces in a row
(define (3-in-a-row board row col player)
    (if (or (3-in-a-row-right board row col player) (3-in-a-row-left board row col player) (3-in-a-row-center board row col player))
        #t
        #f))

;; check if there are 3 pieces in a column
;; return true if there are 3 pieces in a column
(define (3-in-a-col board row col player)
    (if (or (3-in-a-col-up board row col player) (3-in-a-col-down board row col player) (3-in-a-col-center board row col player))
        #t
        #f))

;; check if there are 3 pieces in a row or column
;; return true if there are 3 pieces in a row or column
(define (3-pieces? board row col player)
    (if (or (3-in-a-row board row col player) (3-in-a-col board row col player))
        #t
        #f))

;; count pieces remaining for player
;; returns number of pieces remaining
(define (count-pieces board player)
    (define (count-pieces-iter board player count)
        (if (= count 5)
            0
            (if (= (get-cell board 0 count) player)
                (+ 1 (count-pieces-iter board player (+ count 1)))
                (count-pieces-iter board player (+ count 1)))))
    (count-pieces-iter board player 0))

;;check if player lost
;; returns true if player has only 2 pieces left
(define (game-over? board)
    (if (or (= (count-pieces board PLAYER1) 2) (= (count-pieces board PLAYER2) 2))
        #t
        #f))

;;check wich player won
;; returns player who has 2 pieces left
(define (who-won board)
    (if (= (count-pieces board PLAYER1) 2)
        PLAYER2
        PLAYER1))

;;next player
;; returns next player
(define (next-player player)
    (if (= player PLAYER1)
        PLAYER2
        PLAYER1))

;; game loop
;; iterates through game
;; displays board and player turn
;; checks if game is over
;; if game is over displays winner
(define (play-game-iter board turn-player)
    (if (game-over? board)
        (displayln (who-won board))
        (begin
            (display-board-frame board)
            (displayln turn-player)
            (choose-move board turn-player)


            (play-game-iter board (next-player turn-player)))))

;; display board
;; displays board
(define (display-board-frame board)
    (displayln "   0 1 2 3 4 5 ")
    (display "0") (displayln (get-row board 0))
    (display "1") (displayln (get-row board 1))
    (display "2") (displayln (get-row board 2))
    (display "3") (displayln (get-row board 3))
    (display "4") (displayln (get-row board 4)))

;;play game
;; places pieces for both players

;; manualy place piece for each player
(define (take-turns-place-piece)
    (place-piece board PLAYER1)
    (place-piece board PLAYER2)
    (display-board-frame board))

;; randomly place piece for each player
(define (take-turns-place-piece-random)
    (place-ramdomly board PLAYER1)
    (place-ramdomly board PLAYER2)
    (display-board-frame board))

;; each player placer 12 pieces
(define (place-pieces number-of-pieces)
    (take-turns-place-piece)
    (if (= number-of-pieces 2)
        (displayln "fase 1 done")
        (place-pieces (+ number-of-pieces 1))))

;; place 12 pieces for each player randomly
(define (place-pieces-random number-of-pieces)
    (take-turns-place-piece-random)
    (if (= number-of-pieces 12)
        (displayln "fase 1 done")
        (place-pieces-random (+ number-of-pieces 1))))

;; choose placement mode manual or random
(define (choose-placement-mode)
    (displayln "choose placement mode")
    (displayln "1. manual")
    (displayln "2. random")
    (define mode (read))
    (if (= mode 1)
        (place-pieces 0)
        (place-pieces-random 0)))

(define (play-game board)
    (choose-placement-mode) ;; choose placement mode manual or random
    (define turn-player PLAYER1) ;; set first player
    (play-game-iter board turn-player)) ;; start game loop

(display-board-frame board)
(play-game board)


;;funtions for testing

;(set-cell! board 0 0 PLAYER1)
;(set-cell! board 0 1 PLAYER1)
;(set-cell! board 0 2 PLAYER1)

;(set-cell! board 2 2 PLAYER2)
;(set-cell! board 3 2 PLAYER2)
;(set-cell! board 4 2 PLAYER2)

;(displayln board)
;(displayln (3-in-a-row-right board 0 0 PLAYER1))

;(place-ramdomly board PLAYER1)
;(place-ramdomly board PLAYER2)

;(place-piece board PLAYER1)
;(set-cell! board 0 0 PLAYER1)
; (vector-fill! (vector-ref board 0) 5)
; (vector? board)
; (define list (vector-ref board 0))
; (define cell (vector-ref (vector-ref board 0) 0))
; (displayln board)
; (displayln list)
; (displayln cell)
; (vector? list)
; (vector? cell)
; (vector-set! (vector-ref board 0) 0 9)
; (displayln board)