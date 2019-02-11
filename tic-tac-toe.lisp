;; Values for computer and player in game board
(defvar *computer* 10)
(defvar *opponent*  1)

;; Lists holding corners, sides, and diagonal positions in game board
(defvar *corners*
  '(1 3 7 9))
(defvar *sides*
  '(2 4 6 8))
(defvar *diags*
  '((1 5 9) (3 5 7)))

;; List of all win combos (rep. as index in board list)
(defvar *win-combos*
  '((1 2 3) (4 5 6) (7 8 9) ;; rows
    (1 4 7) (2 5 8) (3 6 9) ;; cols
    (1 5 9) (3 5 7))) ;; diags


;; Game board functions

(defun make-board ()
  "Create list of 0s from 1-9 to represent game board"
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter (v)
  "Convert given int to X, O, or blank to visualize game board"
  (cond ((equal v  1) "O")
        ((equal v 10) "X")
        (t " ")))

(defun print-row (x y z)
  "Convert args to letter with convert-to-letter and display as a row in the board"
  (format t "~&  ~A | ~A | ~A"
          (convert-to-letter x)
          (convert-to-letter y)
          (convert-to-letter z)))

(defun print-board (board)
  "Print given game board as a visual representation of the game"
  (format t "~%~%")
  (print-row
   (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~& -----------")
  (print-row
   (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~& -----------")
  (print-row
   (nth 7 board) (nth 8 board) (nth 9 board)))


;; Functions for checking the winner

(defun sum-triplet (board triplet)
  "Adds the values of given indices in board"
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(defun compute-sums (board)
  "Finds the sums of the values at each win combo in the board"
  (mapcar #'(lambda (triplet)
              (sum-triplet board triplet))
          *win-combos*))

(defun winner-p (board)
  "Checks whether any of the win combos add up to 3 or 30, resulting in a win for
either the opponent or computer"
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
        (member (* 3 *opponent*) sums))))

(defun board-full-p (board)
  (not (member 0 board)))


;; Move functions

(defun make-move (player pos board)
  "Sets given position to the given player's token, then returns the board"
  (setf (nth pos board) player)
  board)

(defun opponent-move (board)
  "Player's move: takes input, checks it's valid, then puts it in the board and
calls the computer move with the new board"
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move
                     *opponent*
                     pos
                     board)))
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&You win!"))
          ((board-full-p new-board) (format t "~&That's a tie."))
          (t
           (computer-move new-board)))))

(defun computer-move (board)
  "Computer move: runs functions to find best move, makes new board with best move,
explains the strategy, then runs the opponent move function"
  (let* ((best-move (choose-best-move board))
         (pos (first best-move))
         (strat (second best-move))
         (new-board (make-move
                     *computer*
                     pos
                     board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%~%" strat)
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~%I win!"))
          ((board-full-p new-board) (format t "~%That's a tie."))
          (t (opponent-move new-board)))))

(defun read-a-legal-move (board)
  "Confirms that a player's move choice is valid"
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 1 pos 9)))
           (format t "~&Invalid input.")
           (read-a-legal-move board))
          ((not (zerop (nth pos board)))
           (format t "~&That space is already occupied.")
           (read-a-legal-move board))
          (t pos))))


