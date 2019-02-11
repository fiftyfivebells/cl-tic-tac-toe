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
