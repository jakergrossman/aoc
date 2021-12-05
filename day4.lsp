(load "inputs/input4")

; convert raw boards to board-state indicators
(setq board-states
  (map 'list
    (lambda (xs)
      (map 'list (lambda (entry) (list entry 0)) xs))
    boards))

; update every board with the next number
(defun call-number (x states)
  (map 'list
    (lambda (state)
      (map 'list
        (lambda (entry)
          (cond
            ((eq x (car entry)) (list (car entry) 1))
            (t entry)))
        state))
    states))

; collect specific indices of the board
(defun collect-positions (state result positions)
  (cond
    ((null positions) result)
    (t
      (collect-positions state
        (append result (list (nth (car positions) state)))
        (cdr positions)))))

(defun check-board-state (state)
  (cond
    ((check-rows state) T)
    ((check-cols state) T)
    (t NIL)))

(defun check-cols (state)
  (setq cols
    (map 'list
      (lambda (x)
        (collect-positions state '() (loop :for n :from 0 :to 4 :collect (+ (* 5 n) x))))
      (loop :for n :from 0 :to 4 :collect n)))
  (cond
    ; for each col
    ((eq 5 (reduce '+ (map 'list 'cadr (nth 0 cols)))) T)
    ((eq 5 (reduce '+ (map 'list 'cadr (nth 1 cols)))) T)
    ((eq 5 (reduce '+ (map 'list 'cadr (nth 2 cols)))) T)
    ((eq 5 (reduce '+ (map 'list 'cadr (nth 3 cols)))) T)
    ((eq 5 (reduce '+ (map 'list 'cadr (nth 4 cols)))) T)
    (t NIL)))

(defun check-rows (state)
  (setq rows
    (map 'list
      (lambda (x)
        (collect-positions state '() (loop :for n :from 0 :to 4 :collect (+ (* 5 x) n))))
      (loop :for n :from 0 :to 4 :collect n)))

  (cond
    ; for each row
    ((eq 5 (reduce '+ (map 'list 'cadr (nth 0 rows)))) T)
    ((eq 5 (reduce '+ (map 'list 'cadr (nth 1 rows)))) T)
    ((eq 5 (reduce '+ (map 'list 'cadr (nth 2 rows)))) T)
    ((eq 5 (reduce '+ (map 'list 'cadr (nth 3 rows)))) T)
    ((eq 5 (reduce '+ (map 'list 'cadr (nth 4 rows)))) T)
    (t NIL)))

; find first winning board
(defun bingo1 (states ns)
  (setq next-num (car ns))
  (setq new-states (call-number next-num states))
  (loop :for x :below (length new-states)
    do
      (setq state (nth x new-states))
      (cond
        ((check-board-state state) (return-from bingo1 (list next-num state)))))

  (bingo1 new-states (cdr ns)))

; find last winning board
(defun bingo2 (states ns)
  (setq next-num (car ns))
  (setq new-states (call-number next-num states))
  (cond
    ((eq (length states) 1)
      (cond
        ((check-board-state (car new-states)) (return-from bingo2 (list next-num (car new-states))))
        (t (bingo2 new-states (cdr ns)))))

    (t
      (bingo2 (remove-if #'check-board-state new-states) (cdr ns)))))

; given the winning number and board, calculate the final score
(defun final-score (result)
  (setq unset-values (map 'list
    (lambda (x)
      (cond
        ((eq (cadr x) 0) (car x))
        (t 0)))

    (cadr result)))

  (* (car result) (apply '+ unset-values)))

; part 1
(final-score (bingo1 board-states numbers))
(final-score (bingo2 board-states numbers))
