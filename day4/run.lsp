#!/usr/bin/gcl -f

(defun skip-char (line c)
  (cond
    ((null line) "")
    ((eq (char line 0) c) (skip-char (subseq line 1) c))
    (t line)))

(defun parse-nums (line delim)
  (let* ((skip-start (skip-char line delim))
        (delim-pos (position delim skip-start)))
    (cond
      ((null delim-pos)
        (list (parse-integer skip-start)))
      (t
        (append
          (list (parse-integer (subseq skip-start 0 delim-pos)))
          (parse-nums (subseq skip-start (+ 1 delim-pos)) delim))))))

(defun parse-boards (lines)
  (cond
    ((null lines) nil)
    (t
      (let ((board (reduce 'append (loop :for n :from 0 :below 5 :collect (parse-nums (nth n lines) #\Space))))
            (next (nthcdr 6 lines)))
        (append (list board) (parse-boards next))))))

(defun get-input (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
      while line
      collect line)))

(setq lines (get-input "input.txt"))
(setq numbers (parse-nums (car lines) #\,))
(setq boards (parse-boards (cddr lines)))

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
  (let
    ((cols (map 'list
      (lambda (x)
        (collect-positions state '() (loop :for n :from 0 :to 4 :collect (+ (* 5 n) x))))
      (loop :for n :from 0 :to 4 :collect n))))
    (cond
      ; for each col
      ((eq 5 (reduce '+ (map 'list 'cadr (nth 0 cols)))) T)
      ((eq 5 (reduce '+ (map 'list 'cadr (nth 1 cols)))) T)
      ((eq 5 (reduce '+ (map 'list 'cadr (nth 2 cols)))) T)
      ((eq 5 (reduce '+ (map 'list 'cadr (nth 3 cols)))) T)
      ((eq 5 (reduce '+ (map 'list 'cadr (nth 4 cols)))) T)
      (t NIL))))

(defun check-rows (state)
  (let
    ((rows (map 'list
      (lambda (x)
        (collect-positions state '() (loop :for n :from 0 :to 4 :collect (+ (* 5 x) n))))
      (loop :for n :from 0 :to 4 :collect n))))
    (cond
      ; for each row
      ((eq 5 (reduce '+ (map 'list 'cadr (nth 0 rows)))) T)
      ((eq 5 (reduce '+ (map 'list 'cadr (nth 1 rows)))) T)
      ((eq 5 (reduce '+ (map 'list 'cadr (nth 2 rows)))) T)
      ((eq 5 (reduce '+ (map 'list 'cadr (nth 3 rows)))) T)
      ((eq 5 (reduce '+ (map 'list 'cadr (nth 4 rows)))) T)
      (t NIL))))

; find first winning board
(defun bingo1 (states ns)
  (let* ((next-num (car ns))
        (new-states (call-number next-num states)))
    (loop :for x :below (length new-states)
      do
        (setq state (nth x new-states))
        (cond
          ((check-board-state state) (return-from bingo1 (list next-num state)))))

    (bingo1 new-states (cdr ns))))

; find last winning board
(defun bingo2 (states ns)
  (let* ((next-num (car ns))
        (new-states (call-number next-num states)))
    (cond
      ((eq (length states) 1)
        (cond
          ((check-board-state (car new-states)) (return-from bingo2 (list next-num (car new-states))))
          (t (bingo2 new-states (cdr ns)))))
      (t
        (bingo2 (remove-if #'check-board-state new-states) (cdr ns))))))

; given the winning number and board, calculate the final score
(defun final-score (result)
  (let
    ((unset-values (map 'list
      (lambda (x)
        (cond
          ((eq (cadr x) 0) (car x))
          (t 0)))
      (cadr result))))

    (* (car result) (apply '+ unset-values))))

(format t "Part 1: ~d~%" (final-score (bingo1 board-states numbers)))

(format t "Part 2: ~d~%" (final-score (bingo2 board-states numbers)))
