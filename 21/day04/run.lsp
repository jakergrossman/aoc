; Day 4: Giant Squid

(load "../../include/lisp/common.lsp")

(defun parse-boards (lines)
  (cond
    ((null lines) nil)
    (t
      (let ((board (reduce 'append (loop :for n :from 0 :below 5 :collect (parse-integers (nth n lines) #\Space))))
            (next (nthcdr 6 lines)))
        (append (list board) (parse-boards next))))))

(setq lines (get-input "input.txt"))
(setq numbers (parse-integers (car lines) #\,))
(setq boards (parse-boards (cddr lines)))

; convert raw boards to board-state indicators
(setq board-states
  (map 'list
    (lambda (xs)
      (map 'list (lambda (entry) (list entry 0)) xs))
    boards))

; update every board with the next number
(defun call-number (num states)
  (map 'list
    (lambda (x)
      (substitute-if (list num 1) (lambda (y) (eq y num)) x :key #'car :count 1))
    states))

; collect specific indices of the board
; test that all specified positions are marked
(defun test-positions (state positions)
  (notany #'null (map 'list (lambda (x) (eq (cadr (nth x state)) 1)) positions)))

(defun check-board-state (state)
  (let ((cols (loop :for n :below 5 :collect
          (test-positions state (loop :for m :below 5 :collect (+ (* 5 m) n)))))
        (rows (loop :for n :below 5 :collect
          (test-positions state (loop :for m :below 5 :collect (+ (* 5 n) m))))))
    (notevery #'null (append cols rows))))

; find first winning board
(defun bingo1 (states ns)
  (let* ((next-num (car ns))
        (new-states (call-number next-num states)))
    (loop :for x :below (length new-states)
      do
        (let ((state (nth x new-states)))
          (cond
            ((check-board-state state) (return-from bingo1 (list next-num state))))))

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
  (*
    (car result)
    (reduce
      (lambda (x y)
        (cond
          ((null y) x)
          ((eq (cadr y) 0) (+ x (car y)))
          (t x)))
      (cadr result)
      :initial-value 0)))

(format t "Part 1: ~d~%" (final-score (bingo1 board-states numbers)))

(format t "Part 2: ~d~%" (final-score (bingo2 board-states numbers)))
