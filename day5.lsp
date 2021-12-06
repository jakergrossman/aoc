; process an input line, returning ((x1 y1) (x2 y2))
(defun process-line (line)
  (setq left-comma (position #\, line))
  (setq left-space (position #\Space line))
  (setq right-comma (position #\, line :from-end T))
  (setq right-space (position #\Space line :from-end T))

  (setq x1 (subseq line 0 left-comma))
  (setq y1 (subseq line (+ 1 left-comma) left-space))

  (setq x2 (subseq line (+ 1 right-space) right-comma))
  (setq y2 (subseq line (+ 1 right-comma)))

  (list (list (parse-integer x1) (parse-integer y1)) (list (parse-integer x2) (parse-integer y2))))

(defun get-input (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
      while (and line (> (length line) 0))
      collect (process-line line))))

; check if a movement is in a straight line
(defun is-straight (entry)
  (not
    ; x1 != x2 && y1 != y2 i.e.; not a straight line
    (and
      (not (eq (caar entry) (caadr entry)))
      (not (eq (cadar entry) (cadadr entry))))))

; create a 'Width' x 'Height' matrix filled with 'value'
(defun mtx (width height value) (loop :for n :below height :collect (loop :for m :below width :collect value)))

; only straight lines
(setq input1 (remove-if-not #'is-straight (get-input "inputs/input5.txt")))
(setq state1 (mtx 1000 1000 0))

; all lines
(setq input2 (get-input "inputs/input5.txt"))
(setq state2 (mtx 1000 1000 0))

; increment numbers along a line
(defun draw (delta-x delta-y pos-x pos-y state)
  (setq current-value (nth pos-x (nth pos-y state)))
  (setf (nth pos-x (nth pos-y state)) (+ 1 current-value))
  (cond
    ((and (eq 0 delta-x) (eq 0 delta-y)) state)
    (t
      (setq step-x
        (cond
          ((< delta-x 0) -1)
          ((> delta-x 0) 1)
          (t 0)))
      (setq step-y
        (cond
          ((< delta-y 0) -1)
          ((> delta-y 0) 1)
          (t 0)))

      (draw (- delta-x step-x) (- delta-y step-y) (+ pos-x step-x) (+ pos-y step-y) state))))

(defun run-input (input state)
  (loop :for n :below (length input)
    do
    (setq movement (nth n input))
    (setq x1 (caar movement))
    (setq y1 (cadar movement))
    (setq x2 (caadr movement))
    (setq y2 (cadadr movement))

    (setq delta-x (- x2 x1))
    (setq delta-y (- y2 y1))
    (draw delta-x delta-y x1 y1 state)))

(defun count-values (xs)
  (cond
    ((null xs) 0)
    (t (cond ((> (car xs) 1) (+ 1 (count-values (cdr xs)))) (t (count-values (cdr xs)))))))

(run-input input1 state1)
(run-input input2 state2)

; part 1
(reduce '+ (map 'list #'count-values state1))

; part 2
(reduce '+ (map 'list #'count-values state2))
