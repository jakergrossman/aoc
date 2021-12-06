#!/usr/bin/gcl -f

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

; only straight lines
(setq input2 (get-input "input.txt"))
(setq input1 (remove-if-not #'is-straight input2))
(setq state1 (make-array '(1000000) :initial-element 0))

; all lines
(setq state2 (make-array '(1000000) :initial-element 0))

(defun draw2 (delta-x delta-y pos-x pos-y state)
  (setq pos (+ (* pos-x 1000) pos-y))
  (setq current-value (aref state pos))
  (setf (aref state pos) (+ 1 current-value))
  (cond
    ((and (eq 0 delta-x) (eq 0 delta-y)) state)
    (t
      (setq step-x (signum delta-x))
      (setq step-y (signum delta-y))

      (draw2 (- delta-x step-x) (- delta-y step-y) (+ pos-x step-x) (+ pos-y step-y) state))))

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
    (draw2 delta-x delta-y x1 y1 state))
  (return-from run-input state))

(defun count-values (xs)
  (reduce
    (lambda (x y)
      (cond
        ((> y 1) (+ 1 x))
        (t x)))
    xs
    :initial-value 0))

(format t "Part 1: ~d~%" (count-values (run-input input1 state1)))
(format t "Part 2: ~d~%" (count-values (run-input input2 state2)))
