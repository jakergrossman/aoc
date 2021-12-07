#!/usr/bin/gcl -f

(load "../common")

; process an input line, returning ((x1 y1) (x2 y2))
(defun process-line (line)
  (let* ((left-comma (position #\, line))
        (left-space (position #\Space line))
        (right-comma (position #\, line :from-end T))
        (right-space (position #\Space line :from-end T))

        (x1 (parse-integer (subseq line 0 left-comma)))
        (y1 (parse-integer (subseq line (+ 1 left-comma) left-space)))

        (x2 (parse-integer (subseq line (+ 1 right-space) right-comma)))
        (y2 (parse-integer (subseq line (+ 1 right-comma)))))
    (list (list x1 y1) (list x2 y2))))

; check if a movement is in a straight line
(defun is-straight (entry)
  (not
    ; x1 != x2 && y1 != y2 i.e.; not a straight line
    (and
      (not (eq (caar entry) (caadr entry)))
      (not (eq (cadar entry) (cadadr entry))))))

; only straight lines
(setq input2
      (get-input
        "input.txt" 'process-line (lambda (x) (> (length x) 0))))
(setq input1 (remove-if-not #'is-straight input2))
(setq state1 (make-array '(1000000) :initial-element 0))

; all lines
(setq state2 (make-array '(1000000) :initial-element 0))

(defun draw2 (delta-x delta-y pos-x pos-y state)
  (let* ((pos (+ (* pos-x 1000) pos-y))
        (current-value (aref state pos))
        (step-x (signum delta-x))
        (step-y (signum delta-y)))
    (setf (aref state pos) (+ 1 current-value))
    (cond
      ((and (eq 0 delta-x) (eq 0 delta-y)) state) ; done
      (t (draw2 (- delta-x step-x) (- delta-y step-y) (+ pos-x step-x) (+ pos-y step-y) state)))))

(defun run-input (input state)
  (loop :for n :below (length input)
    do
    (let* ((movement (nth n input))
          (x1 (caar movement))
          (y1 (cadar movement))
          (x2 (caadr movement))
          (y2 (cadadr movement))

          (delta-x (- x2 x1))
          (delta-y (- y2 y1)))
      (draw2 delta-x delta-y x1 y1 state)))
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
