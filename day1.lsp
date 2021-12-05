(defun get-input (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
      while (and line (> (length line) 0))
      collect (parse-integer line))))

(setq input (get-input "inputs/input1.txt"))

; Count the number of times a value increases from the last
(defun count1 (xs)
  (setq head (car xs))
  (setq tail (cdr xs))
  (cond
    ((null tail) 0)
    ((< head (car tail)) (+ 1 (count1 tail)))
    (t (count1 tail))
  )
)

; Count the number of times the sum of a 3 count window increases from the last
(defun count2 (xs prev)
  (cond
    ((< (length xs) 4) 0) ; no more chunks to compare
    (t
      (setq a (car xs))
      (setq b (cadr xs))
      (setq c (caddr xs))
      (setq sum (+ (+ a b) c))
      (cond
        ((< prev sum) (+ 1 (count2 (cdr xs) sum)))
        (t (count2 (cdr xs) sum))
      )
    )
  )
)

(compile 'count1)
(compile 'count2)

(count1 input)
(count2 input 0)
