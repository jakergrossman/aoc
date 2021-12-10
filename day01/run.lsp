; Day 1: Sonar Sweep

(load "../common")

(setq input
  (get-input
    "input.txt" :process #'parse-integer :predicate #'string-empty-p))

; Count the number of times a value increases from the last
(defun count1 (xs)
  (let ((head (car xs))
        (tail (cdr xs)))
    (cond
      ((null tail) 0)
      ((< head (car tail)) (+ 1 (count1 tail)))
      (t (count1 tail)))))

; Count the number of times the sum of a 3 count window increases from the last
(defun count2 (xs prev)
  (cond
    ((< (length xs) 4) 0) ; no more chunks to compare
    (t
      (let* ((a (car xs))
            (b (cadr xs))
            (c (caddr xs))
            (sum (+ (+ a b) c)))
        (cond
          ((< prev sum) (+ 1 (count2 (cdr xs) sum)))
          (t (count2 (cdr xs) sum)))))))

(compile 'count1)
(compile 'count2)

(format t "Part 1: ~d~%" (count1 input))
(format t "Part 2: ~d~%" (count2 input 0))
