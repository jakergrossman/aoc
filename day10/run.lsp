; Day 10: Syntax Scoring

(load "../common")

(setq input (get-input "input.txt" :predicate #'string-empty-p))

; return first invalid character, or the remaining stack if str is exhausted
(defun validate (str stack)
  (cond
    ((string-empty-p str) stack)
    (t
      (let ((chr (char str 0)))
        (cond
          ((eq #\[ chr) (validate (subseq str 1) (cons #\] stack)))
          ((eq #\{ chr) (validate (subseq str 1) (cons #\} stack)))
          ((eq #\( chr) (validate (subseq str 1) (cons #\) stack)))
          ((eq #\< chr) (validate (subseq str 1) (cons #\> stack)))
          (t
            (cond
              ((eq chr (car stack)) (validate (subseq str 1) (cdr stack)))
              (t chr))))))))

; score a string for part 1
(defun score1 (str)
  (let ((result (validate str nil)))
    (cond
      ((listp result) 0)
      ((eq #\) result) 3)
      ((eq #\] result) 57)
      ((eq #\} result) 1197)
      ((eq #\> result) 25137))))

; score a string for part 2
(defun score2 (str)
  (let ((stack (validate str nil)))
    (cond
      ((not (listp stack)) 0)
      (t
        (reduce
          (lambda (x y)
            (+ (* x 5)
              (cond
                ((eq #\) y) 1)
                ((eq #\] y) 2)
                ((eq #\} y) 3)
                ((eq #\> y) 4))))
          stack
          :initial-value 0)))))

(setq part1-scores (map 'list #'score1 input))
(setq part2-scores (sort (remove 0 (map 'list #'score2 input)) #'<))

(format t "Part 1: ~d~%" (reduce '+ part1-scores))
(format t "Part 2: ~d~%" (nth (floor (length part2-scores) 2) part2-scores))
