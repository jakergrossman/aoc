; Day 6: Lanternfish

(load "../../include/lisp/common.lsp")

(defparameter input (parse-integers (car (get-lines "input.txt")) #\,))

(defparameter fish-state (loop :for n :below 9 :collect 0))
(loop :for n :below (length input)
  do (apply (lambda (x) (setf (nth x fish-state) (+ 1 (nth x fish-state)))) (list (nth n input))))

; operate on #'s of fish instead of individual fish for speed
(defun run-day (fish)
  (let ((zero (nth 0 fish))
        (one (nth 1 fish))
        (two (nth 2 fish))
        (three (nth 3 fish))
        (four (nth 4 fish))
        (five (nth 5 fish))
        (six (nth 6 fish))
        (seven (nth 7 fish))
        (eight (nth 8 fish)))
    (list one two three four five six (+ seven zero) eight zero)))

(defun run-days (days fish)
  (cond
    ((eq 0 days) fish)
    (t (run-days (- days 1) (run-day fish)))))

(format t "Part 1: ~d~%" (reduce '+ (run-days 80 fish-state)))
(format t "Part 2: ~d~%" (reduce '+ (run-days 256 fish-state)))
