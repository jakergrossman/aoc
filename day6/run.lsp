#!/usr/bin/gcl -f

(defun parse-nums (line)
  (setq delim-pos (position #\, line))
  (cond
    ((null delim-pos)
      (setq num (parse-integer line))
      (list num))
    (t
      (setq num (parse-integer (subseq line 0 delim-pos)))
      (append (list num) (parse-nums (subseq line (+ 1 delim-pos)))))))

(defun get-input (filename)
  (with-open-file (stream filename)
    (parse-nums (read-line stream nil nil))))

(setq input (get-input "input.txt"))
(setq fish-state (loop :for n :below 9 :collect 0))
(loop :for n :below (length input)
  do (apply (lambda (x) (setf (nth x fish-state) (+ 1 (nth x fish-state)))) (list (nth n input))))

; operate on #'s of fish instead of individual fish for speed
(defun run-day (fish)
  (setq zero (nth 0 fish))
  (setq one (nth 1 fish))
  (setq two (nth 2 fish))
  (setq three (nth 3 fish))
  (setq four (nth 4 fish))
  (setq five (nth 5 fish))
  (setq six (nth 6 fish))
  (setq seven (nth 7 fish))
  (setq eight (nth 8 fish))
  (list one two three four five six (+ seven zero) eight zero))

(defun run-days (days fish)
  (cond
    ((eq 0 days) fish)
    (t (run-days (- days 1) (run-day fish)))))

(format t "Part 1: ~d~%" (reduce '+ (run-days 80 fish-state)))
(format t "Part 2: ~d~%" (reduce '+ (run-days 256 fish-state)))