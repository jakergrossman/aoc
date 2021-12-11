; Day 1: Not Quite Lisp

(load "../../include/common.lsp")

(setq input (car (get-input "input.txt")))

(defun val (c)
  (cond ((eq #\( c) 1) (t -1)))

(setq part1
  (reduce
    (lambda (x y)
      (+ x (val y)))
    input
    :initial-value 0))

(defun first-negative (str &optional (level 0) (pos 0))
  (cond
    ((< level 0) pos)
    ((string-empty-p str) nil)
    (t (first-negative (subseq str 1) (+ level (val (char str 0))) (+ 1 pos)))))

(format t "Part 1: ~d~%" part1)
(format t "Part 2: ~d~%" (first-negative input))
