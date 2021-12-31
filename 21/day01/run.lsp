; Day 1: Sonar Sweep

(load "../../include/lisp/common.lsp")

(defun count1 (xs)
  "Count the amount of times XS[N+1] > XS[N]"
  (loop for n from 0 below (1- (length xs))
        for a = (nth n xs)
        for b = (nth (1+ n) xs)
        count (> b a)))

(defun count2 (xs)
  "Count the amount of times XS[N+1:N+4] > XS[N:N+3"
  (loop for n from 0 below (- (length xs) 3)
        for a = (subseq xs n (+ n 3))
        for b = (subseq xs (1+ n) (+ n 4))
        count (> (apply #'+ b) (apply #'+ a))))

(defun answer (&optional (file #P"input.txt"))
  (let ((input (mapcar #'parse-integer (get-lines file))))
    (format t "Part 1: ~d~%" (count1 input))
    (format t "Part 2: ~d~%" (count2 input))))

(answer)
