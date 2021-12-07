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

(defun fuel-cost1 (pos crabs)
  (reduce
    (lambda (x y)
      (setq displacement (- pos y))

      ; absolute value of displacement
      (setq distance (* (signum displacement) displacement))
      (+ x distance))
    crabs
    :initial-value 0))

(defun fuel-cost2 (pos crabs)
  (reduce
    (lambda (x y)
      (setq displacement (- pos y))
      (setq distance (* (signum displacement) displacement))
      (+ x (cost distance)))
    crabs
    :initial-value 0))

(setq max-pos
  (reduce
    (lambda (x y)
      (cond
        ((> y x) y)
        (t x)))
    input
    :initial-value 0))

(defun cost (distance)
  (round (* 0.5 (* (+ 1 distance) distance))))

(setq fuel-costs1
  (map 'list
    (lambda (x) (fuel-cost1 x input))
    (loop :for n :below max-pos :collect n)))

(setq fuel-costs2 (loop :for n :below max-pos :collect (fuel-cost2 n input)))

(format t "Part 1: ~d~%" (reduce 'min fuel-costs1))

(format t "Part 2: ~d~%" (reduce 'min fuel-costs2))
