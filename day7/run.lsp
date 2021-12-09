; Day 7: The Treachery of Whales

(load "../common")

(defun process-line (line)
  (parse-integers line #\,))

(setq input
  (car (get-input
    "input.txt" :process #'process-line :predicate #'string-empty-p)))

(setf input-array (make-array (list (length input)) :initial-contents input))

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
      (+ x (triangle-cost distance)))
    crabs
    :initial-value 0))

(setq max-pos (reduce 'max input-array))

(defun triangle-cost (distance)
  (* 0.5 (* (+ 1 distance) distance)))

(setq fuel-costs1
  (map 'list
    (lambda (x) (fuel-cost1 x input-array))
    (loop :for n :below max-pos :collect n)))

(setq fuel-costs2 (loop :for n :below max-pos :collect (round (fuel-cost2 n input-array))))

(format t "Part 1: ~d~%" (reduce 'min fuel-costs1))

(format t "Part 2: ~d~%" (reduce 'min fuel-costs2))
