; Day 2: I Was Told There Would Be No Math

(load "../../include/lisp/common.lsp")

(defun process-line (line)
  (parse-integers line #\x))

(setq input (mapcar #'process-line (get-lines "input.txt")))

(setq part1
  (reduce
    (lambda (x y)
      (let* ((len (car y))
            (hgt (cadr y))
            (width (caddr y))
            (a1 (* len hgt))
            (a2 (* len width))
            (a3 (* width hgt)))
        (+ x (* 2 a1) (* 2 a2) (* 2 a3) (reduce #'min (list a1 a2 a3)))))
    input
    :initial-value 0))

(setq part2
  (reduce
    (lambda (x y)
      (let* ((len (car y))
            (hgt (cadr y))
            (width (caddr y))
            (p1 (* 2 (+ len hgt)))
            (p2 (* 2 (+ len width)))
            (p3 (* 2 (+ width hgt))))
        (+ x (reduce #'min (list p1 p2 p3)) (* len hgt width))))
    input
    :initial-value 0))

(format t "Part 1: ~d~%" part1)
(format t "Part 2: ~d~%" part2)
