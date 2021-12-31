; Day 2: Dive!

(load "../../include/lisp/common.lsp")

(defun subpath1 (input)
  (loop for (action value) in input
        when (equal action '|forward|)
          sum value into distance

        when (equal action '|up|)
          sum (- value) into depth

        when (equal action '|down|)
          sum value into depth

        finally (return (* distance depth))))

(defun subpath2 (input)
  (loop for (action value) in input
        when (equal action '|forward|)
          sum value into distance
          and sum (* value aim) into depth

        when (equal action '|up|)
          sum (- value) into aim

        when (equal action '|down|)
          sum value into aim

        finally (return (* distance depth))))

(defun read-direction (line)
  (let ((words (parse-words line)))
    (list (intern (car words)) (parse-integer (cadr words)))))

(defun answer (&optional (file #P"input.txt"))
  (let ((input (mapcar #'read-direction (get-lines file))))
    (format t "Part 1: ~d~%" (subpath1 input))
    (format t "Part 2: ~d~%" (subpath2 input))))
