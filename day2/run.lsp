#!/usr/bin/gcl -f

(load "../common")

(defun process-line (line)
  (let* ((words (parse-words line)))
    (list (car words) (parse-integer (cadr words)))))

(setq input
  (get-input
    "input.txt" 'process-line (lambda (x) (> (length x) 0))))


(defun subpath1 (input)
  (reduce
    (lambda (x y)
      (let ((depth (car x))
            (distance (cadr x))
            (action (car y))
            (value (cadr y)))
        (cond
          ((string= action "forward")
            (list depth (+ distance value)))

          ((string= action "up")
            (list (- depth value) distance))

          ((string= action "down")
            (list (+ depth value) distance)))))
    input
    :initial-value (list 0 0)))

(defun subpath2 (input)
  (reduce
    (lambda (x y)
      (let ((depth (car x))
            (distance (cadr x))
            (aim (caddr x))
            (action (car y))
            (value (cadr y)))
        (cond
          ((string= action "forward")
            (list (+ depth (* aim value)) (+ distance value) aim))

          ((string= action "up")
            (list depth distance (- aim value)))

          ((string= action "down")
            (list depth distance (+ aim value))))))
    input
    :initial-value (list 0 0 0)))


(format t "Part 1: ~d~%" (* (car (subpath1 input)) (cadr (subpath1 input))))
(format t "Part 2: ~d~%" (* (car (subpath2 input)) (cadr (subpath2 input))))
