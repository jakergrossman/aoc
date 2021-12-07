#!/usr/bin/gcl -f

(load "../common")

(defun process-line (line)
  (let* ((space-pos (position #\Space line))
        (direction (subseq line 0 space-pos))
        (distance  (parse-integer (subseq line (+ 1 space-pos)))))
    (list direction distance)))

(setq input
  (get-input
    "input.txt" 'process-line (lambda (x) (> (length x) 0))))

(defun subpath1 (input depth distance)
  (cond
    ((null input) (list depth distance))
    (t (let ((nextmove (car input)))
      (cond
        ((string= (car nextmove) "forward")
          (subpath1 (cdr input) depth (+ distance (cadr nextmove))))

        ((string= (car nextmove) "up")
          (subpath1 (cdr input) (- depth (cadr nextmove)) distance))

        ((string= (car nextmove) "down")
          (subpath1 (cdr input) (+ depth (cadr nextmove)) distance)))))))

(defun subpath2 (input depth distance aim)
  (cond
    ((null input) (list depth distance aim))
    (t (let ((nextmove (car input)))
      (cond
        ((string= (car nextmove) "forward")
          (subpath2 (cdr input) (+ depth (* aim (cadr nextmove))) (+ distance (cadr nextmove)) aim))

        ((string= (car nextmove) "up")
          (subpath2 (cdr input) depth distance (- aim (cadr nextmove))))

        ((string= (car nextmove) "down")
          (subpath2 (cdr input) depth distance (+ aim (cadr nextmove)))))))))

(compile 'subpath1)
(compile 'subpath2)

(format t "Part 1: ~d~%" (* (car (subpath1 input 0 0)) (cadr (subpath1 input 0 0))))
(format t "Part 2: ~d~%" (* (car (subpath2 input 0 0 0)) (cadr (subpath2 input 0 0 0))))
