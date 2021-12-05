(defun subpath1 (input depth distance)
  (cond
    ((null input) (list depth distance))
    (t (setq nextmove (car input))
      (cond
        ((string= (car nextmove) "forward")
          (subpath1 (cdr input) depth (+ distance (cadr nextmove))))

        ((string= (car nextmove) "up")
          (subpath1 (cdr input) (- depth (cadr nextmove)) distance))

        ((string= (car nextmove) "down")
          (subpath1 (cdr input) (+ depth (cadr nextmove)) distance))))))

(defun subpath2 (input depth distance aim)
  (cond
    ((null input) (list depth distance aim))
    (t
      (setq nextmove (car input))
      (cond
        ((string= (car nextmove) "forward")
          (subpath2 (cdr input) (+ depth (* aim (cadr nextmove))) (+ distance (cadr nextmove)) aim))

        ((string= (car nextmove) "up")
          (subpath2 (cdr input) depth distance (- aim (cadr nextmove))))

        ((string= (car nextmove) "down")
          (subpath2 (cdr input) depth distance (+ aim (cadr nextmove))))))))

(defun process-line (line)
  (setq space-pos (position #\Space line))

  (setq direction (subseq line 0 space-pos))
  (setq distance  (parse-integer (subseq line (+ 1 space-pos))))

  (list direction distance))

(defun get-input (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
      while (and line (> (length line) 0))
      collect (process-line line))))

(setq input (get-input "inputs/input2.txt"))

(compile 'subpath1)
(compile 'subpath2)

(* (car (subpath1 input 0 0)) (cadr (subpath1 input 0 0)))
(* (car (subpath2 input 0 0 0)) (cadr (subpath2 input 0 0 0)))
