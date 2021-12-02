(defun subpath1 (input depth distance)
  (cond
    ((null input) (list depth distance))
    (t
      (setq nextmove (car input))
      (cond
        ((eq (car nextmove) 'forward)
          (subpath1 (cdr input) depth (+ distance (cadr nextmove))))

        ((eq (car nextmove) 'up)
          (subpath1 (cdr input) (- depth (cadr nextmove)) distance))

        ((eq (car nextmove) 'down)
          (subpath1 (cdr input) (+ depth (cadr nextmove)) distance))
      )
    )
  )
)

(defun subpath2 (input depth distance aim)
  (cond
    ((null input) (list depth distance aim))
    (t
      (setq nextmove (car input))
      (cond
        ((eq (car nextmove) 'forward)
          (subpath2 (cdr input) (+ depth (* aim (cadr nextmove))) (+ distance (cadr nextmove)) aim))

        ((eq (car nextmove) 'up)
          (subpath2 (cdr input) depth distance (- aim (cadr nextmove))))

        ((eq (car nextmove) 'down)
          (subpath2 (cdr input) depth distance (+ aim (cadr nextmove))))
      )
    )
  )
)

(load "./inputs/input2")
(compile 'subpath1)
(compile 'subpath2)

(* (car (subpath1 input 0 0)) (cadr (subpath1 input 0 0)))
(* (car (subpath2 input 0 0 0)) (cadr (subpath2 input 0 0 0)))
