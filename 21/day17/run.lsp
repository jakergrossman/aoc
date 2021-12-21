; Day 17: Trick Shot

(load "../../include/lisp/common.lsp")

(defun target-area-p (x y target-x target-y)
  (and (>= x (car target-x))
       (<= x (cadr target-x))
       (>= y (car target-y))
       (<= y (cadr target-y))))

(defun get-pos (vx0 vy0 time)
  (let ((y (- (* vy0 time) (triangle (- time 1)))))
    (cond
      ((< time vx0)
       (let ((x (floor (* time (- (* 2 vx0) time -1)) 2)))
         (list x y)))

      (t (list (triangle vx0) y)))))

(defun valid-velocity-p (vx0 vy0 target-x target-y &optional (time 1))
  (let* ((pos (get-pos vx0 vy0 time))
         (x (car pos))
         (y (cadr pos)))
    (cond
      ((or (> x (cadr target-x))
           (< y (car target-y))) nil)

      ((target-area-p x y target-x target-y) (list vx0 vy0))

      (t (valid-velocity-p vx0 vy0 target-x target-y (+ time 1))))))

(defun collect-valid-velocities (bound-x bound-y target-x target-y)
  (loop :for y :from bound-y :to (- (abs bound-y) 1)
        :append (loop :for x :from 1 :to bound-x
                      :for result = (valid-velocity-p x y target-x target-y)
                      :when (not (null result))
                      :collect result)))

(defun triangle (n)
  (floor (* n (+ n 1)) 2))

(defun parse-line (line)
  (let* ((words (parse-words line))
         (xs (subseq (nth 2 words) 0 (- (length (nth 2 words)) 1)))
         (ys (nth 3 words)))
    (loop :for data :in (list xs ys)
          :for equal-pos = (position #\= data)
          :for dot-pos = (position #\. data)
          :collect (list (parse-integer (subseq data (+ 1 equal-pos) dot-pos))
                         (parse-integer (subseq data (+ 2 dot-pos)))))))

(defun answer (&optional (file #P"input.txt"))
  (let* ((input (parse-line (car (get-lines file))))
         (max-x (apply #'max (car input)))
         (min-y (apply #'min (cadr input)))
         (part1 (triangle (- (abs min-y) 1)))
         (part2 (collect-valid-velocities max-x min-y (car input) (cadr input))))
    (format t "Part 1: ~d~%" part1)
    (format t "Part 2: ~d~%" (length part2))))

(answer)
