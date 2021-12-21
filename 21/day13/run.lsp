; Day 13: Transparent Origami

(load "../../include/lisp/common.lsp")

(defun parse-fold-direction (line)
  (let ((eq-pos (position #\= line)))
    ; (axis, value)
    (list (char line (- eq-pos 1))
          (parse-integer (subseq line (+ eq-pos 1))))))

; reduce lines to ((points) (folds))
(defun parse-input (lines)
  (reduce
    (lambda (x y)
      (cond
        ((digit-char-p (char y 0))
           (cons (cons (parse-integers y #\,) (car x))
                 (cdr x)))

        (t (cons (car x)
                 (list (cons (parse-fold-direction y) (cadr x)))))))
    lines
    :initial-value '(() ())))

(defun parse-fold-direction (line)
  (let ((eq-pos (position #\= line)))
    ; (axis, value)
    (list (char line (- eq-pos 1))
          (parse-integer (subseq line (+ eq-pos 1))))))

(defun fold-point (point direction)
  (let* ((axis (car direction))
         (value (cadr direction))
         (x (car point))
         (y (cadr point))

         ; distance from fold*2
         ; is the movement required
         ; to fold over the axis
         (x-diff (* 2 (abs (- value x))))
         (y-diff (* 2 (abs (- value y)))))
    (cond
      ((and (eq #\x axis) (>= x value))
        (list (- x x-diff) y))

      ((and (eq #\y axis) (>= y value))
        (list x (- y y-diff)))

      (t point))))

(defun fold (points direction)
  (remove-duplicates
    (mapcar (lambda (point) (fold-point point direction))
            points)
    :test #'equal))

(defun answer (&optional (file #P"input.txt"))
  (let* ((input-lines (remove "" (get-lines file) :test #'string=))
         (input (parse-input input-lines))
         (points (car input))
         (folds (reverse (cadr input)))
         (p1 (length (fold points (car folds))))
         (p2 (reduce (lambda (x y) (fold x y))
                     folds
                     :initial-value points)))
    (list p1 p2)))

; print output of folding
(defun print-points (points)
  (let* ((max-x (reduce #'max points :key #'car))
         (max-y (reduce #'max points :key #'cadr))
         (min-x (reduce #'min points :key #'car))
         (min-y (reduce #'min points :key #'cadr))
         (width (+ 1 (- max-x min-x)))
         (height (+ 1 (- max-y min-y)))
         (grid (make-array (list height width) :initial-element #\Space)))

    ; fill grid
    (mapc (lambda (p) (setf (aref grid (cadr p) (car p)) #\#))
          points)

    (dotimes (y height)
      (dotimes (x width)
        (princ (aref grid y x)))
      (terpri))))

(print-points (cadr (answer)))
(format t "Part 1: ~d~%" (car (answer)))
(format t "Part 2: ~d~%" "PCPHARKL") ; hardcoded based on visual output above
