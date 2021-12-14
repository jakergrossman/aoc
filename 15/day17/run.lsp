; Day 17: No Such Thing as Too Much

(load "../../include/common.lsp")

; Find every way to hold `volume` units with `containers`
(defun discover (containers volume &optional used-containers)
  (let ((next-container (car containers)))
    (cond
      ((eq 0 volume) (list used-containers))
      ((null containers) nil)
      ((> next-container volume) (discover (cdr containers) volume used-containers))
      (t (append (discover (cdr containers)
                           (- volume next-container)
                           (cons next-container used-containers))

                 (discover (cdr containers)
                           volume
                           used-containers))))))

(defun answer (&optional (file #P"input.txt") (volume 150))
  (let* ((containers (get-input file :process #'parse-integer))
         (p1-combs (discover containers volume))
         (min-containers (reduce #'min (mapcar #'length p1-combs)))
         (p2-combs (remove min-containers p1-combs :test-not #'eq :key #'length)))
    (format t "Part 1: ~d~%" (length p1-combs))
    (format t "Part 2: ~d~%" (length p2-combs))))

(answer)
