; Day 22: Reactor Robot

(load "../../include/lisp/common.lsp")

(defun read-positions (str)
  (let ((words (parse-words str #\,)))
    (loop :for word :in words
          :for dash-pos = (position #\. word)
          :for from = (parse-integer (subseq word 2 dash-pos))
          :for to = (parse-integer (subseq word (+ 2 dash-pos)))
          :collect (cons from to))))

(defun read-moves (lines)
  (loop :for line :in lines
        :for (move rest) = (parse-words line #\Space)
        :for new-state = (if (string= "off" move) -1 1)
        :for positions = (read-positions rest)
        :collect (list new-state positions)))

(defun offset (n)
  (+ 50 n))

(defun init-p (move)
(every (lambda (x) (and (>= (car x) -50) (<= (car x) 50)
                        (>= (cdr x) -50) (<= (cdr x) 50))) move))

(defun part1 (input)
  (let ((init-zone (make-array '(101 101 101) :initial-element nil)))
    (loop :for (value key) :in input
          :for enabled = (eq value 1)
          :for (xs ys zs) = key
          :when (init-p key) :do
      (loop :for x :from (car xs) :below (1+ (cdr xs)) :do
        (loop :for y :from (car ys) :below (1+ (cdr ys)) :do
          (loop :for z :from (car zs) :below (1+ (cdr zs))
                :do (setf (aref init-zone (offset z) (offset y) (offset x)) enabled)))))
    (loop :for n :below (array-total-size init-zone)
          :when (row-major-aref init-zone n)
            :sum 1)))

(defun segment-size (s)
  (1+ (- (cdr s) (car s))))

(defun box-size (box)
  (apply #'* (mapcar #'segment-size box)))

(defun segment-intersection (p1 p2)
  (let ((left (max (car p1) (car p2)))
        (right (min (cdr p1) (cdr p2))))
    (if (<= left right)
        (cons left right))))

(defun intersection-boxes (boxes new-box)
  (loop :for (box . value) :in boxes
        :for intersections = (map 'list #'segment-intersection new-box box)
        :unless (some #'null intersections)
          :collect (cons intersections (- value))))

(defun add-box (boxes new-box)
  (let ((intersections (intersection-boxes boxes (cadr new-box))))
    (if (eq (car new-box) 1)

        ;; box is lit, keep intersecting portion
        (cons (cons (cadr new-box) 1) (nconc intersections boxes))

        ;; box is unlit, keep only non-intersecting portion
        (nconc intersections boxes))))

(defun part2 (input)
  (loop :for (box . value) :in (reduce #'add-box input :initial-value nil)
        :sum (* value (box-size box))))

(defun answer (&optional (file #P"input.txt"))
  (let ((moves (read-moves (get-lines file))))
    (format t "Part 1: ~d~%" (part1 moves))
    (format t "Part 2: ~d~%" (part2 moves))))

(answer)
