; Day 18: Like a GIF For Your Yard

(load "../../include/lisp/common.lsp")

#+(or :GCL :CLISP)
(eval-when (:compile-toplevel :execute)
  (format t "This day is too slow on GCL or CLISP,~%use SBCL instead.~%")
  (quit))

(defparameter *width* nil)
(defparameter *height* nil)

(defun xy-index (x y)
  (+ x (* y *width*)))

(defun livep (c)
  (cond
    ((char-equal #\. c) nil)
    ((char-equal #\# c) t)))

; collect neight positions of a specified point
(defun neighbors (x y)
  (loop :for n :from (- y 1) :to (+ y 1)
        :unless (or (< n 0) (>= n *height*))
        :append (loop :for m :from (- x 1) :to (+ x 1)
                      :unless (or (and (eq x m) (eq y n))
                                  (< m 0) (>= m *width*))
                      :collect (list m n))))

; count living neighbors
(defun num-living-neighbors (x y state)
  (let ((positions (neighbors x y)))
    (loop :for pos :in positions
          :for (n m) = pos
          :when (livep (aref state (xy-index n m)))
          :sum 1)))

; check if corner for part 2
(defun corner-p (x y)
  (or (and (eq x 0) (eq y 0))
      (and (eq x 0) (eq y (- *height* 1)))
      (and (eq x (- *width* 1)) (eq y 0))
      (and (eq x (- *width* 1)) (eq y (- *height* 1)))))

; update state
(defun tick-state (state &key broken)
  (let ((old-state (subseq state 0)))
    (loop :for y :below *height* :do
      (loop :for x :below *width* :do
        (let ((alive (livep (aref old-state (xy-index x y))))
              (neighbors (num-living-neighbors x y old-state)))
          (cond
            ((and broken (corner-p x y))
                (setf (aref state (xy-index x y)) #\#))

            ((and alive (not (or (eq 2 neighbors) (eq 3 neighbors))))
                (setf (aref state (xy-index x y)) #\.))

            ((and (not alive) (eq 3 neighbors))
                (setf (aref state (xy-index x y)) #\#))))))))

(defun process-lines (lines)
  (setf *width* (length (car lines)))
  (setf *height* (length lines))
  (make-array (list (* *width* *height*))
              :initial-contents (reduce #'append (mapcar (lambda (x) (coerce x 'list)) lines))))

(defun answer (&optional (file #P"input.txt"))
  (let ((state1 (process-lines (get-input file)))
        (state2 (process-lines (get-input file)))
        (p1 nil)
        (p2 nil))
    (dotimes (i 100) (tick-state state1))
    (setf p1 (count #\# state1))

    ; broken lights
    (setf (aref state2 0) #\#)
    (setf (aref state2 (- *width* 1)) #\#)
    (setf (aref state2 (* *width* (- *height* 1))) #\#)
    (setf (aref state2 (- (* *width* *height*) 1)) #\#)

    (dotimes (i 100) (tick-state state2 :broken t))
    (setf p2 (count #\# state2))

    (format t "Part 1: ~d~%" p1)
    (format t "Part 2: ~d~%" p2)))

(answer)
