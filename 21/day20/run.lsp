; Day 20: Trench Map

(load "../../include/lisp/common.lsp")

(defun point-digits (state x y frontier-fill)
  "Find the binary digits of an (x . y) point in state"
  (let* ((discovered)
     (digits (loop :for y :from (1- y) :to (1+ y)
               :append (loop :for x :from (1- x) :to (1+ x)
                     :collect  (let* ((key (cons x y))
                              (val (gethash key state)))
                         (cond
                           ((null val) ; register discovered point
                            (push key discovered)
                            frontier-fill)
                           (t val)))))))
    (values digits discovered)))


(defun enhance (state translate frontier-fill)
  (let ((new-points (loop :for point :being :each hash-key :of state
              :for (x . y) = point
              :for (digits1 discovered) = (multiple-value-bind (a b)
                              (point-digits state x y frontier-fill)
                            (list a b))

              :for digits2 = (loop :for d :in discovered
                           :for (x . y) = d
                           :collect (list d (point-digits state x y frontier-fill)))
              :collect (list point digits1)
              :append digits2)))
    (loop :for (point value) :in new-points
      :for decimal = (digit-dec value :radix 2)
      :do (setf (gethash point state) (aref translate decimal)))))

(defun count-lit (state)
  (length (loop :for k :being :each hash-key :of state
        :for value = (gethash k state)
        :when (= 1 value)
          :collect value)))

(defun read-map (lines)
  "Reads initial map data into a hash-map of data-points"
  (let ((hash (make-hash-table :test #'equal)))
    (dotimes (j (length lines))
      (let ((line (nth j lines)))
        (dotimes (i (length line))
      (ecase (char line i)
        (#\. (setf (gethash (cons i j) hash) 0))
        (#\# (setf (gethash (cons i j) hash) 1))))))
    hash))

(defun read-translate (line)
  "Reads translation string into an array of 0's and 1's"
  (let ((translate (make-array (list (length line)))))
    (dotimes (i (length line))
      (ecase (char line i)
      (#\. (setf (aref translate i) 0))
      (#\# (setf (aref translate i) 1))))
    translate))

(defun answer (&optional (file #P"input.txt"))
  (let* ((input (get-lines file))
     (translate (read-translate (car input)))
     (state (read-map (cddr input)))
     (frontier-fill 0)
     (p1)
     (p2))
    (dotimes (i 2)
      (enhance state translate frontier-fill)
      (setf frontier-fill (if (zerop frontier-fill) (aref translate 0) (aref translate 511))))
    (setf p1 (count-lit state))
    (dotimes (i 48)
      (enhance state translate frontier-fill)
      (setf frontier-fill (if (zerop frontier-fill) (aref translate 0) (aref translate 511))))
    (setf p2 (count-lit state))
    (format t "Part 1: ~d~%" p1)
    (format t "Part 2: ~d~%" p2)))

(answer)
