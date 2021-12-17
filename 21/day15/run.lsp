; Day 15: Chiton

#+(or :GCL :CLISP)
(eval-when (:compile-toplevel :execute)
  (format t "This day is too slow on GCL or CLISP,~%use SBCL instead.~%")
  (quit))

(load "../../include/lisp/common.lsp")

(defparameter *size* nil)

(defparameter *start* '(0 . 0))
(defparameter *end* nil)
(defparameter *end2* nil)

(defun neighbors (node size)
  (let ((x (car node))
        (y (cdr node)))
    (append
      (cond ((> x 0)      (list (cons (- x 1) y))))
      (cond ((< x (- size 1)) (list (cons (+ x 1) y))))
      (cond ((> y 0)      (list (cons x (- y 1)))))
      (cond ((< y (- size 1)) (list (cons x (+ y 1))))))))

; walks the backtrace, appending each element
; to the start, resulting in the path from
; start to finish
(defun walk-path (backtrace-table tail start)
  (let* ((head (car tail))
         (next (gethash head backtrace-table)))
    (cond
      ((eq head start) tail)
      ((null next) tail)
      (t (walk-path backtrace-table (cons next tail) start)))))

; uniform cost search
(defun djikstra (grid frontier costs start end
                      &optional (backtrace (make-hash-table :test #'equal)))
  (cond
    ((null frontier) (format t "grid exhausted but no path found~%"))
    ((equal (caar frontier) end) (walk-path backtrace (list end) *start*))
    (t (let* ((current (caar frontier))
              (found (loop :for next :in (neighbors current (car (array-dimensions grid)))
                           :for new-cost = (+ (if (gethash current costs)
                                                  (gethash current costs) 0)
                                              (aref grid (cdr next) (car next)))
                           :when (or (eq next start)
                                     (null (gethash next costs))
                                     (< new-cost (gethash next costs)))
                           :collect (list next new-cost) :into new-nodes
                           :finally (return new-nodes))))
         (loop :for node :in found
               :for (pos cost) = node
               :do (setf (gethash pos backtrace) current)
               :do (setf (gethash pos costs) cost))

         (djikstra grid (sort (append (cdr frontier) found) #'< :key #'cadr)
                   costs start end backtrace)))))

; scale the grid according to the rules outlined in the prompt
(defun interpolate-grid (grid scale)
  (let* ((side-length (car (array-dimensions grid)))
         (interpolated (make-array (list (* scale side-length) (* scale side-length)))))
    (loop :for y :below side-length :do
      (loop :for x :below side-length :do
        (let ((value (aref grid y x)))
          (loop :for i :below scale :do
            (loop :for j :below scale :do
              (setf (aref interpolated (+ y (* side-length i)) (+ x (* side-length j)))
                          (+ (mod (- (+ i j value) 1) 9) 1)))))))
    interpolated))

(defun process-lines (lines)
  (let ((grid (loop :for line :in lines
                    :collect (mapcar #'digit-char-p (coerce line 'list)))))
         (setf *size* (length (car grid))) ; square
         (setf *end* (cons (- *size* 1) (- *size* 1)))
         (setf *end2* (cons (- (* 5 *size*) 1) (- (* 5 *size*) 1)))
         (make-array (list *size* *size*) :initial-contents grid)))

(compile 'walk-path)
(compile 'djikstra)

(defun path-cost (path grid)
  (reduce #'+ (mapcar (lambda (x) (aref grid (cdr x) (car x))) path)))

(defun answer (&optional (file #P"input.txt"))
  (let* ((grid1 (process-lines (get-input file)))
         (grid2 (interpolate-grid grid1 5))
         (costs1 (make-hash-table :test #'equal))
         (costs2 (make-hash-table :test #'equal))
         (p1-path (djikstra grid1 (list (cons *start* 0)) costs1 *start* *end*))
         (p2-path (djikstra grid2 (list (cons *start* 0)) costs2 *start* *end2*)))
  (format t "Part 1: ~d~%" (path-cost p1-path grid1))
  (format t "Part 2: ~d~%" (path-cost p2-path grid2))))

(answer)
