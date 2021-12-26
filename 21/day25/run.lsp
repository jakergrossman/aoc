; 25: Sea Cucumber

(load "../../include/lisp/common.lsp")

(defun read-map (lines)
  (let ((height (length lines))
        (width (length (car lines)))
        (cucs (make-hash-table :test #'equal)))
    (loop :for line :in lines
          :for y :from 0
          :do (loop :for char :across line
                    :for x :from 0
                    :when (or (char= char #\>) (char= char #\v))
                      :do (setf (gethash (cons x y) cucs) char)))
    (values cucs width height)))

(defun update (cucs width height &key vertical)
  (let ((new-cucs (make-hash-table :test #'equal))
        (moved-cucs 0))
    (loop :for pos :being :each hash-key :of cucs
          :for char = (gethash pos cucs)
          :do (cond
                ((and (not vertical)
                      (char= char #\>))
                 (let* ((new-x (1+ (car pos)))
                        (wrapped (mod new-x width))
                        (new-pos (cons wrapped (cdr pos))))
                   (cond
                     ((null (gethash new-pos cucs))
                      (incf moved-cucs)
                      (setf (gethash new-pos new-cucs) char))
                     (t (setf (gethash pos new-cucs) char)))))
                ((and vertical
                      (char= char #\v))
                 (let* ((new-y (1+ (cdr pos)))
                        (wrapped (mod new-y height))
                        (new-pos (cons (car pos) wrapped)))
                   (cond
                     ((null (gethash new-pos cucs))
                      (incf moved-cucs)
                      (setf (gethash new-pos new-cucs) char))
                     (t (setf (gethash pos new-cucs) char)))))
                (t (setf (gethash pos new-cucs) char))))
    (values new-cucs moved-cucs)))

(defun full-tick (cucs width height)
  (let ((new-cucs)
        (total-moved 0))
    (multiple-value-bind (updated num) (update cucs width height)
      (setf new-cucs updated)
      (incf total-moved num))
    (multiple-value-bind (updated num) (update new-cucs width height :vertical t)
      (setf new-cucs updated)
      (incf total-moved num))
    (values new-cucs total-moved)))

(defun part1 (cucs width height)
  (loop :for step :from 0
        :for (new-cucs moved) = (list cucs -1)
          :then (multiple-value-bind (updated moved) (full-tick new-cucs width height)
                  (list updated moved))
        :until (zerop moved)
        :finally (return step)))

(defun answer (&optional (file #P"input.txt"))
  (multiple-value-bind (cucs width height) (read-map (get-lines file))
    (format t "Part 1: ~d~%" (part1 cucs width height))))

(answer)
