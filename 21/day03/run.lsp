; Day 3: Binary Diagnostic

(load "../../include/lisp/common.lsp")

(defun get-freq (lines)
  "Return an array where the Nth position's sign indicates the most common bit.

When the Nth position is positive, 1 is the most common bit.
When the Nth position is negative, 0 is the most common bit.
When the Nth position is 0, 1 and 0 are equally common."
   (let ((freq (make-array (length (car lines)) :initial-element 0)))
     (loop for line in lines do
       (loop for n below (length line)
             for char = (char line n)
             do (ecase char
                  (#\0 (decf (aref freq n)))
                  (#\1 (incf (aref freq n))))))
     freq))

(defun gamma (freq)
  (map 'string
       (lambda (x)
         (ecase (signum x)
           ((0 1) #\1)
           (-1 #\0)))
       freq))

(defun epsilon (freq)
  (map 'string
       (lambda (x)
         (ecase (signum x)
           (-1 #\1)
           ((0 1) #\0)))
       freq))

(defun filter (rating-type freq)
  "Determine which value to filter based on RATING-TYPE and FREQ"
  (ecase rating-type
    (oxygen (if (>= freq 0) #\1 #\0))
    (co2 (if (< freq 0) #\1 #\0))))

(defun rating (type lines)
  (let ((remaining lines))
    (loop while (> (length remaining) 1)
          for pos from 0
          for freq = (get-freq remaining)
          for filter = (filter type (aref freq pos))
          do (setf remaining (remove filter remaining :key (lambda (x) (char x pos))))
          finally (return (car remaining)))))

(defun answer (&optional (file #P"input.txt"))
  (let* ((lines (get-lines file))
         (freq (get-freq lines))
         (gamma-rate (gamma freq))
         (epsilon-rate (epsilon freq))
         (oxygen-rating (rating 'oxygen lines))
         (co2-rating (rating 'co2 lines)))
    (format t "Part 1: ~d~%" (* (parse-integer gamma-rate :radix 2)
                                (parse-integer epsilon-rate :radix 2)))
    (format t "Part 2: ~d~%" (* (parse-integer oxygen-rating :radix 2)
                                (parse-integer co2-rating :radix 2)))))

(answer)
