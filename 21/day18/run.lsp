; Day 18: Snailfish

(load "../../include/lisp/common.lsp")

(defun cat (&rest strings)
  "Shorthand for concatenating a list of strings"
  (apply #'concatenate 'string strings))

(defun num (str start)
  (parse-integer str :start start :junk-allowed t))

(defun snail-read (str)
  "Read a snail number from the string representation into a tree"
  (read-from-string
    (with-output-to-string (out)
      (with-input-from-string (in str)
        (loop :for char = (read-char in nil nil)
              :until (null char)
              :do (case char
                    (#\, (write-string " . " out))
                    (#\[ (write-char #\( out))
                    (#\] (write-char #\) out))
                    (t (write-char char out))))))))

(defun snail-scan (number)
  "Scan the snail number for values that need to be exploded or split"
  (let ((depth 0) ; current depth
        (num-literal 0) ; accumulated value of current normal number-literal
        (split nil) ; information about first number to be split
        (digit 0)) ; most recent digit
    (dotimes (i (length number))
      (let ((char (char number i)))
        (cond ((char= char #\[)
               (when (= 4 depth)
                 (return-from snail-scan (values :explode i)))
               (incf depth))

              ((char= char #\])
               (decf depth)
               (setf num-literal 0))

              ((setf digit (digit-char-p char))
               (setf num-literal (+ (* num-literal 10) digit))
               (when (and (>= num-literal 10) (null split))
                 (setf split (list :split (1- i) num-literal))))

              ((char= char #\,)
               (setf num-literal 0))

              (t (error "Unexpected char ~s." char)))))
    (if split
      (values-list split)
      :done)))

(defun snail-explode-left (number pos exploded)
  "Apply the explode operation to the left side of the exploded pair"
  (let* ((left-digit-pos
           (position-if #'digit-char-p number :end pos :from-end t))
         (left-num-end-pos
           (and left-digit-pos
                (1+ left-digit-pos)))
         (left-num-pos
           (and left-digit-pos
                (1+
                 (or (position-if-not #'digit-char-p number :end left-digit-pos :from-end t)
                     (1- left-digit-pos)))))
         (left-num
           (and left-num-pos (num number left-num-pos))))
    (if (null left-num)
        (subseq number 0 pos)
        (cat (subseq number 0 left-num-pos)
             (princ-to-string (+ left-num exploded))
             (subseq number left-num-end-pos pos)))))

(defun snail-explode-right (number end-pos exploded)
  "Apply the explode operation to the right side of the exploded pair"
  (let* ((right-num-pos
           (position-if #'digit-char-p number :start end-pos))
         (right-num-end-pos
           (and right-num-pos
                (position-if-not #'digit-char-p number :start right-num-pos)))
         (right-num
           (and right-num-pos (num number right-num-pos))))
    (if (null right-num)
        (subseq number end-pos)
        (cat (subseq number end-pos right-num-pos)
             (princ-to-string (+ right-num exploded))
             (subseq number right-num-end-pos)))))

(defun snail-explode (number pos)
  (let* ((end-pos (1+ (position #\] number :start pos)))
         (a (num number (1+ pos)))
         (comma-pos (position #\, number :start pos))
         (b (num number (1+ comma-pos))))
    (cat (snail-explode-left number pos a)
         "0"
         (snail-explode-right number end-pos b))))

(defun snail-split (number pos n)
  (cat (subseq number 0 pos)
       (format nil "[~d,~d]" (truncate n 2) (ceiling n 2))
       (subseq number (+ pos 2))))

(defun snail-reduction-step (number)
  (multiple-value-bind (action pos n)
      (snail-scan number)
    (ecase action
      (:explode
        (snail-explode number pos))
      (:split
        (snail-split number pos n))
      (:done number))))


(defun snail-reduce (number)
  "Repeatedly reduce `number` as many times as possible"
  (loop :for before = number :then after
        :for after = (snail-reduction-step before)
        :until (equal before after)
        :finally (return after)))

(defun snail-add (a b)
  (snail-reduce (format nil "[~A,~A]" a b)))

(defun snail-sum (numbers)
  (reduce #'snail-add numbers))

(defun magnitude (tree)
  (if (integerp tree)
      tree
      (+ (* 3 (magnitude (car tree)))
         (* 2 (magnitude (cdr tree))))))

(defun largest-sum (numbers)
  (let ((largest 0))
    (loop :for n :in numbers :do
      (loop :for m :in numbers
            :for sum = (magnitude (snail-read (snail-add n m)))
            :when (> sum largest)
            :do (setf largest sum)))
    largest))

(defun answer (&optional (file #P"input.txt"))
  (let* ((numbers (get-input file))
         (p1 (magnitude (snail-read (snail-sum numbers))))
         (p2 (largest-sum numbers)))
    (format t "Part 1: ~d~%" p1)
    (format t "Part 2: ~d~%" p2)))

(answer)
