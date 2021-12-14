; Day 16: Aunt Sue

(load "../../include/common.lsp")

(defparameter *facts* '((|children| . 3) (|cats|        . 7)
                        (|samoyeds| . 2) (|pomeranians| . 3)
                        (|akitas|   . 0) (|vizslas|     . 0)
                        (|goldfish| . 5) (|trees|       . 3)
                        (|cars|     . 2) (|perfumes|    . 1)))

(defun process-line (line sues)
  (let* ((strip-line (remove #\Space line))
         (colon-pos (position #\: strip-line))
         (sue-num (parse-integer (subseq strip-line 3 colon-pos)))
         (fields (parse-words (subseq strip-line (+ 1 colon-pos)) #\,)))

    ; identify this sue in the sue-list...
    (setf (aref sues (- sue-num 1)) (list (cons '|id| sue-num)))

    ; ...and append the values for this sue
    (loop :for field :in fields
          :for (key value) = (parse-words field #\:)
          :do (setf (aref sues (- sue-num 1))
                    (cons (cons (intern key)
                                (parse-integer value))
                          (aref sues (- sue-num 1)))))))

(defun wrong-sue-p (sue key value &key use-range)
  (let ((sue-data (assoc key sue)))
    (cond
      ((null sue-data) nil)

      ((and use-range
            (or (eq '|cats| key)
                (eq '|trees| key)))
                   (<= (cdr sue-data) value))

      ((and use-range
            (or (eq '|pomeranians| key)
                (eq '|goldfish| key)))
                   (>= (cdr sue-data) value))

      (t (not (eq value (cdr sue-data)))))))

(defun filter-sues (sues &key use-range)
  (let ((remaining-sues sues))
    (loop :for fact :in *facts*
          :for (key . value) = fact
          :do (setf remaining-sues
                    (remove-if (lambda (x) (wrong-sue-p x key value :use-range use-range))
                               remaining-sues)))
  remaining-sues))

(defun answer (&optional (file #P"input.txt"))
  (let* ((input (get-input file))
         (sues (make-array '(500) :initial-element nil))
         (p1 nil)
         (p2 nil))

    ; fill sue data
    (mapc (lambda (x) (process-line x sues)) input)

    (setf p1 (aref (filter-sues sues) 0))
    (setf p2 (aref (filter-sues sues :use-range t) 0))

    (format t "Part 1: ~d~%" (cdr (assoc '|id| p1)))
    (format t "Part 2: ~d~%" (cdr (assoc '|id| p2)))))

(answer)
