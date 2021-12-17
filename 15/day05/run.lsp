; Day 5: Doesn't He Have Intern-Elves For This?

(load "../../include/lisp/common.lsp")

(setq input (get-input "input.txt" :predicate #'string-empty-p))

(defun vowel-p (char)
  (find char "aeiou" :test #'char-equal))

(defun double1-p (str)
  (cond
    ((< (length str) 2) nil)
    (t
      (let ((a (char str 0))
            (b (char str 1)))
        (cond
          ((char-equal a b) t)
          (t (double1-p (subseq str 1))))))))

(defun space-p (str)
  (cond
    ((< (length str) 3) nil)
    (t
      (let ((a (char str 0))
            (b (char str 2)))
        (cond
          ((char-equal a b) t)
          (t (space-p (subseq str 1))))))))

(defun double2-p (str)
  (cond
    ((< (length str) 4) nil)
    (t
      (let ((needle (subseq str 0 2))
            (haystack (subseq str 2)))
        (cond
          ((search needle haystack) t)
          (t (double2-p (subseq str 1))))))))

(defun nice1 (str)
  (and
    (>= (length (remove-if-not #'vowel-p str)) 3)
    (double1-p str)
    (and
      (null (search "ab" str))
      (null (search "cd" str))
      (null (search "pq" str))
      (null (search "xy" str)))))

(defun nice2 (str)
  (and (space-p str) (double2-p str)))

(format t "Part 1: ~d~%" (length (remove-if-not #'nice1 input)))
(format t "Part 2: ~d~%" (length (remove-if-not #'nice2 input)))
