; read in lines of a file
;
; predicate - function to apply to decide whether to omit a line
; keep - boolean that inverts the behavior of `predicate` (only accept lines which satisfy `predicate`)
; process - function to apply to each accepted line
(defun get-input (filename &key predicate keep process)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
      while line
      unless
        (cond
          ((null predicate) nil) ; no predicate, always keep
          ((null keep) (funcall predicate line)) ; predicate, throw away matches
          (t (not (funcall predicate line)))) ; predicate, keep matches
      collect
      (cond
        ((null process) line)
        (t (funcall process line))))))

; skip the beginning of str that matches
; the character c
(defun skip-char (str c)
  (let ((non-c-pos (position-if-not (lambda (x) (eq x c)) str)))
    (cond
      ((null non-c-pos) str)
      (t (subseq str non-c-pos)))))

; parse delimiter-separated integers
; from a string
(defun parse-integers (str delim)
  (let* ((skip-start (skip-char str delim))
        (delim-pos (position delim skip-start)))
    (cond
      ((null delim-pos)
        (list (parse-integer skip-start)))
      (t
        (append
          (list (parse-integer (subseq skip-start 0 delim-pos)))
          (parse-integers (subseq skip-start (+ 1 delim-pos)) delim))))))

; parse delim separated words from a string
(defun parse-words (str &optional (delim #\Space))
  (let* ((skipped (skip-char str delim))
         (next-delim (position delim skipped)))
    (cond
      ((null next-delim) (list skipped))
      (t (append (list (subseq skipped 0 next-delim)) (parse-words (subseq skipped next-delim) delim))))))

; test whether a string is the empty string
(defun string-empty-p (str)
  (string= "" str))

(defun number-char-p (c)
  (or (digit-char-p c)
      (char-equal #\- c)))

; like `parse-integer` but behaves similar* to c's strtol:
; it returns two values, the first being the first number
; found in the string (possibly negative) and the second
; being the rest of the string after that number
;
; * similar as in returning both the value and information
;   about the remaining string
(defun next-number (str)
  (let ((next-pos (position-if #'number-char-p str)))
    (cond
      ((null next-pos)
        (values nil nil))

      (t (let ((end-pos (position-if-not #'number-char-p str :start next-pos)))
        (cond
          ((null end-pos)
             (values (parse-integer (subseq str next-pos))
                      nil))
          (t (values (parse-integer (subseq str next-pos end-pos))
                     (subseq str end-pos)))))))))

; get all permutations of a list
(defun permute (l)
  (labels ((discover (remaining)
              (cond ((null remaining) nil)
                    ((null (cdr remaining)) (list remaining))
                    (t (loop :for item :in remaining
                             append (mapcar (lambda (x) (cons item x))
                                            (discover (remove item remaining))))))))
    (discover l)))
