; read in all lines of a file, optionally
; processing each line as it is read with process-line
(defun get-input (filename &optional process predicate)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
      while (cond
              ((null predicate) line)
              (t (and line (funcall predicate line))))
      collect (cond
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

; parse space separated words from a string
(defun parse-words (str)
  (let* ((skipped (skip-char str #\Space))
         (next-space (position #\Space skipped)))
    (cond
      ((null next-space) (list skipped))
      (t (append (list (subseq skipped 0 next-space)) (parse-words (subseq skipped next-space)))))))
