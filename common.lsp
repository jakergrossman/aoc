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

; parse space separated words from a string
(defun parse-words (str)
  (let* ((skipped (skip-char str #\Space))
         (next-space (position #\Space skipped)))
    (cond
      ((null next-space) (list skipped))
      (t (append (list (subseq skipped 0 next-space)) (parse-words (subseq skipped next-space)))))))

; test whether a string is the empty string
(defun string-empty-p (str)
  (string= "" str))
