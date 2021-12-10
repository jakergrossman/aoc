; Day 8: Seven Segment Search

(load "../common.lsp")

; split into ((signal-patterns) (output))
(defun process-line (line)
  (let ((words (parse-words line)))
    (list (butlast words 5) (nthcdr 11 words))))

(setq input
  (get-input
    "input.txt" :process #'process-line :predicate #'string-empty-p))

(defun unique (x)
  (let ((len (length x)))
    (cond
      ((eq 2 len) T) ; 1
      ((eq 4 len) T) ; 4
      ((eq 3 len) T) ; 7
      ((eq 7 len) T) ; 8
      (t nil))))

; count # of words with 2, 3, 4, or 7 characters
(defun get-unique (words) (length (remove-if-not #'unique words)))

; check thay every character of key1 is in key2
(defun is-in (key1 key2)
  (cond
    ((string= "" key1) t)
    ((null (position (char key1 0) key2)) nil)
    (t (is-in (subseq key1 1) key2))))

; count how many letters are in key1 that are also in key2
(defun shared (key1 key2)
  (cond
    ((string= "" key1) 0)
    ((null (position (char key1 0) key2))
      (shared (subseq key1 1) key2))
    (t
      (+ 1 (shared (subseq key1 1) key2)))))

; incrementally calculate decode table
(defun create-decode (keys)
  (let* ((sorted (map 'list (lambda (x) (sort x #'string<=)) keys))
        (segment1 (find-if (lambda (x) (eq 2 (length x))) sorted))
        (segment4 (find-if (lambda (x) (eq 4 (length x))) sorted))
        (segment7 (find-if (lambda (x) (eq 3 (length x))) sorted))
        (segment8 (find-if (lambda (x) (eq 7 (length x))) sorted))
        (segment9 (find-if (lambda (x) (and (eq 6 (length x)) (is-in segment4 x))) sorted))
        (segment3 (find-if (lambda (x) (and (eq 5 (length x)) (is-in segment1 x))) sorted))
        (segment0 (find-if
                    (lambda (x)
                      (and
                        (eq 6 (length x))
                        (and (not (eq x segment9)) (eq (shared x segment1) 2))))
                    sorted))
        (segment6 (find-if
                    (lambda (x)
                      (and
                        (eq 6 (length x))
                        (and (not (eq x segment9)) (not (eq x segment0)))))
                    sorted))
        (segment5 (find-if (lambda (x) (and (eq 5 (length x)) (is-in x segment6))) sorted))
        (segment2 (find-if
                    (lambda (x)
                      (and
                        (eq 5 (length x))
                        (and (not (eq x segment5)) (not (eq x segment3)))))
                    sorted))
        (table-entries (list
          (list segment0 0) (list segment1 1) (list segment2 2) (list segment3 3)
          (list segment4 4) (list segment5 5) (list segment6 6) (list segment7 7)
          (list segment8 8) (list segment9 9)))) ; end let declarations

      ; create decode lambda
      (let ((hash (make-hash-table :test #'equal)))
        (loop :for entry :in table-entries :do (setf (gethash (car entry) hash) (cadr entry)))
        hash)))

; convert digit list to base-10 value
(defun digit-dec (digits)
  (reduce (lambda (x y) (+ (* 10 x) y)) digits))

(setq decoded-values
  (loop :for line :in input
    :collect
    (let ((hash (create-decode (car line))))
      (digit-dec
        (loop :for output-value :in (cadr line)
          :collect
          (gethash (sort output-value #'string<=) hash))))))

(format t "Part 1: ~d~%" (reduce '+ (map 'list (lambda (x) (get-unique (cadr x))) input)))
(format t "Part 2: ~d~%" (reduce '+ decoded-values))
