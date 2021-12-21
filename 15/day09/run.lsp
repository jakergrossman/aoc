; Day 9: All in a Single Night
;
; Brute Force Solution
; TODO some TSP algorithm

(load "../../include/lisp/common.lsp")

(defvar *costs* (make-hash-table :test #'equal))

(defun register-cities (lines &optional (hash (make-hash-table :test #'equal)))
  (loop for line in lines
        do (let* ((words (parse-words line))
                  (from (intern (nth 0 words)))
                  (to (intern (nth 2 words)))
                  (cost (parse-integer (nth 4 words))))
              (setf (gethash (cons from to) hash) cost)
              (setf (gethash (cons to from) hash) cost)))
  (values (remove-duplicates (loop for key being each hash-key in hash collect (car key)))
          hash))

(defvar *cities* (remove-duplicates (loop for key being each hash-key in *costs* collect (car key))))

(defun path-cost (cities costs)
  (loop for i from 0 below (- (length cities) 1)
        sum (gethash (cons (nth i cities) (nth (+ 1 i) cities)) costs)))

(defun answer (&optional (file #P"input.txt"))
  (multiple-value-bind (cities costs) (register-cities (get-lines file))
    (let* ((path-costs (mapcar (lambda (x) (path-cost x costs)) (permute cities)))
           (min-cost (reduce #'min (subseq path-costs 0)))
           (max-cost (reduce #'max (subseq path-costs 0))))
      (values min-cost max-cost))))

(multiple-value-bind (p1 p2) (answer)
  (format t "Part 1: ~d~%" p1)
  (format t "Part 2: ~d~%" p2))
