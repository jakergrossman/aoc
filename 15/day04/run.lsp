; Day 4: The Ideal Stocking Stuffer
;
; Note: This only works on SBCL atm since it has built-in md5-hashing

#+(or :GCL :CLISP)
(eval-when (:compile-toplevel :execute)
  (format t "This file is only~%compatible with SBCL.~%")
  (quit))

(load "../../include/lisp/common.lsp")

(require :asdf)
(require :sb-md5)

(setq input (car (get-lines "input.txt")))

(defun part1 (&optional (n 1))
  (let* ((str (concatenate 'string input (write-to-string n)))
        (hash (sb-md5:md5sum-string str)))
    (cond
      ((and
        (eq (aref hash 0) 0)
        (eq (aref hash 1) 0)
        (< (aref hash 2) 16) n))
      (t (part1 (+ 1 n))))))

(defun part2 (&optional (n 1))
  (let* ((str (concatenate 'string input (write-to-string n)))
        (hash (sb-md5:md5sum-string str)))
    (cond
      ((and
        (eq (aref hash 0) 0)
        (eq (aref hash 1) 0)
        (eq (aref hash 2) 0) n))
      (t (part2 (+ 1 n))))))

(format t "Part 1: ~d~%" (part1))
(format t "Part 2: ~d~%" (part2))
