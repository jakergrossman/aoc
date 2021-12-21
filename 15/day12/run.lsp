; Day 12: JSAbacusFramework.io
;
; TODO: Part 2, would like to write an actual JSON parser

(load "../../include/lisp/common.lsp")

(defun numbers-only (str)
  (let ((remaining-str (copy-seq str))
        (nums nil))
  (loop :while (not (null remaining-str))
        :do (multiple-value-bind (num new-str) (next-number remaining-str)
              (cond
                ((not (null num))
                  (setf nums (cons num nums))))
              (setf remaining-str new-str)))
  nums))

(defun answer (&optional (file #P"input.txt"))
  (let ((input (car (get-lines file))))
    (reduce #'+ (numbers-only input))))

(let ((a (answer)))
  (format t "Part 1: ~d~%" a)
  (format t "Part 2: Not Completed~%"))
