; Day 1: Not Quite Lisp

(load "../../include/common.lsp")

(defun val (c)
  (cond ((eq #\( c) 1)
        ((eq #\) c) -1)))

; append the new current floor to
; the front of previously visited floors
(defun count-floor (floors next)
  (cons (+ (val next) (car floors)) floors))

(defun answer (&optional (file #P"input.txt"))
  (let* ((input (car (get-input file)))
         (floors (reduce #'count-floor input :initial-value '(0))))
    (format t "Part 1: ~d~%" (car floors)) ; floors is reversed
    (format t "Part 2: ~d~%" (position-if (lambda (x) (< x 0)) (reverse floors)))))

(answer)
