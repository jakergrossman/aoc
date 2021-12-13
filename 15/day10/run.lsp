; Day 10: Elves Look, Elves Say

#+(or :GCL :CLISP)
(eval-when (:compile-toplevel :execute)
  (format t "This day is too slow on GCL or CLISP,~%use SBCL instead.~%")
  (quit))

(load "../../include/common.lsp")

; enter the next digit into state
;
; if it is different than the previous
; digit, append the digit's frequency and
; value to the state and add a new entry
; for the new digit
;
; State is of the form
;
;  ( ( a . b ) c1 c2 ... cn )
;
; Where a is the current character, b
; is the current characters count,
; and {c1,c2,...,cn} are the already
; calculated digits
(defun count-digit (digit state)
  (let* ((curr-data (car state))
         (curr-char (car curr-data))
         (curr-count (cdr curr-data)))
    (cond
      ; same as previous digit
      ((eq digit curr-char)
          (cons (cons curr-char
                      (+ 1 curr-count))
                (cdr state)))

      ; different from previous digit
      (t (cons (cons digit 0)
               (append (list (+ curr-count 1)
                             curr-char)
                       (cdr state)))))))

; Walk over the sequence with count-digit
; to collect the new sequence
(defun next-sequence (seq)
  (let ((next-result (reduce #'count-digit
                             seq
                             :from-end t
                             :initial-value (list (cons (car (last seq)) -1)))))

    ; last character & frequency is still
    ; on the top of the returned state
    (append (list (+ 1 (cdar next-result))
                  (caar next-result))
            (cdr next-result))))

(defun answer (iterations &key (file "input.txt") seed)
  (let ((seed1 (if (null seed)
                   (mapcar #'digit-char-p (coerce (car (get-input file)) 'list))
                   seed)))
        (dotimes (i iterations) (setf seed1 (next-sequence seed1)))
        seed1))

(compile 'next-sequence)

(setq p1-digits (answer 40))
(setq p2-digits (answer 10 :seed p1-digits))

(format t "Part 1: ~d~%" (length p1-digits))
(format t "Part 2: ~d~%" (length p2-digits))
