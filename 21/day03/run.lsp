; Day 3: Binary Diagnostic

(load "../../include/common.lsp")

(setq input
  (get-input
    "input.txt" :predicate #'string-empty-p))

; count the number of zeros and ones in a specified column
(defun get-freq (xs col zeros ones)
  (cond
    ((null xs) (list zeros ones))
    (t (cond
      ((eq (char (car xs) col) #\1)
          (get-freq (cdr xs) col zeros (+ 1 ones)))

      ((eq (char (car xs) col) #\0)
          (get-freq (cdr xs) col (+ 1 zeros) ones))))))

; convert binary digits to decimal
;
; in:  list of binary digits, e.g.:      (1 0 1 1 0) (0 1 1 0 0)
; out: the decimal representation, e.g.: 20 12
(defun bin-to-dec (n)
  (reduce (lambda (x y) (+ (* 2 x) y)) n))

(setq digit-freq
  (loop :for n :below (length (car input)) :collect (get-freq input n 0 0)))

(setq gamma-bin
  (map 'list
    (lambda (x)
      (cond
        ((< (car x) (cadr x)) 0)
        (t 1)))
    digit-freq))

(setq gamma-dec (bin-to-dec gamma-bin))

(setq epsilon-bin
  (map 'list
    (lambda (x)
      (cond
        ((< (car x) (cadr x)) 1)
        (t 0)))
    digit-freq))

(setq epsilon-dec (bin-to-dec epsilon-bin))

(defun pick-filter (rating-type freq)
  (cond
    ((eq rating-type 'oxygen)
      ; oxygen rating filter is greater of 1 & 0
      (cond
        ((> (car freq) (cadr freq)) #\0)
        (t #\1)))

    ((eq rating-type 'co2)
      ; co2 rating filter is lesser of 1 & 0
      (cond
        ((> (car freq) (cadr freq)) #\1)
        (t #\0)))))


; find either the oxygen or co2 rating
(defun find-rating (rating-type xs pos)
  (cond
    ((null xs) '())
    ((eq 1 (length xs)) (car xs))
    (t
      (let* ((freq (get-freq xs pos 0 0))
            (filter (pick-filter rating-type freq)))
        (find-rating rating-type (remove-if-not (lambda (x) (eq filter (char x pos))) xs) (+ pos 1))))))

(setq oxygen-rating (parse-integer (find-rating 'oxygen input 0) :radix 2))
(setq co2-rating (parse-integer (find-rating 'co2 input 0) :radix 2))

(format t "Part 1: ~d~%" (* epsilon-dec gamma-dec))
(format t "Part 2: ~d~%" (* co2-rating oxygen-rating))
