(load "inputs/input3")

; count the number of zeros and ones in a specified column
(defun count-digits (xs col zeros ones)
  (cond
    ((null xs) (list zeros ones))
    (t (cond
      ((eq (char (car xs) col) #\1)
          (count-digits (cdr xs) col zeros (+ 1 ones)))

      ((eq (char (car xs) col) #\0)
          (count-digits (cdr xs) col (+ 1 zeros) ones))
    ))
  )
)

(setq freq1
  (loop :for n :below (length (car input)) :collect (count-digits input n 0 0))
)

(setq gamma1
  (reduce
    (lambda (x y) (+ (* 2 x) y))
    (map 'list
      (lambda (x)
        (cond
          ((< (car x) (cadr x)) 0)
          (t 1)
        )
      )
      freq1
    )
  )
)

(setq epsilon1
  (reduce
    (lambda (x y) (+ (* 2 x) y))
    (map 'list
      (lambda (x)
        (cond
          ((< (car x) (cadr x)) 1)
          (t 0)
        )
      )
      freq1
    )
  )
)

; part1
(* epsilon1 gamma1)

; part2
