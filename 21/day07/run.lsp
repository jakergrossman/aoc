; Day 7: The Treachery of Whales

; dynamic programming solution by u/Zyppie on reddit:
; https://www.reddit.com/r/adventofcode/comments/rar7ty/comment/hnmiagb/?utm_source=share&utm_medium=web2x&context=3

(load "../../include/lisp/common.lsp")

(setq input (parse-integers (car (get-lines "input.txt")) #\,))

(setq input-array (make-array (list (length input)) :initial-contents input))

(setq range (+ 1 (reduce #'max input-array)))

; convert input to crab frequency mapping
(setq crab-freq (make-array (list (+ 1 range)) :initial-element 0))
(loop :for n :below (length input-array)
  do
  (setf
    (aref crab-freq (aref input-array n))
    (+ 1 (aref crab-freq (aref input-array n)))))

(setq right-fuel-costs (make-array (list range) :initial-element 0))
(setq left-fuel-costs (make-array (list  range) :initial-element 0))

(defun fuel-right-cost1 (pos passed-crabs)
  (cond
    ((< pos range)
      (setf
        (aref right-fuel-costs pos)
        (+ (aref right-fuel-costs (- pos 1)) passed-crabs))
      (fuel-right-cost1
        (+ 1 pos)
        (+ passed-crabs (aref crab-freq pos))))
    (t right-fuel-costs)))

(defun fuel-left-cost1 (pos passed-crabs)
  (cond
    ((>= pos 0)
      (setf
        (aref left-fuel-costs pos)
        (+ (aref left-fuel-costs (+ pos 1)) passed-crabs))
      (fuel-left-cost1
        (- pos 1)
        (+ passed-crabs (aref crab-freq pos))))
    (t left-fuel-costs)))

(defun fuel-right-cost2 (pos passed-crabs move-cost)
  (let ((new-move-cost (+ move-cost passed-crabs)))
    (cond
      ((< pos range)
        (setf
          (aref right-fuel-costs pos)
          (+ (aref right-fuel-costs (- pos 1)) new-move-cost))
        (fuel-right-cost2
          (+ 1 pos)
          (+ passed-crabs (aref crab-freq pos))
          new-move-cost))
      (t right-fuel-costs))))

(defun fuel-left-cost2 (pos passed-crabs move-cost)
  (let ((new-move-cost (+ move-cost passed-crabs)))
    (cond
      ((>= pos 0)
        (setf
          (aref left-fuel-costs pos)
          (+ (aref left-fuel-costs (+ pos 1)) new-move-cost))
        (fuel-left-cost2
          (- pos 1)
          (+ passed-crabs (aref crab-freq pos))
          new-move-cost))
        (t left-fuel-costs))))

; compile to avoid invocation stack overflow
(compile 'fuel-left-cost2)
(compile 'fuel-right-cost2)

(fuel-left-cost1 (- range 2) (aref crab-freq (- range 1)))
(fuel-right-cost1 1 (aref crab-freq 0))
(setq costs1 (loop :for n :below range :collect (+ (aref right-fuel-costs n) (aref left-fuel-costs n))))

(fuel-left-cost2 (- range 2) (aref crab-freq (- range 1)) 0)
(fuel-right-cost2 1 (aref crab-freq 0) 0)
(setq costs2 (loop :for n :below range :collect (+ (aref right-fuel-costs n) (aref left-fuel-costs n))))

(format t "Part 1: ~d~%" (reduce 'min costs1))
(format t "Part 2: ~d~%" (reduce 'min costs2))
