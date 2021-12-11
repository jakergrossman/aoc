; Day 3

(load "../../include/common.lsp")

(setq input (car (get-input "input.txt")))

(setq visited1 (make-hash-table :test #'equal))
(setq visited2 (make-hash-table :test #'equal))

(defun do-part1 (str &optional (x 0) (y 0))
  (let ((key (list x y)))
    (cond ((null (gethash key visited1)) (setf (gethash key visited1) 1))))

  (cond
    ((string-empty-p str) t)
    ((eq #\> (char str 0)) (do-part1 (subseq str 1) (+ 1 x) y))
    ((eq #\< (char str 0)) (do-part1 (subseq str 1) (- x 1) y))
    ((eq #\^ (char str 0)) (do-part1 (subseq str 1) x (+ 1 y)))
    ((eq #\v (char str 0)) (do-part1 (subseq str 1) x (- y 1)))))

(defun do-part2 (str &optional (santa-x 0) (santa-y 0) (robo-x 0) (robo-y 0) (turn nil))
  (cond
    ((null turn) ; santa
      (let ((key (list santa-x santa-y)))
        (cond
          ((null (gethash key visited2))
           (setf (gethash key visited2) 1))))
      (cond
        ((string-empty-p str) t)
        ((eq #\> (char str 0)) (do-part2 (subseq str 1) (+ 1 santa-x) santa-y robo-x robo-y (not turn)))
        ((eq #\< (char str 0)) (do-part2 (subseq str 1) (- santa-x 1) santa-y robo-x robo-y (not turn)))
        ((eq #\^ (char str 0)) (do-part2 (subseq str 1) santa-x (+ 1 santa-y) robo-x robo-y (not turn)))
        ((eq #\v (char str 0)) (do-part2 (subseq str 1) santa-x (- santa-y 1) robo-x robo-y (not turn)))))
    (t ; robo santa
      (let ((key (list robo-x robo-y)))
        (cond ((null (gethash key visited2)) (setf (gethash key visited2) 1))))

      (cond
        ((string-empty-p str) t)
        ((eq #\> (char str 0)) (do-part2 (subseq str 1) santa-x santa-y (+ 1 robo-x) robo-y (not turn)))
        ((eq #\< (char str 0)) (do-part2 (subseq str 1) santa-x santa-y (- robo-x 1) robo-y (not turn)))
        ((eq #\^ (char str 0)) (do-part2 (subseq str 1) santa-x santa-y robo-x (+ 1 robo-y) (not turn)))
        ((eq #\v (char str 0)) (do-part2 (subseq str 1) santa-x santa-y robo-x (- robo-y 1) (not turn)))))))


(compile 'do-part1)
(compile 'do-part2)

(do-part1 input)
(do-part2 input)

(format t "Part 1: ~d~%" (hash-table-count visited1))
(format t "Part 2: ~d~%" (hash-table-count visited2))
