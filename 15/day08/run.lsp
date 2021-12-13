; Day 8: Matchsticks

(load "../../include/common.lsp")

(setq input (get-input "input.txt"))

(defun count-bytes (str)
  (cond
    ((string-empty-p str) 0)
    (t
      (let* ((first-char (char str 0)))
        (cond

          ; all escape characters
          ((char-equal #\\ first-char)
            (cond
              ((char-equal #\x (char str 1))
                (+ 1 (count-bytes (subseq str 4))))
              (t (+ 1 (count-bytes (subseq str 2))))))

          ; literal quotes (first and last char of line)
          ((char-equal #\" first-char)
            (count-bytes (subseq str 1)))

          (t (+ 1 (count-bytes (subseq str 1)))))))))

(defun escaped-length (str)
  (let ((chars (coerce str 'list)))
    (+ 2 ; quotes
      (reduce
        (lambda (x y)
          (cond
            ((or (eq y #\\) (eq y #\"))
              (+ 2 x))
            (t (+ 1 x))))
        chars
        :initial-value 0))))

(setq num-code-chars (reduce '+ (mapcar #'length input)))
(setq num-mem-chars (reduce '+ (mapcar #'count-bytes input)))

(setq num-escaped-chars (reduce '+ (mapcar #'escaped-length input)))

(format t "Part 1: ~d~%" (- num-code-chars num-mem-chars))
(format t "Part 2: ~d~%" (- num-escaped-chars num-code-chars))
