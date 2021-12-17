; Day 11: Corporate Policy

(load "../../include/lisp/common.lsp")

(defun nice-letter-p (letter)
  (and (not (char-equal #\i letter))
       (not (char-equal #\o letter))
       (not (char-equal #\l letter))))

; ticks a password, skipping those with naughty characters
(defun tick (password &optional (pos (- (length password) 1)))
  (let ((naughty-pos (position-if-not #'nice-letter-p password))
        (new-value (+ 1 (char-code (aref password pos)))))
    (cond
      ((not (null naughty-pos)) ; skip all passwords with naughty letters

        ; increment naughty letter ('j', 'p', and 'm' always valid)
        (setf (aref password naughty-pos)
              (code-char (+ 1 (char-code (aref password naughty-pos)))))

        ; zero out rest of password with 'a'
        (loop :for n
              :from (+ 1 naughty-pos)
              :below (length password)
              :do (setf (aref password n) #\a)))

      ((> new-value (char-code #\z)) ; roll over to next letter
          (if (> pos 0) (tick password (- pos 1)))
          (setf (aref password pos) #\a))

      (t (setf (aref password pos) (code-char new-value))))
    password))

(defun increasing-straight-p (password)
  (let ((checks (loop :for n :below (- (length password) 2)
                      :collect (let ((a (char-code (char password n)))
                                     (b (char-code (char password (+ 1 n))))
                                     (c (char-code (char password (+ 2 n)))))
                                 (and (eq b (+ 1 a))
                                      (eq c (+ 1 b)))))))
    (notevery #'null checks)))

(defun matching-pairs-p (password &optional (pair-seen nil))
  (cond
    ((< (length password) 2) nil)
    (t (let ((a (char password 0))
             (b (char password 1)))
         (cond
           ((eq a b) (if pair-seen
                         t
                         (matching-pairs-p (subseq password 2) t)))

           (t (matching-pairs-p (subseq password 1) pair-seen)))))))

(defun valid-p (password)
  (and (every #'nice-letter-p password)
       (increasing-straight-p password)
       (matching-pairs-p password)))

(defun next-password (password)
  (let ((new-password (tick (copy-seq password))))
    (loop while (not (valid-p new-password))
          do (tick new-password))
    new-password))

(defun answer (&optional (file #P"input.txt"))
  (let* ((input (car (get-input file)))
         (p1 (next-password input))
         (p2 (next-password p1)))
    (list p1 p2)))

(let ((a (answer)))
  (format t "Part 1: ~a~%" (car a))
  (format t "Part 2: ~a~%" (cadr a)))
