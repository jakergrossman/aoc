; Day 7: Some Assembly Required

(load "../../include/lisp/common.lsp")

(defparameter *bitwidth* 16)
(defparameter *bitmask* 65535)

(setq wires (make-hash-table :test #'equal))

(defun parse-line (line)
  (let* ((words (parse-words line))
         (hint1 (car words))
         (hint2 (cadr words)))
    (cond
      ((eq 3 (length words))
        (list "ASSIGN" hint1 (nth 2 words)))

      ((string= "NOT" hint1)
        ; bitwise not of ID
        (list "NOT" (nth 1 words) (nth 3 words)))

      ((string= "SHIFT" (subseq hint2 1))
        (list (nth 1 words) (nth 0 words) (parse-integer (nth 2 words)) (nth 4 words)))

      (t
        ; a OP b -> c
        (list (nth 1 words) (nth 0 words) (nth 2 words) (nth 4 words))))))

(setq input (mapcar #'parse-line (get-lines "input.txt")))

(defun eval-expr (expr)
  (cond
    ((stringp expr)
      (cond
        ((every #'digit-char-p expr) (parse-integer expr))
        (t
          (let ((value (gethash expr wires)))
            (cond
              ((listp value) (eval-stmt value))
              (t value))))))
    (t expr)))

(defun eval-stmt (stmt)
  (let ((op (car stmt))
        (args (cdr stmt)))
    (cond
      ((string= op "ASSIGN")
        (let ((value (eval-expr (car args))))
          (setf (gethash (cadr args) wires) value)))

      ((string= op "NOT")
        (let ((value (eval-expr (car args))))
          (setf (gethash (cadr args) wires) (logxor *bitmask* value))))

      ((string= op "AND")
        (let ((value1 (eval-expr (car args)))
              (value2 (eval-expr (cadr args))))
          (setf (gethash (caddr args) wires) (logand value1 value2))))

      ((string= op "OR")
        (let ((value1 (eval-expr (car args)))
              (value2 (eval-expr (cadr args))))
          (setf (gethash (caddr args) wires) (logior value1 value2))))
      
      ((string= op "LSHIFT")
        (let ((value (eval-expr (car args)))
              (shift (cadr args)))
          (setf (gethash (caddr args) wires) (ash value shift))))

      ((string= op "RSHIFT")
        (let ((value (eval-expr (car args)))
              (shift (cadr args)))
          (setf (gethash (caddr args) wires) (ash value (* -1 shift))))))))

; register expression
(loop :for connect :in input
  do
  (let ((dest (car (last connect))))
    (setf (gethash dest wires) connect)))

(loop :for connection :in input :do (eval-stmt connection))

(setq part1 (gethash "a" wires))

(let ((b-pos (position "b" input :test #'equal :key (lambda (x) (car (last x))))))
  (setf (nth b-pos input) (list "ASSIGN" (write-to-string part1) "b")))

(clrhash wires)

; register expression
(loop :for connect :in input
  do
  (let ((dest (car (last connect))))
    (setf (gethash dest wires) connect)))

(loop :for connection :in input :do (eval-stmt connection))
(setq part2 (gethash "a" wires))

(format t "Part 1: ~d~%" part1)
(format t "Part 2: ~d~%" part2)
