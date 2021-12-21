; Day 16: Packet Decoder

(load "../../include/lisp/common.lsp")

#+(or :SBCL :CLISP)
(eval-when (:compile-toplevel :execute)
  (format t "Broken for now on SBCL and CLISP, try GCL~%~%")
  (quit))

(defun bin-dec (digits)
  (reduce (lambda (x y) (+ y (* 2 x))) digits))

(defun char-to-bits (c)
  (cond
    ((char-equal #\0 c) '(0 0 0 0)) ((char-equal #\1 c) '(0 0 0 1))
    ((char-equal #\2 c) '(0 0 1 0)) ((char-equal #\3 c) '(0 0 1 1))
    ((char-equal #\4 c) '(0 1 0 0)) ((char-equal #\5 c) '(0 1 0 1))
    ((char-equal #\6 c) '(0 1 1 0)) ((char-equal #\7 c) '(0 1 1 1))
    ((char-equal #\8 c) '(1 0 0 0)) ((char-equal #\9 c) '(1 0 0 1))
    ((char-equal #\A c) '(1 0 1 0)) ((char-equal #\B c) '(1 0 1 1))
    ((char-equal #\C c) '(1 1 0 0)) ((char-equal #\D c) '(1 1 0 1))
    ((char-equal #\E c) '(1 1 1 0)) ((char-equal #\F c) '(1 1 1 1))))

; Convert a hexadecimal-encoded string to binary bits
; Done like this so that bits are preserved exactly as they appear,
; without dropping the leading zeros
(defun parse-hex (str)
  (reduce #'append (mapcar #'char-to-bits (coerce str 'list))))

(defun parse-subpacket (bits pos cap packets &key by-segment)
  (cond
    ((null by-segment)
       (cond ((eq pos cap) (values (reverse packets) pos))
             (t (multiple-value-bind (packet offset) (parse-packet (subseq bits pos))
                 (parse-subpacket bits (+ pos offset) cap (cons packet packets))))))
    (t (cond ((eq 0 cap) (values (reverse packets) pos))
            (t (multiple-value-bind (packet offset) (parse-packet (subseq bits pos))
                (parse-subpacket bits (+ pos offset) (- cap 1) (cons packet packets) :by-segment t)))))))

(defun parse-packet (bits)
  (let* ((version (bin-dec (subseq bits 0 3)))
         (type (bin-dec (subseq bits 3 6)))
         (payload (subseq bits 6)))
    (cond
      ((eq type 4)
          (let ((num-data (loop :for n :from 0
                            :for offset = (+ 6 (* n 5))
                            :for chunk = (subseq bits offset (+ 5 offset))

                            :until (and (> n 0)
                                        (zerop (nth (+ (- offset 5)) bits)))

                            :sum 5 :into total-offset
                            :append (cdr chunk) :into data-pieces
                            :finally (return (list data-pieces total-offset)))))
            (values (list (cons version type) (bin-dec (car num-data)))
                    (+ 6 (cadr num-data)))))

      ((eq (car payload) 0)
          (let ((len (bin-dec (subseq payload 1 16))))
            (multiple-value-bind (packet offset) (parse-subpacket (subseq payload 16)
                                                                  0 len nil)
              (values (list (cons version type) packet)
                      (+ 22 offset)))))

      ((eq (car payload) 1)
          (let ((len (bin-dec (subseq payload 1 12))))
            (multiple-value-bind (packet offset) (parse-subpacket (subseq payload 12)
                                                                  0 len nil :by-segment t)
              (values (list (cons version type) packet)
                      (+ 18 offset))))))))

(defun version-sum (packet &optional (total 0))
  (let ((meta (car packet))
        (data (cadr packet)))
    (cond
      ((listp data) (+ (car meta) (reduce #'+ (mapcar #'version-sum data))))

      (t (+ total (car meta))))))

(defun interpret-result (decoded-packet)
  (let ((op (cdar decoded-packet))
        (args (cadr decoded-packet)))
    (cond
      ((eq op 0) (let ((arg-values (mapcar #'interpret-result args)))
                     (reduce #'+ arg-values)))

      ((eq op 1) (let ((arg-values (mapcar #'interpret-result args)))
                    (reduce #'* arg-values)))


      ((eq op 2) (let ((arg-values (mapcar #'interpret-result args)))
                    (reduce #'min arg-values)))

      ((eq op 3) (let ((arg-values (mapcar #'interpret-result args)))
                    (reduce #'max arg-values)))

      ((eq op 4) args)

      ((eq op 5) (let ((a (interpret-result (car args)))
                       (b (interpret-result (cadr args))))
                   (cond ((> a b) 1) (t 0))))

      ((eq op 6) (let ((a (interpret-result (car args)))
                       (b (interpret-result (cadr args))))
                   (cond ((< a b) 1) (t 0))))

      ((eq op 7) (let ((a (interpret-result (car args)))
                       (b (interpret-result (cadr args))))
                   (cond ((eq a b) 1) (t 0)))))))

(defun answer (&optional (file #P"input.txt"))
  (let ((decoded-input (parse-packet (parse-hex (car (get-lines file))))))
    (format t "Part 1: ~d~%" (version-sum decoded-input))
    (format t "Part 2: ~d~%" (interpret-result decoded-input))))

(answer)
