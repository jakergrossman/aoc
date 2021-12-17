;  Day 14: Extended Polymerization

(load "../../include/lisp/common.lsp")

(defvar *rules* nil)
(defvar *pair-counts* nil)
(defvar *quantities* nil)

(defun process-input-rules (lines hash)
  (loop :for line :in lines :do
      (let* ((words (parse-words line))
             (first (char (car words) 0))
             (second (char (car words) 1))
             (mid (char (caddr words) 0)))
        (setf (gethash (cons first second) hash)
                       (list (cons first mid) (cons mid second))))))

; create a list of pairs of adjacent letters
(defun get-pairs (letters)
  (loop :for n :below (- (length letters) 1)
        :collect (cons (nth n letters) (nth (+ 1 n) letters))))

; process a step of polymerization
(defun tick ()
  (let ((pairs (loop :for key :being :each hash-key in *pair-counts*
                     :collect (list key (gethash key *pair-counts*))))
        (new-pairs (make-hash-table :test #'equal)))
    (loop :for pair :in pairs
          :for (value count) = pair
          :for (first second) = (gethash value *rules*)
          :for new-element = (cdr first)

          ; add new element to quantities
          :do  (cond ((null (gethash new-element *quantities*))
                        (setf (gethash new-element *quantities*) 1))

                     (t (setf (gethash new-element *quantities*)
                        (+ count (gethash new-element *quantities*)))))

          ; first pair
          :do  (cond ((null (gethash first new-pairs))
                        (setf (gethash first new-pairs) count))

                     (t (setf (gethash first new-pairs)
                        (+ (gethash first new-pairs) count))))

          ; second pair
          :do  (cond ((null (gethash second new-pairs))
                        (setf (gethash second new-pairs) count))

                     (t (setf (gethash second new-pairs)
                        (+ (gethash second new-pairs) count)))))

    (setf *pair-counts* new-pairs)))

(defun answer (&optional (file #P"input.txt"))
  (let ((input (get-input file))
        (start-elements nil)
        (part1 nil)
        (part2 nil))

    (setf *rules* (make-hash-table :test #'equal))
    (setf *pair-counts* (make-hash-table :test #'equal))
    (setf *quantities* (make-hash-table :test #'equal))

    ; process element addition rules
    (process-input-rules (cddr input) *rules*)

    ; initial string as list
    (setf start-elements (coerce (car input) 'list))

    ; add initial element frequencies
    (loop :for c :in start-elements
          :do (cond
                ((null (gethash c *quantities*))
                   (setf (gethash c *quantities*) 1))

                (t (setf (gethash c *quantities*)
                         (+ 1 (gethash c *quantities*))))))

    ; add initial pair frequencies
    (loop :for pair :in (get-pairs start-elements)
          :do (cond ((null (gethash pair *pair-counts*))
                        (setf (gethash pair *pair-counts*) 1))

                    (t  (setf (gethash pair *pair-counts*)
                              (+ 1 (gethash pair *pair-counts*))))))

    (dotimes (i 10) (tick))
    (setf part1 (sort (loop :for key being :each hash-key in *quantities*
                            :collect (gethash key *quantities*)) #'>))

    (dotimes (i 30) (tick))
    (setf part2 (sort (loop :for key being :each hash-key in *quantities*
                            :collect (gethash key *quantities*)) #'>))

    (format t "Part 1: ~d~%" (- (car part1) (car (last part1))))
    (format t "Part 2: ~d~%" (- (car part2) (car (last part2))))))

(answer)
