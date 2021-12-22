; Day 11: Dumbo Octopus

(load "../../include/lisp/common.lsp")

(defun process-line (line)
  (map 'list #'digit-char-p (coerce line 'list)))

(defparameter *side-length* 10)

(let ((raw-input (mapcar #'process-line (get-lines "input.txt"))))
  (defparameter input1 (make-array (list *side-length* *side-length*) :initial-contents raw-input))
  (defparameter input2 (make-array (list *side-length* *side-length*) :initial-contents raw-input)))

(defun index-xy (index)
  (multiple-value-bind (y x) (floor index *side-length*) (list x y)))

; run one step of simulation
(defun tick (state)
  ; +1 to every octopus' power level
  (dotimes (i (array-total-size state))
    (setf (row-major-aref state i) (+ 1 (row-major-aref state i))))

  ; must use #'equal because keys are coordinate list (x y)
  (propagate state (make-hash-table :test #'equal)))

; propagate burst octos for a single step
(defun propagate (state visited)
  (let ((next-bursts
         (loop
          :for n
          :below (array-total-size state)
          :unless
            (or
              (< (row-major-aref state n) 10)
              (gethash (index-xy n) visited))
          :collect (index-xy n))))
    (cond
      ((null next-bursts)
        ; no more to burst, power-down octos
        (dotimes (i (array-total-size state))
          (cond
            ((> (row-major-aref state i) 9) (setf (row-major-aref state i) 0))))

        (length
          (loop ; count number of zeros (bursted octos)
            :for n
            :below (array-total-size state)
            :if (eq 0 (row-major-aref state n))
            :collect n)))

      (t
        (loop :for next :in next-bursts
          :do
          (burst (car next) (cadr next) state)

          ; add bursted octo to visited
          (setf (gethash next visited) t))
        (propagate state visited)))))

; get in-bound neighbors positions and values
(defun get-neighbors (x y state)
  (reduce #'append
    (loop :for m :from (- y 1) :to (+ y 1)
      :collect
      (loop :for n :from (- x 1) :to (+ x 1)
        :unless
          (or
            (< n 0) (>= n *side-length*)
            (< m 0) (>= m *side-length*)
            (and (eq n x) (eq m y)))
        :collect (list (list n m) (aref state m n))))))

; burst an individual octo
(defun burst (x y state)
  (let ((neighbors (get-neighbors x y state)))
    (loop :for neighbor :in neighbors
      :do
      (let ((x (caar neighbor))
            (y (cadar neighbor))
            (val (cadr neighbor)))
        (setf (aref state y x) (+ val 1))))))

; collect flash-counts for first 100 ticks
(defparameter total-flashes (loop :for n :below 100 :collect (tick input1)))

; could just add 100 and use the already
; modified state, but who's to say the first unison isn't < 100
(defun find-unison (pos)
  (let ((flashes (tick input2)))
    (cond
      ((eq flashes (* *side-length* *side-length*)) pos)
      (t (find-unison (+ 1 pos))))))

(defparameter unison-step (find-unison 1))

(format t "Part 1: ~d~%" (reduce '+ total-flashes))
(format t "Part 2: ~d~%" unison-step)
