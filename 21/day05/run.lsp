; Day 5: Hydrothermal Venture

(load "../../include/common.lsp")

(defparameter *side-length* 1000)

; process an input line, returning ((x1 y1) (x2 y2))
(defun process-line (line)
  (let* ((words (parse-words line))
         (a (parse-integers (car words) #\,))
         (b (parse-integers (caddr words) #\,)))
    (list a b)))

; check if a movement is in a straight line
(defun straight-p (entry)
  ; x1 == x2 || y1 == y2
  (or (eq (caar entry) (caadr entry))
      (eq (cadar entry) (cadadr entry))))

; collect all positions between two points, inclusive
(defun line-positions (a b)
  (let* ((x1 (car a))
         (y1 (cadr a))
         (x2 (car b))
         (y2 (cadr b)))
    (cond ((eq x1 x2) (loop :for x = x1
                            :for y :from (min y1 y2) :to (max y1 y2) 
                            :collect (list x y)))

          ((eq y1 y2) (loop :for x :from (min x1 x2) :to (max x1 x2)
                            :for y = y1
                            :collect (list x y)))

          ; gross hack
          ; to properly iterate diagonals
          ((and (< x1 x2) (< y1 y2))
             (loop :for x :from x1 :to x2
                   :and y :from y1 :to y2
                   :collect (list x y)))

          ((and (< x1 x2) (> y1 y2))
             (loop :for x :from x1 :to x2
                   :and y :from y1 :downto y2
                   :collect (list x y)))

          ((and (> x1 x2) (< y1 y2) )
             (loop :for x :from x1 :downto x2
                   :and y :from y1 :to y2
                   :collect (list x y)))

          (t (loop :for x :from x1 :downto x2
                   :and y :from y1 :downto y2
                   :collect (list x y))))))

; draws a single line from a to b
(defun draw (a b state)
  (loop :for pos :in (line-positions a b)
        :for (x y) = pos
        :do (let ((current (aref state (+ x (* *side-length* y)))))
              (setf (aref state (+ x (* *side-length* y))) (+ 1 current)))))

; modifies state in-place and returns a copy of the new state
(defun run-input (input state)
  (loop :for line :in input
        :for (a b) = line
        :do (draw a b state)

        ; subseq creates copy of array, ala copy-seq
        :finally (return (subseq state 0))))

(defun answer (&optional (file #P"input.txt"))
  (let* ((input (mapcar #'process-line (get-input file)))
         (lines (loop :for line :in input
                      :if (straight-p line) :collect line :into straight
                      :else :collect line :into diagonal
                      :finally (return (list straight diagonal))))
        (state (make-array (list (* *side-length* *side-length*)) :initial-element 0))
        (part1 (run-input (car lines) state))
        (part2 (run-input (cadr lines) state)))
    (format t "Part 1: ~d~%" (count 2 part1 :test-not #'>))
    (format t "Part 2: ~d~%" (count 2 part2 :test-not #'>))))

(answer)
