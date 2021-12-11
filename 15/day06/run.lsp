; Day 6: Probably a Fire Hazard

(load "../../include/common.lsp")

(defun process-line (line)
  (let* ((words (parse-words line))
         (first-word (car words))
         (rev-words (reverse words))
         (start-pos (parse-integers (nth 2 rev-words) #\,))
         (end-pos (parse-integers (nth 0 rev-words) #\,)))
    (cond
      ((string= "toggle" first-word) (list "toggle" start-pos end-pos))
      (t (list (cadr words) start-pos end-pos)))))

(setq input
  (get-input "input.txt" :process #'process-line :predicate #'string-empty-p))

; (setq lights1
;   (make-array (list 1000 1000) :initial-element nil))

(setq lights1
  (make-array (list (* 1000 1000)) :initial-element nil))

(setq lights2
  (make-array (list 1000 1000) :initial-element 0))

(defun draw1 (command start end)
  (let* ((x1 (car start))
        (y1 (cadr start))
        (x2 (car end))
        (y2 (cadr end))
        (left (min x1 x2))
        (right (max x1 x2))
        (top (min y1 y2))
        (bottom (max y1 y2))
        (height (- bottom top))
        (width (- right left)))
    (loop :for n :below height
      do
      (cond
        ((string= "on" command) (fill lights1 t :start (+ (* 1000 n) left) :end (+ (* 1000 n) right 1)))
        ((string= "off" command) (fill lights1 nil :start (+ (* 1000 n) left) :end (+ (* 1000 n) right 1)))
        (t (loop :for m :from 0 :to width do (setf (aref lights1 (+ (* 1000 n) m)) (not (aref lights1 (+ (* 1000 n) m))))))))))

(defun draw2 (command start end)
  (let* ((x1 (car start))
        (y1 (cadr start))
        (x2 (car end))
        (y2 (cadr end))
        (left (min x1 x2))
        (right (max x1 x2))
        (top (min y1 y2))
        (bottom (max y1 y2)))
    (loop :for x :from left :to right
      do (loop :for y :from top :to bottom
        do
        (cond
          ((string= "on" command)
            (setf (aref lights2 y x) (+ 1 (aref lights2 y x))))
          ((string= "off" command)
            (setf (aref lights2 y x) (max 0 (- (aref lights2 y x) 1))))
          (t
            (setf (aref lights2 y x) (+ 2 (aref lights2 y x)))))))))


(loop :for move :in input do (apply #'draw1 move))
(loop :for move :in input do (apply #'draw2 move))

(setq part1
  (length (remove nil lights1)))

(setq part2
  (reduce '+
    (reduce #'append
      (loop :for n :below 1000
        :collect
        (loop :for m :below 1000 :unless (eq 0 (aref lights2 n m)) :collect (aref lights2 n m))))))

(format t "Part 1: ~d~%" part1)
(format t "Part 2: ~d~%" part2)
