; Day 21: Dirac Dice

(load "../../include/lisp/common.lsp")

(defun turn (n)
  "Calculates the sum of the n'th turn's rolls"
  (let ((base (1+ (* (1- n) 3))))
    (loop :for n :from base :to (+ 2 base)
          :sum (1+ (mod (1- n) 100)))))

(defun move (position score roll-number)
  (let* ((distance (turn roll-number))
         (new-pos (+ position distance))
         (wrapped-pos (1+ (mod (1- new-pos) 10))))
    (list wrapped-pos (+ score wrapped-pos))))

(defun game1 (p1-start p2-start)
  (let ((p1-state (list p1-start 0))
        (p2-state (list p2-start 0)))
    (loop :for turn :from 1
          :for roll :from 0 :by 3
          :for (p1-pos p1-score) = p1-state
          :for (p2-pos p2-score) = p2-state
        :if (zerop (mod turn 2))
          :do (setf p2-state (move p2-pos p2-score turn))
        :else
          :do (setf p1-state (move p1-pos p1-score turn))

        :when (or (>= p1-score 1000)
                  (>= p2-score 1000))
          :do (return-from game1 (* (min p1-score p2-score) roll)))))

;; Number of ways to roll each number with a Dirac dice
(defparameter *probability* '((3 . 1)
                              (4 . 3)
                              (5 . 6)
                              (6 . 7)
                              (7 . 6)
                              (8 . 3)
                              (9 . 1)))

(defun possible-states (current step)
  "Generate a list of all possible next states with their frequencies"
  (let ((pos1 (nth 0 current))
        (pos2 (nth 1 current))
        (score1 (nth 2 current))
        (score2 (nth 3 current)))
  (cond
    ((= (mod step 2) 1)
     ;; player 1
     (loop :for (roll . freq) :in *probability*
           :for new-pos = (+ pos1 roll)
           :for wrapped = (1+ (mod (1- new-pos) 10))
           :for new-score = (+ score1 wrapped)
           :collect (list (list wrapped pos2 new-score score2) freq)))
    ((= (mod step 2) 0)
     ;; player 2
     (loop :for (roll . freq) :in *probability*
           :for new-pos = (+ pos2 roll)
           :for wrapped = (1+ (mod (1- new-pos) 10))
           :for new-score = (+ score2 wrapped)
           :collect (list (list pos1 wrapped score1 new-score) freq))))))

(defun game2 (p1-start p2-start)
  (let ((p1 0)
        (p2 0)
        (gamestates (make-hash-table :test #'equal)))
    (setf (gethash (list p1-start p2-start 0 0) gamestates) 1)
    (loop :for step = 1 :then (1+ step)
          :while (> (hash-table-count gamestates) 0)
          :do (let ((new-gamestates (make-hash-table :test #'equal)))
                (loop :for state :being :each hash-key :of gamestates
                      :for occurrences = (gethash state gamestates)
                      :for new-states = (possible-states state step)
                      :do (loop :for (new-state freq) :in new-states
                                :do (cond
                                      ((>= (nth 2 new-state) 21)
                                       ;; player 1 wins
                                       (setf p1 (+ p1 (* occurrences freq))))
                                      ((>= (nth 3 new-state) 21)
                                       ;; player 2 wins
                                       (setf p2 (+ p2 (* occurrences freq))))
                                      (t
                                       ;; game is still in play
                                       ;; check if state already in new-states, more
                                       ;; than 1 way to get to a state
                                       (let* ((stored (gethash new-state new-gamestates))
                                              (previous-freq (if stored stored 0)))
                                            (setf (gethash new-state new-gamestates)
                                                  (+ previous-freq (* occurrences freq))))))))

                (setf gamestates new-gamestates)))
    (max p1 p2)))

(defun read-start-pos (line)
  (let ((words (parse-words line)))
    (parse-integer (car (last words)))))

(defun answer (&optional (file #P"input.txt"))
  (let* ((input (get-lines file))
         (start1 (read-start-pos (car input)))
         (start2 (read-start-pos (cadr input)))
         (part1 (game1 start1 start2))
         (part2 (game2 start1 start2)))
    (format t "Part 1: ~d~%" part1)
    (format t "Part 2: ~d~%" part2)))

(answer)
