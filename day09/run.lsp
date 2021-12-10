; Day 9: Smoke Basin

(load "../common.lsp")

(setq raw-input
  (get-input "input.txt" :predicate #'string-empty-p))

(setq width (length (car raw-input)))
(setq height (length raw-input))

(setq input
  (make-array
    (list (* (length (car raw-input)) (length raw-input)))
    :initial-contents
    (map 'list #'digit-char-p
      (reduce 'append
        (map 'list (lambda (x) (coerce x 'list)) raw-input)))))

; convert linear index to x-y coordinates
(defun index-xy (index)
  (multiple-value-bind (y x) (floor index width) (list x y)))

; Checks whether the point at the specified index
; is the lowest of it's neighbors
;
;                        .-> x
; Coordinate Directions: |
;                        V y
(defun lowest (index)
  (let ((x (car (index-xy index)))
        (y (cadr (index-xy index)))
        (target (aref input index)))
    (cond
      ((and (> x 0) (>= target (aref input (- index 1)))) nil)
      ((and (< x (- width 1)) (>= target (aref input (+ index 1)))) nil)
      ((and (> y 0) (>= target (aref input (- index width)))) nil)
      ((and (< y (- height 1)) (>= target (aref input (+ index width)))) nil)
      (t t))))

(setq low-points
  (loop :for n :below (length input) :unless (not (lowest n)) :collect n))

; valid neighbors are neighbors that:
;   - are bigger then the current node (would flow into current node)
;   - not 9 (not part of any basins)
(defun valid-neighbor (lo-pos hi-pos)
  (let ((lo (aref input lo-pos))
        (hi (aref input hi-pos)))
    (cond
      ((eq hi 9) nil)
      ((< lo hi) t))))

; discover next set of valid neighbors
(defun more-neighbors (index)
  (let ((x (car (index-xy index)))
        (y (cadr (index-xy index)))
        (target (aref input index)))
    (append 
      (cond
        ((and (> x 0) (valid-neighbor index (- index 1))) (list (- index 1)))
        (t nil))
      (cond
        ((and (< x (- width 1)) (valid-neighbor index (+ index 1))) (list (+ index 1)))
        (t nil))
      (cond
        ((and (> y 0) (valid-neighbor index (- index width))) (list (- index width)))
        (t nil))
      (cond
        ((and (< y (- height 1)) (valid-neighbor index (+ index width))) (list (+ index width)))
        (t nil)))))

; DFS neighbors of index until no more
; valid neighbors are found
;
; visited is technically re-used, but because
; the problem states that "all other locations will
; always be part of one basin" (i.e., disjoint sets)
; it can used as the visited array for *all* basins
(setq visited (make-array (list (length input)) :initial-element nil))
(defun basin-size (index)
  (cond
    ((aref visited index) 0)
    (t
      (setf (aref visited index) t)
      (let ((new-neighbors (more-neighbors index)))
        (cond
          ((null new-neighbors) 1)
          (t
            (+ 1
              (reduce '+ (map 'list #'basin-size new-neighbors)))))))))

; find a basin for each low point
(setq basins (sort (map 'list #'basin-size low-points) #'>))

(format t "Part 1: ~d~%"
  (reduce
    (lambda (x y)
      (+ x (aref input y) 1))
    low-points
    :initial-value 0))

(format t "Part 2: ~d~%"
  (apply #'* (subseq basins 0 3)))
