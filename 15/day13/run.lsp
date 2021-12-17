; Day 13: Knights of the Dinner Table

(load "../../include/lisp/common.lsp")

; read each line into a link cost
; of the form:
;
;   ( from.to cost )
;
(defun process-lines (lines)
  (loop :for line :in lines
        :collect (let* ((words (parse-words line))
                        (person-a (nth 0 words))
                        (b-period (car (last  words)))
                        (person-b (subseq b-period 0 (- (length b-period) 1)))
                        (sign (nth 2 words))
                        (magnitude (parse-integer (nth 3 words)))
                        (from (intern person-a))
                        (to   (intern person-b)))
                   (list (cons from to)
                         (if (string= sign "gain")
                             magnitude
                             (* -1 magnitude))))))

; convert individual connection data to a list where
; each item has the form:
;
;   ( from to1 to2 to3 ... toN )
;
; where 'from' is a person and 'to1', 'to2', ..., 'toN'
; are the costs from 'from' to each other person. Each
; cost is of the form:
;
;   ( to . cost)
;
(defun adjacency-lists (edges)
  (remove-duplicates
    (loop :for v :in (mapcar #'caar edges)
          :collect
          (cons v (loop :for (link cost) :in edges
                               :for (from . to) = link
                               :when (eq v from)
                               :collect (cons to cost))))
    :key #'car))

; get the cost for an individual connection from the adjacency list
(defun get-cost (from to link-costs)
  (cdr (assoc to (cdr (assoc from link-costs)))))

(defun happiness (seating link-costs &key (include-ends t))
  (let ((num-people (length seating)))
      (loop :for n :below num-people
            ; seated to the right
            :when (< n (- num-people 1) )
            :sum (get-cost (nth n seating) (nth (+ n 1) seating) link-costs)

            ; seated to the left
            :when (> n 0)
            :sum (get-cost (nth n seating) (nth (- n 1) seating) link-costs)

            ; left end
            :when (and include-ends (eq n 0))
            :sum (get-cost (nth 0 seating) (nth (- num-people 1) seating) link-costs)

            ; right end
            :when (and include-ends (eq n (- num-people 1)))
            :sum (get-cost (nth (- num-people 1) seating) (nth 0 seating) link-costs))))

(defun answer (&optional (file #P"input.txt"))
  (let* ((raw-input (get-input file))
         (link-costs (adjacency-lists (process-lines raw-input)))
         (attendees (loop :for attendee :in (mapcar #'car link-costs) :collect attendee))
         (seatings (permute attendees))
         (seating-costs1 (mapcar (lambda (x) (happiness x link-costs)) seatings))

         ; when not incuding ends, it's like seating yourself between the ends (cost 0).
         ; so, since 'seatings' is every permutation, seating yourself between every end
         ; is the same as the permutations with yourself in between every person.
         (seating-costs2 (mapcar (lambda (x) (happiness x link-costs :include-ends nil)) seatings)))

    (format t "Part 1: ~d~%" (reduce #'max (remove-duplicates seating-costs1)))
    (format t "Part 2: ~d~%" (reduce #'max (remove-duplicates seating-costs2)))))

(answer)
