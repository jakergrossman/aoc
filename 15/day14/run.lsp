; Day 14: Reindeer Olympics

(load "../../include/lisp/common.lsp")

(defun run (seconds reindeer)
  (let* ((num-deer (length reindeer))
         (speeds (mapcar #'car reindeer))
         (run-times (mapcar #'cadr reindeer))
         (stop-times (mapcar #'caddr reindeer))

         (distances (make-array (list num-deer) :initial-element 0))
         (scores (make-array (list num-deer) :initial-element 0))
         (run-times-left (make-array (list num-deer) :initial-contents run-times))
         (stop-times-left (make-array (list num-deer) :initial-element -1)))
    (dotimes (i seconds)
      ; update distances
      (loop :for n :below num-deer :do
        ; decrement running timer
        (cond
          ((> (aref stop-times-left n) 0)
            (setf (aref stop-times-left n) (- (aref stop-times-left n) 1)))

          ((> (aref run-times-left n) 0)
            (setf (aref distances n) (+ (aref distances n) (nth n speeds)))
            (setf (aref run-times-left n) (- (aref run-times-left n) 1))))

        ; start/stop newly expired timer
        (cond
          ; start deer
          ((and (eq (aref stop-times-left n) 0)
                (eq (aref run-times-left n) -1))
            (setf (aref stop-times-left n) -1)
            (setf (aref run-times-left n) (nth n run-times)))

          ; stop deer
          ((and (eq (aref run-times-left n) 0)
                (eq (aref stop-times-left n) -1))
            (setf (aref run-times-left n) -1)
            (setf (aref stop-times-left n) (nth n stop-times))))) ; end inner loop

      ; update scores
      (let* ((max-distance (reduce #'max distances)))
        (loop :for n :below num-deer
          :do (cond ((eq (aref distances n) max-distance)
                (setf (aref scores n) (+ 1 (aref scores n))))))))
    (list distances scores)))

(defun parse-line (line)
  (let* ((words (parse-words line))
         (speed (parse-integer (nth 3 words)))
         (run-time (parse-integer (nth 6 words)))
         (stop-time (parse-integer (nth (- (length words) 2) words))))
    (list speed run-time stop-time)))

(defun answer (&optional (file #P"input.txt") (seconds 2503))
  (let* ((reindeer (mapcar #'parse-line (get-lines file)))
         (result (run seconds reindeer)))
    (format t "Part 1: ~d~%" (reduce #'max (car result)))
    (format t "Part 2: ~d~%" (reduce #'max (cadr result)))))

(answer)
