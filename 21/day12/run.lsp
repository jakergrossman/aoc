; Day 12: Passage Pathing

(load "../../include/lisp/common.lsp")

(defun adjacency-lists (edges)
  (loop for v in (remove-duplicates (mapcar #'car edges) :test #'equal)
        collect (cons v (loop for (u . w) in edges when (eq u v) collect w))))

(defun cave-graph (file)
  (adjacency-lists
   (loop for line in (get-input file)
         for (u v) = (mapcar #'intern (parse-words line #\-))
         collect (cons u v) collect (cons v u))))

(defun small-cave-p (cave) (lower-case-p (char (symbol-name cave) 0)))

(defun count-paths (caves start end &key allow-twice)
  (labels ((discover (start end seen twice)
              (cond ((and (member start seen)
                          (or twice (not allow-twice)
                              (eq start '|start|) (eq start '|end|)))
                      0)
                    ((eq start end) 1)
                    (t (let ((new-seen (if (small-cave-p start) (cons start seen) seen))
                             (new-twice (when allow-twice
                                          (or twice (member start seen)))))
                         (loop for next in (cdr (assoc start caves))
                               sum (discover next end new-seen new-twice)))))))
    (discover start end nil nil)))

(defun answer (&optional (file #P"input.txt"))
  (values (count-paths (cave-graph file) '|start| '|end| :allow-twice nil)
          (count-paths (cave-graph file) '|start| '|end| :allow-twice t)))

(multiple-value-bind (p1 p2) (answer)
  (format t "Part 1: ~d~%" p1)
  (format t "Part 2: ~d~%" p2))
