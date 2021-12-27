#!/usr/bin/env -S sbcl --script

(defparameter *usage* "Usage: ./aoc [run|create] <day-specifier> ...")

(defun eprint (msg)
    (loop :for line :in (if (listp msg) msg (list msg))
          :do (format *error-output* "[ERROR] ~a~%" line)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                        (user-homedir-pathname))))
    (cond
      ((probe-file quicklisp-init)
       (load quicklisp-init))
      (t
       (eprint "Quicklisp not located at ~/quicklisp")
       (exit)))))

(ql:quickload '("with-user-abort" "unix-opts") :silent t)

(defparameter *days* nil)

(defparameter *exec* nil)

(opts:define-opts
  (:name :help
         :description "Print a brief help summary (this message)."
         :short #\h
         :long "help")
  (:name :interpreter
         :description "Select the Lisp interpreter to use."
         :short #\i
         :long "interp"
         :default "sbcl"
         :arg-parser #'string
         :meta-var "INTERPRETER"))

(defun run (days)
  (cond
    ((null days)
     (eprint "No days specified to 'run'!")
     (progn
       (opts:describe
        :prefix *usage*
        :args "[keywords]")
       (exit)))
    (t
     (loop :for day :in days
           :do (uiop:chdir day)
               (format t "-- ~a --~%" day)
               (uiop:run-program
                (append *exec* '("run.lsp"))
                :output *standard-output*)
               (write-char #\Newline *standard-output*)
               (uiop:chdir "..")))))

(defun create-days (days)
  (cond
    ((null days)
     (eprint "No days specified to 'create'!")
     (progn
       (opts:describe
        :prefix *usage*
        :args "[keywords]")
       (exit)))
    (t (loop :for day :in days
             :do (cond
                   ((probe-file day)
                    (eprint (concatenate 'string day " already exists!")))
                   (t
                    (format t "[INFO] Creating skeleton for '~a'~%" day)
                    (ensure-directories-exist (concatenate 'string day "/"))
                    (uiop:run-program
                     (list "cp" "../include/lisp/template.lsp"
                           (concatenate 'string day "/run.lsp")))))))))

(defun resolve-day (day)
  "Convert a user-entered day to a long form day specifier"
  (assert (> (length day) 0))
  (let ((short-form (every #'digit-char-p day)))
    (cond
      ((null short-form) day)
      ((= (length day) 1) (concatenate 'string "day0" day))
      (t (concatenate 'string "day" day)))))

(defun find-days ()
  ;; find . -type d -name "day*" | sed "s/^\\.\\///"
  (let ((days (uiop:run-program (list "sed" "s/^\\.\\///")
                                :output '(:string :stripped t)
                                :input
                                (uiop:process-info-output
                                 (uiop:launch-program
                                  (list "find" "." "-type" "d" "-name" "day*")
                                  :output :stream)))))
    (sort
      (read-from-string
       (with-output-to-string (out)
         (write-string "(\"" out)
         (loop :for char :across days
               :do (if (char= char #\Newline)
                       (write-string "\" \"" out)
                       (write-char char out)))
         (write-string "\")" out)))
      #'string<=)))

(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
    (with-user-abort:user-abort () (sb-ext:exit :code 130))))

(defun toplevel ()
  (sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (multiple-value-bind (opts args) (opts:get-opts)
      (if (getf opts :help)
          (progn
            (opts:describe
             :prefix *usage*
             :args "[keywords]")
            (exit)))

      (let ((interp (string-downcase (getf opts :interpreter))))
        (cond
          ((string= interp "sbcl")
           (setf *exec* '("sbcl" "--script")))
          ((string= interp "clisp")
           (setf *exec* '("clisp")))
          ((string= interp "gcl")
           (setf *exec* '("gcl" "-batch" "-f")))
          (t (eprint "Invalid interpreter specified!"))))

      (setf *days* (find-days))

      (cond
        ((null args)
         (eprint "No subcommand passed!")
         (progn
           (opts:describe
            :prefix *usage*
            :args "[keywords]"))
         (exit))
        ((string= (car args) "run")
         (run (if (position "all" args :test #'string=)
              *days*
              (mapcar #'resolve-day (cdr args)))))
        ((string= (car args) "create")
         (create-days (mapcar #'resolve-day (cdr args))))))))

(toplevel)
