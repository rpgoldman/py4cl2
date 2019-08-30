;; This file is divided into:
;; - Preparations for calling
;;   - dispatch-reply: Used for calling and passing lisp functions to python
;;     Note that this calling can be nested: probably, "return_values"
;;     py4cl.py file keeps track of this nesting (see LispCallbackObject class
;;     in python
;; - Raw Functions
;; - Utility Functions
;;   - eval, exec, call, method, async, monitor
;;   - chain
;;   - remote objects

(in-package :py4cl)

;; ============================ PREPARATIONS FOR CALLING ======================

(define-condition pyerror (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "Python error: ~a" (text condition)))))

(defun dispatch-reply (stream value)
  (write-char #\r stream)
  (stream-write-value value stream)
  (force-output stream))

(defun dispatch-messages (process)
  "Read response from python, loop to handle any callbacks"
  (setq *python-process-busy-p* t)
  (let* ((read-stream (uiop:process-info-output process))
         (write-stream (uiop:process-info-input process))
         (return-value
          (loop
             (case (read-char read-stream) ; First character is type of message
               (#\r (return (stream-read-value read-stream)))  ; Returned value

               (#\e (error 'pyerror  
                           :text (stream-read-string read-stream)))

               ;; Delete object. This is called when an UnknownLispObject is deleted
               (#\d (free-handle (stream-read-value read-stream)))

               ;; Slot access
               (#\s (destructuring-bind (handle slot-name) (stream-read-value read-stream)
                      (let ((object (lisp-object handle)))
                        ;; User must register a function to handle slot access
                        (dispatch-reply
                         write-stream
                         (restart-case
                             (python-getattr object slot-name)
                           ;; Provide some restarts for missing handler or missing slot
                           (return-nil () nil)
                           (return-zero () 0)
                           (enter-value (return-value)
                             :report "Provide a value to return"
                             :interactive (lambda ()
                                            (format t "Enter a value to return: ")
                                            (list (read)))
                             return-value))))))
               
               (#\c ;; Callback. Return a list, containing function ID, then the args
                (let ((call-value (stream-read-value read-stream)))
                  (let ((return-value (apply (lisp-object (first call-value))
                                             (second call-value))))
                    (dispatch-reply write-stream return-value))))
               (#\p ; Print stdout
                (let ((print-string (stream-read-value read-stream)))
                  (princ print-string)))
               
               (otherwise (error "Unhandled message type"))))))
    (setq *python-process-busy-p* nil)
    return-value))


;; ============================== RAW FUNCTIONS ================================
(declaim (ftype (function (character &rest string)) raw-py))
(defun raw-py (cmd-char &rest strings)
  "Intended as an abstraction to RAW-PYEVAL and RAW_PYEXEC.
Passes strings as they are, without any 'pythonize'ation."
  (python-start-if-not-alive)
  (let ((stream (uiop:process-info-input *python*))
        (str (apply #'concatenate 'string strings)))
    (write-char cmd-char stream)
    (stream-write-string str stream)
    (force-output stream)
    (dispatch-messages *python*))) ; wait for python

(declaim (ftype (function (&rest string)) raw-pyeval))
(defun raw-pyeval (&rest strings)
  "Calls python eval on the concatenation of strings, as they are, without any 
pythonization or modification."
  (apply #'raw-py #\e strings))

(declaim (ftype (function (&rest string)) raw-pyexec))
(defun raw-pyexec (&rest strings)
  "Calls python exec on the concatenation of strings, as they are, without any 
pythonization or modification.
NOTE: Like usual, there are peculiarities to exec commands.
For instance,
  import sys
  def foo:
    sys.stdout.write('hello')
  foo()
will result in 'sys' name not defined PYERROR."
  (apply #'raw-py #\x strings))

(defun (setf raw-pyeval) (value &rest args)
  (apply #'raw-pyexec (append args
                              (list "=" (pythonize value))))
  value)

;; =========================== UTILITY FUNCTIONS ===============================

(defun pythonizep (value)
  "Determines if VALUE should be pythonized."
  (or (not (stringp value)) ; do not pythonize if
      (realp (ignore-errors (parse-number:parse-number value)))))

(defun pythonize-if-needed (value)
  (if (pythonizep value) (pythonize value) value))

(defun pyeval (&rest args)
  "Calls python eval on args; PYTHONIZEs arg if it satisfies PYTHONIZEP.
Eg.
  > (let ((a 5)) (pyeval a \"*\" a)) 
  25"
  (delete-freed-python-objects) ; delete before pythonizing
  (delete-numpy-pickle-arrays)
  (apply #'raw-pyeval (mapcar #'pythonize-if-needed args)))

(defun pyexec (&rest args)
  "Calls python exec on args; PYTHONIZEs arg if it satisfies PYTHONIZEP."
  (delete-freed-python-objects) ; delete before pythonizing
  (delete-numpy-pickle-arrays)
  (apply #'raw-pyexec (mapcar #'pythonize-if-needed args)))

;; One argument for the name (setf pyeval) is that it sets the "place" returned
;; by pyeval.
(defun (setf pyeval) (value &rest args)
  "Set an expression to a value. Just adds \"=\" and the value
to the end of the expression. Note that the result is evaluated
with exec rather than eval.
Example:
    (setf (pyeval \"a\") 2)  ; python \"a=2\"
Can be useful for modifying a value directly in python.
"
  (apply #'pyexec (append args (list "=" value))) ; would nconc be better?
  value)

(defun %pycall-args (&rest args)
  (apply #'concatenate
         'string
         "("
         `(,@(iter (for arg in args)
                   (for pythonized-arg = (pythonize arg))
                   (if (and (symbolp arg)
                            (eq (find-package :keyword)
                                (symbol-package arg)))
                       (collect pythonized-arg)
                       (progn (collect pythonized-arg)
                              (collect ","))))
             ")")))

(defun pycall (fun-name &rest args)
  "Calls FUN-NAME with ARGS as arguments. Arguments can be keyword based, or 
 otherwise."
  (raw-pyeval "("
              (typecase fun-name
                (string fun-name)
                (t (pythonize fun-name)))
              ")"
              (apply #'%pycall-args args)))

(defun pymethod (object method &rest args)
  "PYCALLs METHOD of OBJECT with ARGS
Examples:
  > (pymethod \"'hello {0}'\" 'format \"world\") 
  \"hello world\"
  > (pymethod '(1 2 3) '--len--)
  3
Note: FUN-NAME is NOT PYTHONIZEd if it is a string.
"
  (python-start-if-not-alive)
  (apply #'pycall
         (concatenate 'string
                      (pythonize object) "." (pythonize-name method))
         args))

(defun pygenerator (function stop-value)
  (pycall "_py4cl_generator" function stop-value))

(defun pyslot-value (object slot-name)
  (pyeval object "." (pythonize-name slot-name)))

(defun pyversion-info ()
  "Return a list, using the result of python's sys.version_info."
  (pyexec "import sys")
  (pyeval "tuple(sys.version_info)"))

(defun pyhelp (object)
  (pyeval "help(" object ")"))

;; Chain -----------------------------------------------------------------------

;;; If someone wants to handle multidimensional array slicing and stuff, they should take
;;; a look at: https://github.com/hylang/hy/issues/541
(defun %chain (&rest body)
  (format nil "~{~A~^.~}"
          (mapcar (lambda (elt)
                    (typecase elt
                      (null "")
                      (cons (apply #'concatenate 'string
                                   (case (car elt)
                                     (aref
                                      `(,(%chain (cadr elt))
                                         ,@(mapcar (lambda (idx)
                                                     (format nil "[~A]" idx))
                                                   (mapcar #'%chain (cddr elt)))))
                                     (t `(,(typecase (car elt)
                                             (string (car elt))
                                             (t (pythonize (car elt))))
                                           ,(apply #'%pycall-args (cdr elt)))))))
                      (t (pythonize elt))))
                  body)))

(defmacro chain (&body body)
    "Chain method calls, member access, and indexing operations
on objects.
Keywords inside python function calls are converted to python keywords.

Functions can be specified using a symbol or a string. If a symbol is used
then it is converted to python using STRING-DOWNCASE. 

Examples:

  (chain \"hello {0}\" (format \"world\") (capitalize)) 
     => python: \"hello {0}\".format(\"world\").capitalize()
     => \"Hello world\"

  (chain (range 3) stop) 
     => python: range(3).stop
     => 3

  (chain \"hello\" 4)
     => python: \"hello\"[4]
     => \"o\"
"
    `(raw-pyeval ,(apply #'%chain body)))

(defun chain* (&rest body) (raw-pyeval (apply #'%chain body)))
(defun (setf chain*) (value &rest args)
  (apply #'raw-pyexec (list (apply #'%chain args)
                            "="
                            (pythonize value)))
  value)
