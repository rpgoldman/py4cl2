;;; Code to read from python process over a stream

(in-package :py4cl)

(defstruct python-object
  "A handle for a python object
which couldn't be translated into a Lisp value.
TYPE slot is the python type string
HANDLE slot is a unique key used to refer to a value in python."
  (type "" :type string)
  handle)

(defmethod print-object ((obj python-object) stream)
  (let ((name-sym (intern (python-object-type-name obj) :python-names)))
    (print-python-object name-sym obj stream)))

(defmethod python-getattr ((obj python-object) (attr-name string))
  (python-eval (pythonize obj) "." attr-name))

(defgeneric python-object-type-name (obj)
  (:documentation
   "Returns a string value for the type name of the python object, stripping out
boilerplate from the class meta-objects.")
  (:method ((obj python-object))
    (multiple-value-bind (match-p match-array) 
        (ppcre:scan-to-strings "^<class ['\"](.*)['\"]>$" (python-object-type obj))
      (if match-p
          (aref match-array 0)
          (python-object-type obj)))))

(defgeneric print-python-object (python-type obj stream)
  (:documentation
   "Function for printing a PYTHON-OBJECT based on the type stored in it.

Methods should be written to key off the name of the python type, which should be
a symbol that is interned in the PYTHON-NAMES package.  These methods should use
PRINT-UNREADABLE-OBJECT, since PYTHON-OBJECTS cannot be printed readably.")
  (:method :around ((python-type string) (obj python-object) stream)
    (call-next-method (intern python-type :python-names) obj stream))
  ;; default method
  (:method (python-type (obj python-object) stream)
    (declare (ignorable python-type))
    (print-unreadable-object (obj stream)
      (format stream "PYTHON-OBJECT ~a HANDLE ~d" (python-object-type obj) (python-object-handle obj)))))

(defvar *freed-python-objects* nil
  "A list of handles to be freed. This is used because garbage collection may occur in parallel with the main thread.")

(defun free-python-object (python-id handle)
  (push (list python-id handle) *freed-python-objects*))

(defun delete-freed-python-objects ()
  ;; Remove (python-id handle) pairs from the list and process
  (loop for id-handle = (pop *freed-python-objects*)
     while id-handle
     do (let ((python-id (first id-handle))
              (handle (second id-handle)))
          (if (and
               (python-alive-p) ; If not alive, python-exec will start python
               (= *current-python-process-id* python-id))  ; Python might have restarted
              ;; Call the internal function, to avoid infinite recursion or deadlock
              (python-eval* #\x "
try:
  del _py4cl_objects[" handle "]
except:
  pass"))))
  (delete-numpy-pickle-arrays))

(defun delete-numpy-pickle-arrays ()
  "Delete pickled arrays, to free space."
  (loop :while (> *numpy-pickle-index* 0)
        :do (decf *numpy-pickle-index*)
            (uiop:delete-file-if-exists
             (concatenate 'string
                          (config-var 'numpy-pickle-location)
                          ".to." (write-to-string *numpy-pickle-index*)))))

(defun make-python-object-finalize (&key (type "") handle)
    "Make a PYTHON-OBJECT struct with a finalizer.
This deletes the object from the dict store in python.

Uses trivial-garbage (public domain)
"
    (tg:finalize
     (make-python-object :type type
                         :handle handle)
     (let ((python-id *current-python-process-id*))
       (lambda () ; This function is called when the python-object is garbage collected
         (ignore-errors
           ;; Put on a list to free later. Garbage collection may happen
           ;; in parallel with the main thread, which may be executing other commands.
           (free-python-object python-id handle))))))

(defun stream-read-string (stream)
  "Reads a string from a stream
Expects a line containing the number of chars following
e.g. '5~%hello'
Returns the string or nil on error
"
  (let ((nchars (parse-integer (read-line stream))))
    (with-output-to-string (str)
      (loop for i from 1 to nchars do
           (write-char (read-char stream) str)))))

(defun stream-read-value (stream)
  "Get a value from a stream
Currently works by reading a string then using read-from-string
"
  (let ((str (stream-read-string stream)))
    (multiple-value-bind (value count)
        (read-from-string str)
      ;; Check if all characters were used
      (unless (eql count (length str))
        (error (concatenate 'string "unread characters in reading string \"" str "\""))) 
      value)))
