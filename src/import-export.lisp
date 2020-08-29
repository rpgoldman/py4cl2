;; Functions and macros for importing and exporting symbols to python

;;;; Things we need to achieve - in case someone wants to attempt refactorisation
;;; For defpyfun:
;;;   - For convenience, we need to be able to show the function's arguments and
;;;   default values in Slime.
;;;   - For customizability, we ought to be able to load some "config" file
;;;   containing name, signature, documentation, call method for some functions.
;;;   This latter hasn't been attempted yet.

(declaim (optimize (debug 3)))
(in-package :py4cl2)

(defun numeric-char-p (ch) (find ch "0123456789"))

(defun split-point-p (pch ch)
  (or (and (upper-case-p ch) (lower-case-p pch))
      (and (numeric-char-p ch) (alpha-char-p pch))
      (char= pch #\_)))

(defun collect-first-word (char-list) ; can this be simplified?
  "Returns ((h e l l o) W o r l d), given (h e l l o W o r l d)."
  (iter (for ch-list initially char-list
             then (cdr ch-list))
        (while ch-list) 
        (for ch = (first ch-list))
        (for pch previous ch)
        (for word initially () 
             then (cons ch word))
        (unless (first-iteration-p) (until (split-point-p pch ch)))
        (finally (return (if ch-list
                             (cons (nreverse word) ch-list) 
                             (list char-list))))))

(defun break-into-words (char-list)
  "Returns ((h e l l o) (W o r l d)), given (h e l l o W o r l d)."
  (when char-list
    (destructuring-bind (word . rem-chars) (collect-first-word char-list)
      (cons word (break-into-words rem-chars)))))

(declaim (ftype (function (string) string) lispify-name))
(defun lispify-name (name)
  "Converts NAME to a lisp-like name. Specifically:
  1. Replaces underscores with hyphens.
  2. CamelCase is converted to CAMEL-CASE"
  (let* ((words (mapcar (lambda (word)
                          (coerce word 'string))
                        (remove-if #'null
                                   (break-into-words (coerce name 'list)))))
         (prefinal-string (string-upcase (format nil "~{~A~^-~}" words))))
    (remove-if (lambda (ch)
                 (char= ch #\_))
               prefinal-string
               :end (1- (length prefinal-string)))))

(defun get-unique-symbol (symbol-name package-name)
  (multiple-value-bind (symbol location)
      (intern symbol-name package-name)
    (declare (ignore symbol))
    (if location
        (concatenate 'string
                     symbol-name "/1")
        symbol-name)))

(defun pythonize-kwargs (arg-plist)
  (nconc (iter (generate elt in arg-plist)
               (collect (pythonize (next elt)))
               (collect (pythonize (next elt)))
               (collect ","))
         '(")")))

;;; If anyone wants to generate something so as to allow this, feel free to :)
;;; (pyfun 1 2 3 :a 'a :b 'b)
;;; args '(1 2 3)
;;; kwargs '(:a a :b b)

;; https://stackoverflow.com/questions/2677185/how-can-i-read-a-functions-signature-including-default-argument-values
(defun get-arg-list (fullname lisp-package)
  "Returns a list of two lists: PARAMETER-LIST and PASS_LIST"
  (if (string= "<class 'numpy.ufunc'>" (pyeval "str(type(" fullname "))"))
      (let* ((n (pyeval fullname ".nin"))
             (arg-list-without-keys
              (iter (for ch in-string "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                    (for i below n)
                    (collect (intern (string ch) lisp-package)))))
        `(,(append arg-list-without-keys
                   '(&rest keys &key out (where t) &allow-other-keys))
           ((declare (ignore out where))
            (apply #'pycall ,fullname ,@arg-list-without-keys keys))))
      (let* ((return-with-default-return nil)
             (signature (handler-case (pyeval "inspect.signature(" fullname ")")
                          (pyerror (e)
                            (when (search "no signature found"
                                          (slot-value e 'text))
                              (setq return-with-default-return t)
                              nil))
                          (t nil)))
             ;; errors could be value error or type error
             (pos-only (find #\/ (pyeval "str(" signature ")")))
             ;; we are ignoring futther keyword args
             (sig-dict (if signature
                           (pyeval "dict(" signature ".parameters)")
                           (make-hash-table)))
             (default-return (list '(&rest args) ; see the case for allow-other-keys
                                   `(() (apply #'pycall ,fullname args))))
             (allow-other-keys nil)
             other-kwarg-symbol)
        ;; below, pass-list is the argument list passed to the raw-pyexec,
        ;; or underlying function
        ;; parameter-list is the argument list corresponding to defun-visible-to-the-user

        ;; handling the general case of *args and **kwargs is a bit too hard,
        ;; particularly that lisp lambda-lists do not have one-to-one mapping with them
        
        (iter (initially (when return-with-default-return
                           (return-from get-arg-list default-return)))
              (for val in (alexandria:hash-table-values sig-dict))
              (for name = (pyeval val ".name")) ; this will not contain * or **
              (for default = (pyeval val ".default"))
              (for name-str = (pyeval "str(" val ")"))
              (when (or (typep default 'python-object) ; handle would likely be lost
                        (and (search "*" name-str)
                             (not (search "**" name-str))))           
                (return-from get-arg-list default-return)) ; and be unreliable
              (for arg-symbol = (intern (lispify-name name) lisp-package))
              (setq arg-symbol
                    (cond ((eq arg-symbol t)
                           (intern (concatenate 'string "." (lispify-name name))
                                   lisp-package))
                          ((eq arg-symbol nil)
                           (intern (concatenate 'string "." (lispify-name name))
                                   lisp-package))
                          (t arg-symbol)))
              (when (search "**" name-str)
                (setq allow-other-keys t)
                (setq other-kwarg-symbol arg-symbol)
                (next-iteration))
              (for arg-default = (if (or (symbolp default) (listp default))
                                     `',default
                                     default))
              (for parameter-elt = (list arg-symbol arg-default))
              (for pass-elt = (if pos-only
                                  `((pythonize ,arg-symbol) ",")
                                  `(,name "=" (pythonize ,arg-symbol) ",")))
              (collect parameter-elt into parameter-list)
              (collect arg-symbol into arg-symbols)
              (appending pass-elt into pass-list)
              (finally
               (return (cond (pos-only
                              `((&optional ,@parameter-list)
                                (() (raw-pyeval ,fullname "(" ,@pass-list ")"))))
                             (allow-other-keys
                              `((&rest ,other-kwarg-symbol
                                       &key ,@parameter-list &allow-other-keys)
                                (() (apply #'raw-pyeval ,fullname "("
                                           ,@pass-list
                                           (pythonize-kwargs
                                            (progn
                                              (mapc (lambda (symbol)
                                                      (remf ,other-kwarg-symbol
                                                            (find-symbol (symbol-name symbol)
                                                                         :keyword)))
                                                    ',arg-symbols)
                                              ,other-kwarg-symbol))))))
                             ((null parameter-list)
                              `(() (() (raw-pyeval ,fullname "(" ")"))))
                             (t `((&key ,@parameter-list)
                                  (() (raw-pyeval ,fullname "(" ,@pass-list ")")))))))))))


(defun pymethod-list (python-object &key (as-vector nil))
  (pyexec "import inspect")
  (let ((method-vector (pyeval "[name for name, ele in inspect.getmembers("
                               python-object ", callable)]")))
    (if as-vector method-vector (coerce method-vector 'list))))

(defun pyslot-list (python-object &key (as-vector nil))
  (pyexec "import inspect")
  (pyexec "
def _py4cl_non_callable(ele):
  import inspect
  return not(inspect.isroutine(ele))")
  (let ((slot-vector
         (pyeval "[name for name, ele in inspect.getmembers("
                 python-object
                 ", _py4cl_non_callable)]")))
    (if as-vector slot-vector (coerce slot-vector 'list))))

(defun builtin-p (pymodule-name)
  "Some builtin functions like 'sum' do not take keyword args."
  (or (null pymodule-name)
      (string= "" pymodule-name)))

(defun fun-symbol (pyfun-name pyfullname lisp-package &optional (ensure-unique t))
  (if ensure-unique
      (let ((callable-type (cond ((pyeval "inspect.isfunction(" pyfullname ")") 'function)
                                 ((pyeval "inspect.isclass(" pyfullname ")") 'class)
                                 (t t)))
            (lisp-fun-name (lispify-name pyfun-name)))
        (intern (case callable-type
                  (class (concatenate 'string lisp-fun-name "/CLASS"))
                  (function (if (upper-case-p (char pyfun-name 0))
                                (concatenate 'string lisp-fun-name "/1")
                                lisp-fun-name))
                  (t (get-unique-symbol lisp-fun-name lisp-package)))
                lisp-package))
      ;; later, specialize further if needed
      (intern (lispify-name pyfun-name) lisp-package)))

;; In essence, this macro should give the full power of the
;;   "from modulename import function as func"
;; to the user.

;; "from keras.layers import Input" creates only "Input" and not
;; "keras.layers.Input" in python;
;; However, this leaves open the chance of a name conflict
;; - what if two python modules have the same name?
;; defpymodule takes care of this, along with keeping minimal work
;; in defpyfun

(defmacro defvar-doc (name doc)
  `(progn
     (defvar ,name)
     (setf (documentation ',name 'variable) ,doc)))

(defvar *called-from-defpymodule* nil
  "Internal variable used for coordinating between DEFPYMODULE and DEFPYFUN.")
(defvar *defpymodule-silent-p* nil
  "DEFPYMODULE avoids printing progress if this is T.")
(defvar-doc *function-reload-string*
    "String pyexec-ed at the start of a DEFPYFUN when SAFETY is T.")
(defvar-doc *lisp-package-supplied-p*
    "Internal variable used by PYMODULE-IMPORT-STRING to determine the import string.")

(defmacro defpyfun (fun-name &optional pymodule-name
                    &key
                      (as fun-name)
                      (lisp-fun-name (lispify-name as))
                      (lisp-package *package*)
                      (safety t))
  "Defines a function which calls python
Example
  (py4cl:pyexec \"import math\")
  (py4cl:defpyfun \"math.sqrt\")
  (math.sqrt 42) -> 6.4807405

Arguments:
  FUN-NAME: name of the function in python, before import
  PYMODULE-NAME: name of the module containing FUN-NAME
  AS: name of the function in python, after import
  LISP-FUN-NAME: name of the lisp symbol to which the function is bound*
  LISP-PACKAGE: package (not its name) in which LISP-FUN-NAME will be interned
  SAFETY: if T, adds an additional line in the function asking to import the 
    package or function, so that the function works even after PYSTOP is called.
    However, this increases the overhead of stream communication, and therefore,
    can reduce speed."
  (check-type fun-name string)
  (check-type lisp-fun-name string)
  (check-type lisp-package package)
  ;; (check-type pymodule-name string) ;; (or nil string)
  ;; (check-type as string) ;; (or nil string)?
  (python-start-if-not-alive)
  (pyexec "import inspect")
  ;; (format t "*called-from-defpymodule* ~D in ~D~%"
  ;;         *called-from-defpymodule*
  ;;         fun-name)
  (unless (or *called-from-defpymodule*
              (builtin-p pymodule-name))
    (pyexec (function-reload-string :pymodule-name pymodule-name
                                    :fun-name fun-name
                                    :as as)))
  (let* ((fullname (if *called-from-defpymodule*
                       (concatenate 'string pymodule-name "." fun-name)
                       (or as fun-name)))
         (fun-doc (pyeval fullname ".__doc__"))
         (fun-symbol (intern lisp-fun-name lisp-package)))
    (destructuring-bind (parameter-list pass-list)
        (get-arg-list fullname (find-package lisp-package))
      `(progn
         (defun ,fun-symbol (,@parameter-list)
           ,(or fun-doc "Python function")
           ,(first pass-list)
           ,(when safety
              (if (builtin-p pymodule-name)
                  `(python-start-if-not-alive)
                  (if *called-from-defpymodule*
                      `(pyexec ,*function-reload-string*)
                      `(pyexec ,(function-reload-string :pymodule-name pymodule-name
                                                        :fun-name fun-name
                                                        :as as)))))
           ,(second pass-list))
         ,(if *called-from-defpymodule* `(export ',fun-symbol ,lisp-package))))))

(defvar *is-submodule* nil
  "Used for coordinating import statements from defpymodule while calling recursively")

;;; packages in python are collection of modules; module is a single python file
;;; In fact, all packages are modules; but all modules are not packages.
(defun defpysubmodules (pymodule-name lisp-package continue-ignoring-errors)
  (let ((submodules
         (pyeval "tuple((modname, ispkg) for importer, modname, ispkg in "
                 "pkgutil.iter_modules("
                 pymodule-name
                 ".__path__))")))
    (when (and (stringp submodules)
               (string= "None" submodules))
      (setq submodules nil))
    (iter (for (submodule has-submodules) in submodules)
      (for submodule-fullname = (concatenate 'string
                                             pymodule-name "." submodule))
      (when (and (char/= #\_ (aref submodule 0)) ; avoid private modules / packages
                 ;; pkgutil is of type module
                 ;; import matplotlib does not import matplotlib.pyplot
                 ;; https://stackoverflow.com/questions/14812342/matplotlib-has-no-attribute-pyplot
                 ;; We maintain these semantics.
                 ;; The below form errors in the case of submodules and
                 ;; therefore returns NIL.
                 (ignore-errors (pyeval "type(" submodule-fullname
                                        ") == type(pkgutil)")))
        (collect (let ((*is-submodule* t))
                   (macroexpand-1
                    `(defpymodule ,submodule-fullname
                         ,has-submodules
                         :lisp-package ,(concatenate 'string lisp-package "."
                                                     (lispify-name submodule))
                         :continue-ignoring-errors ,continue-ignoring-errors))))))))

(declaim (ftype (function (string string) pymodule-import-string)))
(defun pymodule-import-string (pymodule-name lisp-package)
  (let ((package-in-python (pythonize (intern lisp-package))))
    (values
     (cond (*is-submodule* "")
           (*lisp-package-supplied-p*
            (concatenate 'string "import " pymodule-name
                         " as " package-in-python))
           (t (concatenate 'string "import " pymodule-name)))
     package-in-python)))

(defun function-reload-string (&key pymodule-name lisp-package fun-name as)
  (if *called-from-defpymodule*
      (pymodule-import-string pymodule-name lisp-package)
      (concatenate 'string "from " pymodule-name " import " fun-name " as " as)))

;;; One we need is the name of the package inside python
;;; And the other is the name of the package inside lisp
;;; The relation between the two being that the lisp name should
;;; pythonize to the python name.
(defmacro defpymodule (pymodule-name &optional (import-submodules nil)
                       &key
                         (lisp-package (lispify-name pymodule-name) lisp-package-supplied-p)
                         (reload t) (safety t)
                         (continue-ignoring-errors t)
                         (silent *defpymodule-silent-p*))
  "Import a python module (and its submodules) lisp-package Lisp package(s). 
Example:
  (py4cl:defpymodule \"math\" :lisp-package \"M\")
  (m:sqrt 4)   ; => 2.0
\"Package already exists.\" is returned if the package exists and :RELOAD 
is NIL.
Arguments:
  PYMODULE-NAME: name of the module in python, before importing
  IMPORT-SUBMODULES: leave nil for purposes of speed, if you won't use the  
    submodules
  LISP-PACKAGE: lisp package, in which to intern (and export) the callables
  RELOAD: whether to redefine and reimport
  SAFETY: value of safety to pass to defpyfun; see defpyfun
  SILENT: prints \"status\" lines when NIL"
  (check-type pymodule-name string) ; is there a way to (declaim (macrotype ...?
  ;; (check-type as (or nil string)) ;; this doesn't work!
  (check-type lisp-package string)

  (let ((package (find-package lisp-package))) ;; reload
    (if package
        (if reload 
            (delete-package package)
            (return-from defpymodule "Package already exists."))))
  
  (python-start-if-not-alive) ; Ensure python is running

  (pyexec "import inspect")
  (pyexec "import pkgutil")
  
  ;; fn-names  All callables whose names don't start with "_"
  (let ((*lisp-package-supplied-p* lisp-package-supplied-p)
        (*defpymodule-silent-p* silent))
    (multiple-value-bind (package-import-string package-in-python)
        (pymodule-import-string pymodule-name lisp-package)
      (pyexec package-import-string)
      (unless silent (format t "~&Defining ~D for accessing python package ~D...~%"
                             lisp-package
                             package-in-python))
      (handler-bind ((pyerror (lambda (e)
                                (if continue-ignoring-errors
                                    (invoke-restart 'continue-ignoring-errors)
                                    e))))
        (restart-case
            (let* ((fun-names (pyeval "tuple(name for name, fn in inspect.getmembers("
                                      package-in-python
                                      ", callable) if name[0] != '_')"))
                   ;; Get the package name by passing through reader, rather than using STRING-UPCASE
                   ;; so that the result reflects changes to the readtable
                   ;; Note that the package doesn't use CL to avoid shadowing.
                   (exporting-package
                    (or (find-package lisp-package) (make-package lisp-package :use '())))
                   (fun-symbols (mapcar (lambda (pyfun-name)
                                          (fun-symbol pyfun-name
                                                      (concatenate 'string
                                                                   package-in-python
                                                                   "."
                                                                   pyfun-name)
                                                      lisp-package))
                                        (if (and (stringp fun-names)
                                                 (or (string= "()" fun-names)
                                                     (string= "None" fun-names)))
                                            (setq fun-names ())
                                            fun-names))))
              ;; In order to create a DEFUN form, we need FUN-SYMBOL
              ;; inside the LISP-PACKAGE package at compiler time.
              ;; However, the effects of MAKE-PACKAGE form followed by DEFPACKAGE
              ;; seem to be implementation dependent. Things work in SBCL, CCL and ECL.
              ;; But ABCL refuses to replace the existing package defined by MAKE-PACKAGE.
              ;; Therefore, we need an explicit EXPORT statement in DEFPYFUN. And we also
              ;; do away with DEFPACKAGE deeming it redundant.
              `(progn
                 (defpackage ,lisp-package
                   (:use)
                   (:export ,@fun-symbols))
                 ,@(if import-submodules
                       (defpysubmodules package-in-python
                           lisp-package
                         continue-ignoring-errors))
                 ,@(iter (for fun-name in fun-names)
                         (for fun-symbol in fun-symbols)
                         (collect
                             (let* ((*called-from-defpymodule* t)
                                    (*function-reload-string*
                                     (function-reload-string :pymodule-name pymodule-name
                                                             :lisp-package lisp-package
                                                             :fun-name fun-name)))
                               (macroexpand-1 `(defpyfun
                                                   ,fun-name ,package-in-python
                                                 :lisp-package
                                                 ,exporting-package
                                                 :lisp-fun-name ,(format nil "~A" fun-symbol)
                                                 :safety ,safety)))))
                 t))
          (continue-ignoring-errors nil)))))) ; (defpymodule "torch" t) is one test case

(defmacro defpyfuns (&rest args)
  "Each ARG is supposed to be a 2-3 element list of 
 (pyfun-name pymodule-name) or (pyfun-name pymodule-name lisp-fun-name).
In addition, when ARG is a 2-element list, then, the first element can be
a list of python function names. "
  `(progn
     ,@(iter outer
             (for arg-list in args)
             (ecase (length arg-list)
               (2 (etypecase (first arg-list)
                    (list (iter
                            (for pyfun-name in (first arg-list))
                            (in outer (collect `(defpyfun ,pyfun-name
                                                    ,(second arg-list))))))
                    (string (collect `(defpyfun ,@arg-list)))))
               (3 (collect `(defpyfun ,(first arg-list) ,(second arg-list)
                              :lisp-fun-name ,(third arg-list))))))))

(defun export-function (function python-name)
  "Makes a lisp FUNCTION available in python process as PYTHON-NAME"
  (pyexec (concatenate 'string
                       python-name
                       "=_py4cl_LispCallbackObject("
                       (write-to-string
                        (object-handle function))
                       ")")))
