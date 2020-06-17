(in-package :py4cl2)

#.(progn
    (alexandria:define-constant +py4cl2+numcl-config-path+
        (namestring (asdf:component-pathname (asdf:find-component "py4cl2+numcl" ".config")))
      :test 'equal)
    `(alexandria:define-constant +py4cl2+numcl-config-path+
         (namestring (asdf:component-pathname (asdf:find-component "py4cl2+numcl" ".config")))
       :test 'equal))

(defun initialize ()
  "Intended to be called first upon installation. Sets up default python command,
and numpy pickle file and lower bounds."
  (let ((pycmd (take-input "Provide the python binary to use (default python): "
                           "python"))
        (numpy-pickle-location
         (take-input "~%PY4CL2 uses pickled files to transfer large arrays between lisp
 and python efficiently. These are expected to have sizes exceeding 100MB 
 (this depends on the value of *NUMPY-PICKLE-LOWER-BOUND*). Therefore, choose an 
 appropriate location (*NUMPY-PICKLE-LOCATION*) for storing these arrays on disk.

Enter full file path for storage (default /tmp/_numpy_pickle.npy): "
                     "/tmp/_numpy_pickle.npy"))
        (numpy-pickle-lower-bound
         (parse-integer
          (take-input "Enter lower bound for using pickling (default 100000): "
                      "100000")))
        (use-numcl-arrays
         (let ((*read-eval* nil))
           (read-from-string (take-input "Use numcl arrays [t/nil] (default nil): " "nil")))))
    (setq  *config* ;; case conversion to and from symbols is handled by cl-json
           `((pycmd . ,pycmd)
             (numpy-pickle-location . ,numpy-pickle-location)
             (numpy-pickle-lower-bound . ,numpy-pickle-lower-bound)
             (use-numcl-arrays . ,use-numcl-arrays)))
    ;; to avoid development overhead, we will not bring these variables "out"
    (save-config)))

(defun load-config ()
  #.(format nil "Load to *CONFIG* from ~D" +py4cl2+numcl-config-path+)
  (let ((config-path +py4cl2+numcl-config-path+)
        (cl-json:*json-symbols-package* *package*))
    (setq *config* (with-open-file (f config-path)
                     (cl-json:decode-json f)))))

(defun save-config ()
  #.(format nil "Save to ~D from *CONFIG*" +py4cl2+numcl-config-path+)
  (let ((config-path +py4cl2+numcl-config-path+))
    
    (with-open-file (f config-path :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
      (cl-json:encode-json-alist *config* f))
    (format t "Configuration is saved to ~D.~%" config-path)))

(defun config-var (var)
  "Returns the value associated with VAR in *CONFIG*.
Configuration variables include (all in PY4CL2 package):

  - PYCMD: Path to the python binary to be used
  - NUMPY-PICKLE-LOCATION: PY4CL2 uses pickled files to transfer large arrays between lisp
 and python efficiently. These can have sizes exceeding 100MB. It is recommended that this
 be set to path on a ram-disk. See [this](https://unix.stackexchange.com/questions/66329/creating-a-ram-disk-on-linux) for 
instructions on creating a ram-disk on linux-based systems.
  - NUMPY-PICKLE-LOWER-BOUND: The minimum size of the array for which PY4CL2 should use pickled files.
  - USE-NUMCL-ARRAYS: NUMCL uses displaced arrays. If this variable is T, arrays returned by
python process are passed through NUMCL:ASARRAY before returning them to the user."
  (cdr (assoc var *config*)))

(setf (fdefinition '(setf config-var))
      (fdefinition '(setf py4cl2:config-var)))

(let ((cl-json:*json-symbols-package* *package*))
  (when (uiop:file-exists-p +py4cl2+numcl-config-path+)
    (load-config)))

(setf *python-code*
      (str:replace-all "### NUMCL EXTENSION CODE should replace this comment ###"
                       "
eval_globals[\"_py4cl_config_file_name\"] = \".py4cl2+numcl.config\"
load_config()
"
                       *python-code*))
(format t "~&Restarting python process...~%")
(pystop)
(pystart)

(defmacro with-numcl-arrays (t/nil &body body)
  (let ((original-value (gensym))
        (body-value (gensym)))
    `(let ((,original-value (config-var 'use-numcl-arrays))
           ,body-value)
       (with-output-to-string (*standard-output*)
         (setf (config-var 'use-numcl-arrays) ,t/nil))
       (setq ,body-value (progn ,@body))
       (with-output-to-string (*standard-output*)
         (setf (config-var 'use-numcl-arrays) ,original-value))
       ,body-value)))
