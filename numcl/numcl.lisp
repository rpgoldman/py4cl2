(in-package :py4cl2)

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

(load-config)

(setf *python-code*
      (str:replace-all "### NUMCL EXTENSION CODE should replace this comment ###"
                       "load_config()"
                       *python-code*))
(pyexec "_py4cl_config[\"useNumclArrays\"] = " (config-var 'use-numcl-arrays))

(defmacro with-numcl-arrays (t/nil &body body)
  (let ((original-value (gensym))
        (body-value (gensym)))
    `(let ((,original-value (config-var 'use-numcl-arrays))
           ,body-value)
       (pyexec "_py4cl_config[\"useNumclArrays\"] = " ,t/nil)
       (setq ,body-value (progn ,@body))
       (pyexec "_py4cl_config[\"useNumclArrays\"] = " ,original-value)
       ,body-value)))
