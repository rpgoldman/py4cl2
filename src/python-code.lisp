(in-package :py4cl2)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(defvar *python-code*
  (let ((filename (namestring (merge-pathnames #p"py4cl.py"
                                               py4cl2/config:*base-directory*))))
    (with-output-to-string (*standard-output*)
      (iter (for line in-file filename using #'read-line)
            (write-line line)))))
