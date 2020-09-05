(in-package :cl-user)

(py4cl2:defpymodule "math" nil :silent t)
(py4cl2:defpymodule "numpy" nil :lisp-package "NP" :silent t)
(py4cl2:defpymodule "numpy.random" t :silent t)

(defpackage :py4cl2/tests
  (:use :cl :clunit :py4cl2 :iterate)
  (:export :run))

