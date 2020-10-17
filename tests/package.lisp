(in-package :cl-user)

(py4cl2:defpymodule "math" nil :silent t)
(py4cl2:defpymodule "numpy" nil :lisp-package "NP" :silent t)
(py4cl2:defpymodule "numpy.random" t :silent t)

;;; - Test on travis; not on default quicklisp; otherwise, quicklisp needs to
;;;   install networkx
;;; - Do not test on ECL on travis just to save some travis time
#+(or (and travis
           (not ecl))
      py4cl2-full-tests)
;; TODO: Make the presence of py4cl2-full-tests override travis in tests.lisp
(py4cl2:defpymodule "networkx" nil :lisp-package "NX" :silent t)

(defpackage :py4cl2/tests
  (:use :cl :clunit :py4cl2 :iterate)
  (:export :run))

