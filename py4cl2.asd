(asdf:defsystem "py4cl2"
  :serial t
  :description "Some improvements over py4cl. py4cl is a library for interfacing with python   libraries from common lisp, using streams to communicate with the python process. 
 Report the issues at https://github.com/digikar99/py4cl2/issues
 (More) Documentation is available at https://digikar99.github.io/py4cl2/"
  :author "Ben Dudson <benjamin.dudson@york.ac.uk> (Original author), Shubhamkar Ayare <shubhamayare@yahoo.co.in> (Fork Contributor)"
  :license "MIT"
  :version "2.3.0"                  ; py4cl is assumed to be version 1
  :depends-on ("alexandria"
               "bordeaux-threads"
               "cl-json"
               "trivial-garbage"
               "iterate"
               "numpy-file-format"
               "parse-number"
               "uiop")
  :pathname #P"src/"
  :serial t
  :components ((:static-file "python-code" :pathname #P"../py4cl.py")
               (:file "package")
               (:file "config")
               (:file "reader")
               (:file "writer")
               (:file "python-process")
               (:file "lisp-classes")
               (:file "callpython")
               (:file "import-export")
               (:file "do-after-load")
               (:static-file ".config" :pathname #P"../.py4cl2.config"))
  :in-order-to ((test-op (test-op "py4cl2/tests"))))

(asdf:defsystem "py4cl2/tests"
  :serial t
  :description "Unit tests for the py4cl library."
  :author "Ben Dudson <benjamin.dudson@york.ac.uk>"
  :license "MIT"
  :depends-on ("py4cl2"
               "py4cl2+numcl"
               "clunit"
               "trivial-garbage"
               "trivial-arguments")
  :pathname #P"tests/"
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :py4cl2-tests :run)))

(asdf:defsystem "py4cl2+numcl"
  :serial t
  :description "System to provide integrations for NUMCL by wrapping arrays read from python in NUMCL:ASARRAY. However, even after loading this system, one must (SETF (CONFIG-VAR 'USE-NUMCL-ARRAYS) T) or use WITH-NUMCL-ARRAYS. Call INITIALIZE again if loading this system for the first time."
  :license "MIT"
  :depends-on ("py4cl2"
               "str"
               "numcl")
  :pathname #P"numcl/"
  :serial t
  :components ((:file "numcl")
               (:static-file ".config" :pathname #P"../.py4cl2+numcl.config")))
