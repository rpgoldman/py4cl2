(asdf:defsystem "py4cl2"
  :serial t
  :description "Some improvements over py4cl. py4cl is a library for interfacing with python libraries from common lisp, using streams to communicate with the python process. 
Report the issues at https://github.com/digikar99/py4cl2/issues
(More) Documentation is available at https://digikar99.github.io/py4cl2/"
  :author "Ben Dudson <benjamin.dudson@york.ac.uk> (Original author), Shubhamkar Ayare <shubhamayare@yahoo.co.in> (Fork Contributor)"
  :license "MIT"
  :version "2.2.2"                  ; py4cl is assumed to be version 1
  :depends-on ("trivial-garbage"
               "iterate"
               "cl-json"
               "bordeaux-threads"
               "parse-number"
               "uiop"
               "numpy-file-format"
               "numcl")
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
               (:file "do-after-load"))
  :in-order-to ((test-op (test-op "py4cl2-tests"))))
