(asdf:defsystem "py4cl2"
  :serial t
  :description "Some improvements over py4cl. py4cl is a library for interfacing with python   libraries from common lisp, using streams to communicate with the python process.
 Report the issues at https://github.com/digikar99/py4cl2/issues
 (More) Documentation is available at https://digikar99.github.io/py4cl2/"
  :author "Ben Dudson <benjamin.dudson@york.ac.uk> (Original author), Shubhamkar Ayare <shubhamayare@yahoo.co.in> (Fork Contributor)"
  :license "MIT"
  :version "2.6.0"                  ; py4cl is assumed to be version 1
  :depends-on ("alexandria"
               "bordeaux-threads"
               "cl-json"
               "trivial-garbage"
               "iterate"
               "numpy-file-format"
               "parse-number"
               "uiop")
  :pathname #P"src/"
  :components ((:static-file "python-code" :pathname #P"../py4cl.py")
               (:file "package")
               (:file "config"         :depends-on ("package"))
               (:file "features"       :depends-on ("package"))
               (:file "reader"         :depends-on ("package"))
               (:file "writer"         :depends-on ("package" "features"))
               (:file "python-process" :depends-on ("package" "features"))
               (:file "lisp-classes"   :depends-on ("package"))
               (:file "callpython"     :depends-on ("reader"
                                                    "writer"
                                                    "python-process"))
               (:file "import-export"  :depends-on ("callpython"))
               (:file "do-after-load"  :depends-on ("import-export"))
               (:static-file ".config" :pathname #P"../.config"))
  :in-order-to ((test-op (test-op "py4cl2/tests"))))

(asdf:defsystem "py4cl2/tests"
  :serial t
  :description "Unit tests for the py4cl library."
  :author "Ben Dudson <benjamin.dudson@york.ac.uk>"
  :license "MIT"
  :depends-on ("py4cl2"
               #-(or :ecl :abcl)
               "numcl"
               "alexandria"
               "clunit"
               "trivial-garbage"
               "trivial-arguments")
  :pathname #P"tests/"
  :components ((:file "package")
               (:file "tests"))
  :perform (test-op (o c) (symbol-call :py4cl2/tests :run)))
