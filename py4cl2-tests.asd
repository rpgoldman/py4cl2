(asdf:defsystem "py4cl2-tests"
  :serial t
  :description "Unit tests for the py4cl library."
  :author "Ben Dudson <benjamin.dudson@york.ac.uk>"
  :license "MIT"
  :depends-on ("py4cl2"
               "clunit"
               "trivial-garbage")
  :pathname #P"tests/"
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :py4cl-tests :run))) 
