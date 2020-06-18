(asdf:defsystem "py4cl2+numcl"
  :serial t
  :description "System to provide integrations for NUMCL by wrapping arrays read from python in NUMCL:ASARRAY. However, even after loading this system, one must (SETF (CONFIG-VAR 'USE-NUMCL-ARRAYS) T) or use WITH-NUMCL-ARRAYS. Call INITIALIZE again if loading this system for the first time."
  :license "MIT"
  :depends-on ("py4cl2"
               "str"
               "numcl")
  :pathname #P"numcl/"
  :serial t
  :components ((:file "numcl"))
  :in-order-to ((test-op (test-op "py4cl2+numcl/tests"))))

(asdf:defsystem "py4cl2+numcl/tests"
  :serial t
  :description "Test system for py4cl2+numcl system"
  :license "MIT"
  :depends-on ("py4cl2"
               "py4cl2+numcl"
               "clunit"
               "trivial-garbage"
               "trivial-arguments")
  :pathname #P"tests/"
  :serial t
  :components ((:file "tests")
               (:file "numcl"))
  :perform (test-op (o c) (symbol-call :py4cl2/tests :run)))

