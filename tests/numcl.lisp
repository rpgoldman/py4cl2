(in-package :py4cl2/tests)

(deftest pycall-one-arg-string (callpython-utility)
  (with-numcl-arrays nil
    (assert-equalp #("h" "e" "l" "l" "o")
        (py4cl2:pycall "list" "hello"))))

;; ==================== NUMCL-ARRAYS ======================================

(deftest use-numcl-arrays (numcl-arrays)
  (with-numcl-arrays t
    (assert-true (numcl:numcl-array-p (pyeval #(1 2 3))))
    (assert-true (numcl:numcl-array-p (pyeval #2A((1 2 3) (4 5 6))))))
  (with-numcl-arrays nil
    (assert-false (numcl:numcl-array-p (pyeval #(1 2 3))))
    (assert-false (numcl:numcl-array-p (pyeval #2A((1 2 3) (4 5 6)))))))

(defun run (&optional interactive? result-for)
  "Run all the tests for py4cl2."
  (with-numcl-arrays nil
    (run-suite 'py4cl :use-debugger interactive?)))
