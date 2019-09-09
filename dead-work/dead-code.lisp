(defun get-arg-list (fullname lisp-package)
  "Returns a list of two lists: PARAMETER-LIST and PASS_LIST"
  (if (string= "<class 'numpy.ufunc'>" (pyeval "str(type(" fullname "))"))
      (let* ((n (pyeval fullname ".nin"))
             (arg-list-without-keys
              (iter (for ch in-string "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                    (for i below n)
                    (collect (intern (string ch) lisp-package)))))
        `(,(append arg-list-without-keys
                   '(&rest keys &key out (where t) &allow-other-keys))
           ((declare (ignore out where))
            (apply #'pycall ,fullname ,@arg-list-without-keys keys))))
      (let* ((signature (ignore-errors (pyeval "inspect.signature(" fullname ")")))
             ;; errors could be value error or type error
             (pos-only (find #\/ (pyeval "str(" signature ")")))
             ;; we are ignoring futther keyword args
             (sig-dict (if signature
                           (pyeval "dict(" signature ".parameters)")
                           (make-hash-table)))
             (default-return (list '(&rest args) ; see the case for allow-other-keys
                                   `(() (apply #'pycall ,fullname args))))
             (allow-other-keys nil)
             (multiple-non-key-args nil)
             (key-val-list nil)
             arg-symbol
             rest-arg
             (allow-default nil))
        ;; below, pass-list is the argument list passed to the raw-pyexec,
        ;; or underlying function
        ;; parameter-list is the argument list corresponding to defun-visible-to-the-user
        (iter (for (key val) in-hashtable sig-dict)
              (for name = (pyeval val ".name")) ; this will not contain * or **
              (for default = (pyeval val ".default"))
              (when (typep default 'python-object) ; handle would likely be lost
                (return-from get-arg-list default-return)) ; and be unreliable
              (for name-str = (pyeval "str(" val ")"))
              (for arg-symbol = (intern (lispify-name name) lisp-package))
              (for arg-default = (if (or (symbolp default) (listp default))
                                     `',default
                                     default))
              
              (for op = (cond ((search "**" name-str) 1)
                              ((search "*" name-str) 2)
                              (t 3)))
              (print state)
              (for state
                   initially :begin
                   then (ecase op
                          (1 (ecase state
                               (:begin (appending `(&rest ,arg-symbol &key &allow-other-keys)
                                                  into parameter-list)
                                       (setq rest-arg arg-symbol)
                                       (appending
                                        `((pythonize-kwargs
                                           (progn
                                             (mapc (lambda (symbol)
                                                     (remf args
                                                           (find-symbol (symbol-name symbol)
                                                                        :keyword)))
                                                   ',parameter-list-without-defaults)
                                             args)))
                                        into pass-list)
                                       (setq allow-other-keys t)
                                       :state-2)
                               (:state-1 (setq allow-other-keys t)
                                         (setq parameter-list
                                               (iter (for symbol in parameter-list)
                                                     (if (eq symbol rest-arg)
                                                         (progn
                                                           (appending `(,symbol
                                                                        &rest
                                                                        ,arg-symbol
                                                                        &key))
                                                           (setq rest-arg arg-symbol))
                                                         (collect symbol))))
                                         :state-2)
                               (:state-2 (error "Unexpected state"))
                               (:state-3 (setq allow-other-keys t)
                                         (setq parameter-list
                                               (iter (for symbol in parameter-list)
                                                     (if (eq symbol rest-arg)
                                                         (progn
                                                           (collect '&rest)
                                                           (collect symbol))
                                                         (collect symbol))))
                                         :state-2)
                               (:end (error "Unexpected state"))))
                          (2 (ecase state
                               (:begin (collect arg-symbol into parameter-list)
                                       (collect `(format nil "~{~A~^,~}"
                                                         (mapcar #'pythonize ,arg-symbol))
                                         into pass-list)
                                       (setq rest-arg arg-symbol)
                                       (setq multiple-non-key-args t)
                                       :state-1)
                               (:state-1 (error "Unexpected state"))
                               (:state-2 (error "Unexpected state"))
                               (:state-3 (error "Unexpected state"))
                               (:end (error "Unexpected state"))))
                          (3 (ecase state
                               (:begin (cond ((null default)
                                              (collect arg-symbol into parameter-list)
                                              (collect arg-symbol into
                                                       parameter-list-without-defaults))
                                             ((null allow-default)
                                              (appending `(&optional (,arg-symbol
                                                                      ,arg-default))
                                                         into parameter-list)
                                              (setq allow-default t))
                                             (t (collect `((,arg-symbol ,arg-default))
                                                  into parameter-list)))
                                       (appending (if allow-default
                                                      `(,name "=" (pythonize ,arg-symbol) ",")
                                                      `((pythonize ,arg-symbol) ","))
                                                  into pass-list)
                                       :begin)
                               (:state-1 (collect '&key into parameter-list)
                                         (cond ((null default)
                                                (collect arg-symbol into parameter-list)
                                                (collect arg-symbol into
                                                         parameter-list-without-defaults))
                                               ((null allow-default)
                                                (appending `(&optional (,arg-symbol
                                                                        ,arg-default))
                                                           into parameter-list)
                                                (setq allow-default t))
                                               (t (collect `((,arg-symbol ,arg-default))
                                                    into parameter-list)))
                                         (appending `(,name "=" (pythonize ,default) ","))
                                         :state-3)
                               (:state-2 (error "Unexpected state"))
                               (:state-3 (collect arg-symbol into parameter-list)
                                         (appending `(,name "=" (pythonize ,default) ","))
                                         :state-3)
                               (:end (error "Unexpected state"))))))
              (while (not (eq state :end)))
              (finally
               (cond ((and allow-other-keys multiple-non-key-args)
                      (format t "~%BOTH ARE TRUE~%")
                      (nconc parameter-list '(&allow-other-keys))
                      (nconc pass-list `("," (pythonize-kwargs ,rest-arg)))
                      (setq pass-list (cons "(" pass-list)))
                     (multiple-non-key-args
                      (setq parameter-list
                            (iter (for symbol in parameter-list)
                                  (if (eq symbol rest-arg)
                                      (appending `(&rest ,symbol))
                                      (collect symbol))))
                      (setq pass-list `("(" ,@pass-list '(")"))))
                     (allow-other-keys ; no multiple-non-key-args
                      (setq parameter-list
                            (iter (for symbol in parameter-list)
                                  (case symbol
                                    (&optional (appending `(&rest ,rest-arg &key)))
                                    (&key (next-iteration))
                                    (&rest (next-iteration))
                                    (t (if (eq rest-arg symbol)
                                           (next-iteration)
                                           (collect symbol))))))
                      ()))
               (return `(,parameter-list (() (apply #'raw-pyeval
                                                    ,fullname
                                                    ,@pass-list))))
               ;; (cond ((and allow-other-keys multiple-non-key-args)
                       ;;        (append '&allow-other-keys into parameter-list)
                       ;;        (setq pass-list
                       ;;              (iter (for symbol in parameter-list)))))
                       ))
        
        ;; (iter (for (key val) in-hashtable sig-dict)
        ;;       (for name = (pyeval val ".name")) ; this will not contain * or **
        ;;       (for default = (pyeval val ".default"))
        ;;       (when (typep default 'python-object) ; handle would likely be lost
        ;;         (return-from get-arg-list default-return)) ; and be unreliable
        ;;       (for name-str = (pyeval "str(" val ")"))
        ;;       (print name-str)
        ;;       (cond ((search "**" name-str) ; assumes it be the last arg in the signature
        ;;              (setq allow-other-keys t)
        ;;              (collect 'cl:&allow-other-keys into parameter-list))
        ;;             ((search "*" (print name-str))
        ;;              (setq multiple-non-key-args t)
        ;;              (appending '(*args &rest **kwargs) into parameter-list)
        ;;              (collect `(format nil "~{~A~^,~}"
        ;;                                (mapcar #'pythonize *args))
        ;;                into pass-list))
        ;;             (t
        ;;              (setq arg-symbol (intern (lispify-name name) lisp-package))
        ;;              (collect (list arg-symbol
        ;;                             (if (or (symbolp default) (listp default))
        ;;                                 `',default
        ;;                                 default))
        ;;                into parameter-list)
        ;;              (collect arg-symbol into parameter-list-without-defaults)
        ;;              (appending (if pos-only
        ;;                             `((pythonize ,arg-symbol) ",")
        ;;                             `(,name "=" (pythonize ,arg-symbol) ","))
        ;;                         into pass-list)
        ;;              (if multiple-non-key-args
        ;;                  (appending (if pos-only
        ;;                             `((pythonize ,arg-symbol) ",")
        ;;                             `(,name "=" (pythonize ,arg-symbol) ","))
        ;;                             into pos-args))))
        ;;       (finally 
        ;;        (format t "multiple-non-key-args: ~A~%" multiple-non-key-args)
        ;;        (format t "allow-other-keys: ~A~%" allow-other-keys)
        ;;          (return-from get-arg-list
        ;;          (cond ((null pass-list) default-return)
        ;;                (pos-only `((&optional ,@parameter-list)
        ;;                            (() (raw-pyeval ,fullname "(" ,@pass-list ")"))))
        ;;                ((and multiple-non-key-args allow-other-keys)
        ;;                 `(,parameter-list
        ;;                   (() (apply #'pycall ,pass-list))))
        ;;                (allow-other-keys
        ;;                 `((&rest args &key ,@parameter-list)
        ;;                   (() (apply #'raw-pyeval
        ;;                              ,fullname "(" ,@pass-list
        ;;                              (append (pythonize-kwargs
        ;;                                       (progn
        ;;                                         (mapc (lambda (symbol)
        ;;                                                 (remf args
        ;;                                                       (find-symbol (symbol-name symbol) :keyword)))
        ;;                                               ',parameter-list-without-defaults)
        ;;                                         args))
        ;;                                      '(")"))))))
        ;;                (multiple-non-key-args t)
        ;;                (t `((&key ,@parameter-list)
        ;;                     (() (raw-pyeval ,fullname "(" ,@pass-list ")"))))))))
        ))) 


(deftest arg-list (import-export)
  (pyexec "def foo(*args, **kwargs): return (args, kwargs)")
  (defpyfun "foo") ;; (foo (args &rest kwargs &key &allow-other-keys))
  (destructuring-bind (args kwargs) (foo '(1 2 3) :a 1 :b 2)
    (assert-equalp '(1 2 3) args)
    (assert-equalp '(:a 1 :b 2) (alexandria:hash-table-plist kwargs)))
  (pyexec "def foo2(a,b,*c): return c")
  (defpyfun "foo2")  ;; (foo-2 (a b &rest c))
  (pyexec "def foo3(a,b=3,*c): return c")
  (defpyfun "foo3") ;; (foo-3 (a &optional (b 3) &rest c))
  )

