;;; probably, ASDF should have a feature for doing this
;;; this file should be called after loading all the other files

(in-package :py4cl)
(let ((config-path (concatenate 'string
                                (directory-namestring py4cl/config:*base-directory*)
                                ".config"))
      ;; *package* will have an arbitrary value at this point -- not
      ;; *necessarily :py4cl [2021/02/04:rpg]
      (cl-json:*json-symbols-package* :py4cl))
  (when (uiop:file-exists-p config-path)
    (load-config)))
