;;; probably, ASDF should have a feature for doing this
;;; this file should be called after loading all the other files

(in-package :py4cl2)
(when (uiop:file-exists-p +py4cl2-config-path+)
  (load-config))
