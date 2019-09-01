;;; Functions to start and stop python process

(in-package :py4cl2)

(defvar *python* nil
  "Most recently started python subprocess")

(defvar *current-python-process-id* 0
  "A number which changes when python is started. This
is used to prevent garbage collection from deleting objects in the wrong
python session")

(defvar *python-process-busy-p* nil
  "Used by pyinterrupt to determine if python process is waiting for input, or
is busy processing.

A possible workaround is to use strace.
See https://askubuntu.com/questions/1118109/how-do-i-tell-if-a-command-is-running-or-waiting-for-user-input")

(defvar *py4cl-tests* nil)

(defun pystart (&optional (command (config-var 'pycmd)))
  "Start a new python subprocess
This sets the global variable *python* to the process phandle,
in addition to returning it.
COMMAND is a string with the python executable to launch e.g. \"python\"
By default this is is set to *PYTHON-COMMAND*
"
  (setq *python*
        (uiop:launch-program
            (concatenate 'string
                         command        ; Run python executable
                         " -u "
                         ;; Path *base-pathname* is defined in py4cl.asd
                         ;; Calculate full path to python script
                         (namestring (merge-pathnames #p"py4cl.py"
                                                      py4cl2/config:*base-directory*)))
            :input :stream
            :output :stream
            :error-output :stream))
  (unless *py4cl-tests*
    (bt:make-thread (lambda ()
                      (when *python*
                        (let ((py-out (uiop:process-info-error-output *python*)))
                          (iter (while (and *python* (python-alive-p *python*)))
                                (for char = (read-char py-out nil))
                                (when char (write-char char))))))))
  (incf *current-python-process-id*))

(defun python-alive-p (&optional (process-info *python*))
  "Returns non-NIL if the python process is alive
(e.g. SBCL -> T, CCL -> RUNNING).
Optionally pass the process object returned by PYTHON-START"
  (and process-info
       (uiop:process-alive-p process-info)))

(defun python-start-if-not-alive ()
  "If no python process is running, tries to start it.
If still not alive, raises a condition."
  (unless (python-alive-p)
    (pystart))
  (unless (python-alive-p)
    (error "Could not start python process")))

;; Function defined in writer.lisp, which clears an object store
(declaim (ftype (function () t) clear-lisp-objects))

(defun pystop (&optional (process-info *python*))
  "Stop (Quit) the python process PROCESS"
  (unless (python-alive-p process-info)
    (return-from pystop))
  (let ((stream (uiop:process-info-input process-info)))
    ;; ask the python process to quit; might require a few sec?
    (write-char #\q stream))
  ;; (pyinterrupt process-info)
  #-ccl (uiop:close-streams process-info)
  (uiop:terminate-process process-info)
  (setf *python* nil) ;; what about multiple processes?
  (clear-lisp-objects))

(defun pyinterrupt (&optional (process-info *python*))
  (when (and (python-alive-p process-info)
             *python-process-busy-p*)
    (uiop:run-program
     (concatenate 'string "/bin/kill -SIGINT -"
		  (write-to-string (uiop:process-info-pid process-info)))
     :force-shell t)
    (setq *python-process-busy-p* nil)
    ;; something to do with running in separate threads! "deftest interrupt"
    (unless *py4cl-tests* (dispatch-messages process-info))))

(defun pyversion-info ()
  "Return a list, using the result of python's sys.version_info."
  (python-start-if-not-alive)
  (let ((stream (uiop:process-info-input *python*)))
    (write-char #\v stream)
    (force-output stream))
  (dispatch-messages *python*))
