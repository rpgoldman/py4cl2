(in-package :py4cl2)

(defgeneric python-getattr (object slot-name)
  (:documentation "Called when python accesses an object's slot (__getattr__)"))

