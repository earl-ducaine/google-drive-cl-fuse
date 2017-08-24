;;; The interface to :os is a mechanical transciption of the python
;;; module:
;;; https://docs.python.org/3.7/library/os.html
;;;
;;; extensions or variations are noted.

(in-package :os)

(defparameter *python-string* (python-inter-op::get-python-string "this is a string"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (python-inter-op::create-python-function 'chdir)
  (python-inter-op::create-python-function 'getcwd))
