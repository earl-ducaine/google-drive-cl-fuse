;;; The interface to :os is a mechanical transciption of the python
;;; module:
;;; https://docs.python.org/3.7/library/os.html
;;;
;;; extensions or variations are noted.

(in-package :os)

(defparameter *python-string* (python-inter-op::get-python-string "this is a string"))

;; getcwd
(defun run-os-cwd ()
  (python-inter-op::call-function-from-module 'getcwd nil))

;; chdir
(defun run-os-chdir ()
  (python-inter-op::call-function-from-module
   'chdir
   '("/home/rett/dev/google-drive-fuse-drivers")))
