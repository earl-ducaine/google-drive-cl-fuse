
(in-package :test)

(defun run-tests ()
  (os.path::exists "/")
  (os.path::expanduser "~"))


;; chdir
(defun run-os-chdir ()
  (python-inter-op::call-function-from-module
   'chdir
   '("/home/rett/dev/google-drive-fuse-drivers")))






