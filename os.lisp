;;; The interface to :os is a mechanical transciption of the python
;;; module:
;;; https://docs.python.org/3.7/library/os.html
;;;
;;; extensions or variations are noted.

(in-package :os)

(defparameter python-string (get-python-string "this is a string"))

;; getcwd
(defun run-os-cwd ()
  (call-function-from-module "os" "getcwd" nil))

;; chdir
(defun run-os-chdir ()
  (call-function-from-module "os" "chdir" '("/home/rett/dev/google-drive-fuse-drivers")))