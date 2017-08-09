;; packages that map to Python modules

(defpackage :oauth2client.file
  (:use :common-lisp))

(defpackage :os
  (:use :common-lisp :uiop))

(defpackage :os.path
  (:use :common-lisp))

;; Ordineary packages

(defpackage :python-inter-op
  (:use :common-lisp :uiop))

(defpackage :quick-start
  (:use :common-lisp))

(defpackage :test
  (:use :common-lisp))
