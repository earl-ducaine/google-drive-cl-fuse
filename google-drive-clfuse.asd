;;; This file was written by Earl Ducaine and is released under the
;;; MIT license.

(asdf:defsystem :google-drive-clfuse
  :depends-on (:alexandria :cffi)
  :components
  ((:file "package")
   (:file "python-inter-op.lisp")
   (:file "os")
;;   (:file "gdfuses")
   ))
