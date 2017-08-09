;;; This file was written by Earl Ducaine and is released under the
;;; MIT license.

(asdf:defsystem :google-drive-cl-fuse
  :depends-on (:alexandria :cffi)
  :components
  ((:file "package")
   (:file "python-inter-op")
   (:file "os")
   (:file "os-path")
   (:file "oauth2client-file")
   (:file "quick-start")
;;   (:file "gdfuses")
   ))
