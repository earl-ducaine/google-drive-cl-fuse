;;; This file was written by Earl Ducaine and is released under the
;;; MIT license.

(asdf:defsystem :google-drive-cl-fuse
  :depends-on (:alexandria :cffi :drakma :lquery :iterate)
  :components
  ((:file "package")
   (:file "python-inter-op/interface")
   (:file "python-inter-op/init-finalization-threads")
   
   ;; (:file "os")
   ;; (:file "os-path")
   ;; (:file "quick-start")
   ;; (:file "test")
;;   (:file "gdfuses")
   ))
