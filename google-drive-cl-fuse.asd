;;; This file was written by Earl Ducaine and is released under the
;;; MIT license.

(asdf:defsystem :google-drive-cl-fuse
  :depends-on (:alexandria :cffi)
  :components
  ((:file "package")
   (:file "oauth2client-file")
   (:module standard-library
   	    :depends-on (:package)
   	    :components
	    ((:file "python-inter-op")
	     (:file "os")
	     (:file "os-path")))
   (:module test
   	    :depends-on (:standard-library)
   	    :components
	    ((:file "test")))))
