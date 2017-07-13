

;;; The interface to :os is a mechanical transciption of the python
;;; module:
;;; https://docs.python.org/3.7/library/os.html
;;;
;;; extensions or variations are noted.

(defpackage :os
  (:use :common-lisp :uiop))

(in-package :os)

(defparameter *system-source-directory*
  "~/dev/google-drive-fuse-drivers/google-drive-clfuse")

(defun get-name ()
  (alexandria:switch ((string-downcase (software-type)) :test #'equal)
    ("linux" :posix)
    ("osx" :posix)
    ("mswindows" :nt)
    (t nil)))

(defvar *name* (get-name))

(defun prep-foreign-library-libpython ()
  (cffi:define-foreign-library libpython
      (:darwin (:or "libpython2.7.1.dylib" "libpython2.7.dylib"))
    (:unix (:or "libpython2.7.so.1" "libpython2.7.so"))
    (t (:default "libpython2.7")))
  (cffi:use-foreign-library libpython))

(defun prep-foreign-library-libapp-main ()
  (let ((library-path
	 (concatenate 'string *system-source-directory* "/" "libapp_main.so")))
    (eval `(cffi:define-foreign-library libapp-main
	     (:unix (:or ,library-path))))
    (cffi:use-foreign-library libapp-main)))

(prep-foreign-library-libpython)
(prep-foreign-library-libapp-main)


(cffi:defctype py-object :pointer)
(cffi:defctype py-string py-object)

;; c macro wrapper functions.
(cffi:defcfun ("app_main" app-main) :void)

;; void Py_DECREF (PyObject *o)
(cffi:defcfun ("py_decref" py-decref) :void (object :pointer))


;; c function mappings (all caps in python name has special meaning?)
(cffi:defcfun ("Py_Initialize" py-initialize) :void)
(cffi:defcfun ("PyImport_Import" py-import) :pointer (module :pointer))
(cffi:defcfun ("pyunicode_as_data" pyunicode-as-data) :pointer (pystring :pointer))
(cffi:defcfun ("PyString_FromString" py-string-from-string) :pointer (cstring :pointer))

;;; PyObject* PyObject_GetAttrString (PyObject *o, char *attr_name)
(cffi:defcfun ("PyObject_GetAttrString" py-object-get-attrstring) :pointer
  (o :pointer)
  (attr-name :pointer))


;; char* PyString_AsString (PyObject *string)
(cffi:defcfun ("PyString_AsString" py-string-as-string) :pointer
  (string :pointer))


(cffi:defcfun ("Py_DECREF" py_decref) :void (object py-object))
;; pModule = PyImport_Import(pName);

;; PyObject* PyObject_CallObject (PyObject *callable_object, PyObject *args)
(cffi:defcfun ("PyObject_CallObject" py-object-call-object) :pointer
  (callable-object :pointer)
  (args :pointer))

;; PyObject* PyTuple_New (int len)
(cffi:defcfun ("PyTuple_New" py-tuple-new) :pointer
  (callable-object :int))

;; PyObject* PyTuple_New (int len)
(cffi:defcfun ("PyTuple_Size" py-tuple-size) :int
  (tuple :pointer))


(py-initialize)
(defvar module)


;; (defparameter py-module-string (py-string-from-string import-module-string))

;;  (cffi:with-foreign-string (string "os.path")



(defparameter buff (cffi:foreign-alloc :char :count 1024))
(defparameter string-buff
  (cffi:lisp-string-to-foreign "os" buff 1024))
(defparameter import-module-string (py-string-from-string string-buff))

(defun import-os ()
  ;;(let ((module (py-string-from-string import-module-string)))
  (setf module (py-import import-module-string))
  (py-decref module))

(defparameter *foreign-function-name* nil)
(defparameter *f* nil)
(defparameter *args* nil)

(defun call-function-from-module (module-name function-name args)
  (cffi:with-foreign-strings ((foreign-module-name module-name)
			      (foreign-function-name function-name))
    (let* ((py-module-string (py-string-from-string string-buff))
	   (m (py-import py-module-string))
	   (f (py-object-get-attrstring m foreign-function-name))
	   (args (py-tuple-new 0)))
      (setf *foreign-function-name* foreign-function-name)
      (setf *f* f)
      (setf *args* args))))
	   
      ;; 	    (py-object-call-object f args)


    
      ;; results)))
      ;; 	   (cstring (py-string-as-string results)))
      ;; 	   cstring)))
      ;; (cffi:foreign-string-to-lisp cstring))))




;;      (py-decref m))))
      ;; (list m f args))))


      
      ;; 

;; PyObject* PyString_FromString (const char *v)
;; (cffi:defcfun ("PyString_FromString" py-string-from-string) py-object
;;   (in-string :pointer :char))


(defun decrease-reference-counter (python-object)
  Py_DECREF(pName);
  )


(defun get-python-string (lisp-string)
  (cffi:with-foreign-string (foo lisp-string)
    (py-string-from-string foo)))


(defparameter python-string (get-python-string "this is a string"))



(defun run-os-cwd ()
  (call-function-from-module "os" "cwd" nil))

;; (with-foreign-string (url "http://www.cliki.net/CFFI")



;;; pName = PyString_FromString(argv[1]);



;;; (cffi:use-foreign-library libpython)
