

;;; The interface to :os is a mechanical transciption of the python
;;; module:
;;; https://docs.python.org/3.7/library/os.html
;;;
;;; extensions or variations are noted.


(in-package :python-inter-op)

(defparameter *library-path*
  (merge-pathnames (asdf:system-source-directory :google-drive-cl-fuse)
		   "libapp_main.so"))

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
  (let ((library-path *library-path*))
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




;; PyString
(cffi:defcfun ("PyString_AsString" py-string-as-string) :pointer (pystring :pointer))
(cffi:defcfun ("PyString_FromString" py-string-from-string) :pointer (cstring :pointer))
(cffi:defcfun ("pystring_check" py-string-check) :int (py-object :pointer))
(cffi:defcfun ("pybool_check" py-bool-check) :int (py-object :pointer))

;; PyInt
(cffi:defcfun ("PyInt_AsLong" py-int-as-long) :long (py-object :pointer))

;; PyTuple
(cffi:defcfun ("PyTuple_New" py-tuple-new) :pointer
  (callable-object :int))

(cffi:defcfun ("PyTuple_GetItem" py-tuple-get-item) :pointer
  (tuple-object :pointer)
  (index :int))

(cffi:defcfun ("PyTuple_SetItem" py-tuple-set-item) :int
  (tuple-object :pointer)
  (index :int)
  (object :pointer))

(cffi:defcfun ("PyTuple_Size" py-tuple-size) :int
  (tuple :pointer))

;;; PyObject* PyObject_GetAttrString (PyObject *o, char *attr_name)
(cffi:defcfun ("PyObject_GetAttrString" py-object-get-attrstring) :pointer
  (o :pointer)
  (attr-name :pointer))


(cffi:defcfun ("Py_DECREF" py_decref) :void (object py-object))
;; pModule = PyImport_Import(pName);

;; PyObject* PyObject_CallObject (PyObject *callable_object, PyObject *args)
(cffi:defcfun ("PyObject_CallObject" py-object-call-object) :pointer
  (callable-object :pointer)
  (args :pointer))



(py-initialize)
(defvar module)


;; (defparameter py-module-string (py-string-from-string import-module-string))

;;  (cffi:with-foreign-string (string "os.path")



(defparameter buff1 (cffi:foreign-alloc :char :count 1024))
(defparameter buff2 (cffi:foreign-alloc :char :count 1024))
(defparameter *string-buff*
  (cffi:lisp-string-to-foreign "os" buff1 1024))
(defparameter import-module-string (py-string-from-string *string-buff*))

(defun import-os ()
  ;;(let ((module (py-string-from-string import-module-string)))
  (setf module (py-import import-module-string))
  (py-decref module))

(defparameter *args* nil)
(defparameter *foreign-module-name* nil)
(defparameter *foreign-function-name* nil)
(defparameter *py-module-string* nil)
(defparameter *m* nil)
(defparameter *f* nil)
(defparameter *py-foreign-function-name* nil)


(defun convert-strings ()
  (cffi:foreign-string-to-lisp
   (pyunicode-as-data (py-string-from-string
		       (cffi:lisp-string-to-foreign "os" buff1 1024)))))

;;(cffi:foreign-string-to-lisp )

;; "/home/rett/dev/google-drive-fuse-drivers"
(defun convert-lisp-object-to-python-object (object)
  (ctypecase object
    (string (cffi:with-foreign-string (foreign-string object)
	      (py-string-from-string foreign-string)))
    (symbol  nil)))

;; Generate a python args object from a list of values
(defun generate-python-args (args)
  (let ((result (py-tuple-new (length args))))
    (dolist (arg args result)
      (py-tuple-set-item result 0 (convert-lisp-object-to-python-object arg)))))

(defun convert-python-object-to-lisp-object (python-object)
  (cond
    ((/= (py-string-check python-object) 0)
     (get-py-string python-object))
    ((/= (py-bool-check python-object) 0)
     (get-py-bool python-object))
    (t (error t "Received unexpected Python type"))))

(defun generate-lisp-list-from-python-tuple (tuple)
  (let (lisp-list)
    (dotimes (i (py-tuple-size tuple) lisp-list)
      (push (convert-python-object-to-lisp-object
	     (py-tuple-get-item tuple i)) lisp-list))))

(defun call-function-from-module (module-name function-name args)
  (cffi:with-foreign-strings ((foreign-module-name module-name)
			      (foreign-function-name function-name))
    (let* ((py-module-string (py-string-from-string foreign-module-name))
	   (m (py-import py-module-string))
	   (f (py-object-get-attrstring m foreign-function-name)))
      (convert-python-object-to-lisp-object
	(py-object-call-object f (generate-python-args args))))))

(defun get-py-string (py-string)
  (cffi:foreign-string-to-lisp (py-string-as-string py-string)))

(defun get-py-bool (py-bool)
  (and t (py-int-as-long py-bool)))

(defun decrease-reference-counter (python-object)
  Py_DECREF(pName);
  )


(defun get-python-string (lisp-string)
  (cffi:with-foreign-string (foo lisp-string)
    (py-string-from-string foo)))

(defun symbol-to-python-name (symbol)
  (values
   (string-downcase (package-name (symbol-package symbol)))
   (string-downcase (symbol-name symbol))))

(defun create-python-function (function-name)
  (multiple-value-bind (py-module-name py-function-name)
      (symbol-to-python-name function-name)
  (setf (symbol-function function-name)
        (lambda (&rest args)
	  (call-function-from-module
	   py-module-name
	   py-function-name
	   args)))))
