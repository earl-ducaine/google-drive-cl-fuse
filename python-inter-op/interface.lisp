

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
    ;;    (:unix (:or "libpython2.7.so.1" "libpython2.7.so"))
    (:unix (:or "libpython3.6dm.so.1" "libpython3.6dm.so"))
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

;; PyObject
;; (python-inter-op::call-function-from-module
;;  "types"
;;  "type"
;;  (py-tuple-get-item
;;   (generate-python-args-from-lisp-list
;;    '(("/home/rett"))) 0))

;; PyString (all-lowercase are wrapper functions) obsolete
;; (cffi:defcfun ("PyString_AsString" py-string-as-string) :pointer (pystring :pointer))
;; (cffi:defcfun ("PyString_FromString" py-string-from-string) :pointer (cstring :pointer))
;; (cffi:defcfun ("pystring_check" py-string-check) :int (py-object :pointer))

;; PyUnicode (all-lowercase are wrapper functions)
(cffi:defcfun ("PyUnicode_AsUTF8AndSize" py-unicode-as-utf8-and-size)
    :pointer
  (pystring :pointer)
  (string-size-in-bytes :pointer))

(cffi:defcfun ("PyUnicode_DecodeUTF8" py-unicode-decode-utf-8) :pointer
  (cstring :pointer)
  (string-size-in-bytes :int)
  (errors :pointer))

(cffi:defcfun ("pyunicode_check" py-string-check) :int (py-object :pointer))

;; PyLong
(cffi:defcfun ("pylong_check" py-long-check) :int (py-object :pointer))

;; PyBool
(cffi:defcfun ("pybool_check" py-bool-check) :int (py-object :pointer))

;; PyCallable
(cffi:defcfun ("pycallable_check" py-callable-check) :int (py-object :pointer))

;; PyInt
(cffi:defcfun ("PyInt_AsLong" py-int-as-long) :long (py-object :pointer))

;; PyList
(cffi:defcfun ("PyList_New" py-list-new) :pointer
  (size :int))

(cffi:defcfun ("PyList_Append" py-list-append) :pointer
  (list :pointer)
  (item :pointer))

(cffi:defcfun ("PyList_Size" py-list-size) :int
  (list :pointer))

(cffi:defcfun ("pylist_check" py-list-check) :int
  (object :pointer))

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

(cffi:defcfun ("pytuple_check" py-tuple-check) :int
  (object :pointer))

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

(defparameter *args* nil)
(defparameter *foreign-module-name* nil)
(defparameter *foreign-function-name* nil)
(defparameter *py-module-string* nil)
(defparameter *m* nil)
(defparameter *f* nil)
(defparameter *py-foreign-function-name* nil)

;; errors
;; "surrogateescape"
;; "strict"

;; Python strings are in reality Unicode objects

  ;;(babel:STRING-TO-OCTETS lisp-string)
  ;;  (let ((array (coerce #(84 117 114 97 110 103 97)

(defun list-to-array (sequence)
  (map 'vector #'identity sequence))

(defun array-to-list (sequence)
  (map 'list #'identity sequence))

(defun py-utf-8-from-string (lisp-string)
  (let* ((lisp-vector (babel:string-to-octets lisp-string))
	 (foreign-mem (cffi:foreign-alloc :char :initial-contents lisp-vector))
	 (errors  (cffi:foreign-string-alloc "surrogateescape"))
	 )
    (values (py-unicode-decode-utf-8 foreign-mem (length lisp-vector) errors)
	   (length lisp-vector))))

(defun string-from-py-utf-8 (py-utf-8)
  (let* ((length (cffi:foreign-alloc :int))
	 (foreign-array (py-unicode-as-utf8-and-size
			 py-utf-8
			 length)))
    (babel:octets-to-string 
     (iter (for i  from 0 below (cffi:mem-aref length :int 0))
	   (collect
	       (cffi:mem-aref foreign-array :char i)
	     :result-type '(vector
			    (unsigned-byte
			     8)))))))

(defun py-string-as-string (py-string)
  (string-from-py-utf-8 py-string))

(defun test-string-functions ()
  (let ((compare-string "aoeu"))
    (string= (string-from-py-utf-8 (py-utf-8-from-string compare-string))
	     compare-string)))

;; Depreciated
(defun py-string-from-string (lisp-string)
  (py-utf-8-from-string lisp-string))

(defparameter import-module-string (py-string-from-string "os"))

(defun import-os ()
  ;;(let ((module (py-string-from-string import-module-string)))
  (setf module (py-import import-module-string))
  (py-decref module))

;; (defun string-from-py-utf-8 (py-utf-8)
;;   py-utf-8
;;   ;; (multi-value-bind () (py-unicode-as-utf8-and-size py-utf-8)
;;   ;; 		    (let ((array (coerce #(84 117 114 97 110 103 97)
;;   ;; 					 (vector (unsigned-byte 8)))))
;;   ;; 		       (py-unicode-decode-utf-8 (foreign-string array))
;;   ;; 		      array
;;   ;; 		      ))
;;   )

(defun convert-strings ()
  (cffi:foreign-string-to-lisp
   (pyunicode-as-data (py-string-from-string
		       (cffi:lisp-string-to-foreign "os" buff1 1024)))))

;;(cffi:foreign-string-to-lisp )
;; "/home/rett/dev/google-drive-fuse-drivers"

(defun convert-lisp-list-to-python-list (lisp-list)
  ;; convert and push each Lisp item onto the Python list.
  (let ((python-list (py-list-new 0)))
    (dolist (lisp-item lisp-list)
      (py-list-append python-list
		      (convert-lisp-object-to-python-object lisp-item)))
    python-list))

(defun convert-lisp-object-to-python-object (object)
  (ctypecase object
    (string (cffi:with-foreign-string (foreign-string object)
	      (py-string-from-string foreign-string)))
    (list (convert-lisp-list-to-python-list object))
    (symbol nil)))

;; Generate a python args object from a list of values
(defun generate-python-args-from-lisp-list (args)
  (let ((result (py-tuple-new (length args)))
	(index 0))
    (dolist (arg  args)
      (py-tuple-set-item result index (convert-lisp-object-to-python-object arg))
      (incf index))
    result))

(defun convert-python-object-to-lisp-object (python-object)
  (cond
    ((/= (py-string-check python-object) 0)
     (get-py-string python-object))
    ((/= (py-bool-check python-object) 0)
     (get-py-bool python-object))
    ((/= (py-long-check python-object) 0)
     (get-py-long python-object))
    (t (error t "Received unexpected Python type"))))

(defun generate-lisp-list-from-python-tuple (tuple)
  (let (lisp-list)
    (dotimes (i (py-tuple-size tuple) lisp-list)
      (push (convert-python-object-to-lisp-object
	     (py-tuple-get-item tuple i)) lisp-list))))

(defun get-specs () )
  
(ql:quickload :drakma)
(ql:quickload :lquery)
(defparameter response  (drakma:http-request "https://docs.python.org/2/library/os.path.html"))
;; (lquery:$ (initialize response))
;; (lquery:$ "dl.function" (serialize)))

;;; Examples
;;; args:
;;;    '()
;;;    


;;; https://docs.python.org/2/library/os.path.html

(defun validate-type (args type-args)
  (dolist (type-arg type-args)
    (dolist (arg args)
      (unless (typep arg type-arg)
	(error "Wrong number of arguments or wrong argument type")))))

(defun call-function-from-module (module-name function-name args &optional type-args)
  (declare (ignore type-args))
  (let ((foreign-module-name (py-utf-8-from-string module-name))
	(foreign-function-name (py-utf-8-from-string function-name)))
    (let* ((py-module-string (py-string-from-string foreign-module-name))
	   (m (py-import py-module-string))
	   (f (py-object-get-attrstring m foreign-function-name)))
      (when (= (py-callable-check f) 0)
      	(error
      	 "Unable to find callable python type: module (~s) function-name (~s)"
      	 module-name function-name))
      (convert-python-object-to-lisp-object
       (let ((result
	      (py-object-call-object f (generate-python-args-from-lisp-list
					args))))
	 (when (= (cffi:pointer-address result) 0)
	   (error "Problems executing the following function: module (~s) function-name (~s)"
		  module-name function-name))
	 result)))))


;; these all leak!
(defparameter str1 (cffi:foreign-alloc :char :count 1024))
(defparameter str2 (cffi:foreign-alloc :char :count 1024))
(defparameter module-name "os.path")
(defparameter function-name "join")
(defparameter foreign-module-name (cffi:lisp-string-to-foreign
				   module-name
				   str1
				   (1+ (length module-name))))
(defparameter foreign-function-name (cffi:lisp-string-to-foreign
				     function-name
				     str2
				     (1+ (length function-name))))
;; (defparameter py-module-string (py-string-from-string foreign-module-name))
;; (defparameter m (py-import py-module-string))
;;(defparameter f (py-object-get-attrstring m foreign-function-name))
;;(defparameter py-args (generate-python-args (list "/home/rett" "dev")))
;; (py-object-call-object f py-args)


(defun get-py-string (py-string)
  (cffi:foreign-string-to-lisp (py-string-as-string py-string)))

(defun get-py-long (py-long)
  (py-int-as-long py-long))

(defun get-py-bool (py-bool)
  (and t (py-int-as-long py-bool)))

(defun decrease-reference-counter (python-object)
  (declare (ignore python-object))
  ;; Py_DECREF(pName);
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
