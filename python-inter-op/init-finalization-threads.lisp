

;;; The interface to the low level python c-api.  It is a mechanical
;;; transciption that interface.
;;; https://docs.python.org/3/c-api/init.html
;;;
;;; extensions or variations are noted.
;;;
;;; Transcription rules


(in-package :python-inter-op)



(cffi:defcfun ("Py_Initialize" py-initialize) :void)

(cffi:defcfun ("Py_InitializeEx" py-initialize-ex) :void (initsigs :int))

;; todo -- should be wrapped 
(cffi:defcfun ("Py_IsInitialized" py-is-initialized) :int)



(cffi:defcfun ("Py_FinalizeEx" py-Py_FinalizeEx) :void (initsigs :int))


"int Py_FinalizeEx()"

"int Py_SetStandardStreamEncoding(const char *encoding, const char *errors)"




(defun str (&rest args)
  (apply #'concatenate `(string ,@args)))




;; (let ((c-types "(\\s+(\\w*)\\s+)+"))
;;   (scan-to-strings "(.*)\((.*)\)" "int Py_SetStandardStreamEncoding(const char *encoding, const char *errors)")




(cffi:defcfun ("Py_FinalizeEx" py-Py_FinalizeEx) :void (initsigs :int))


;; "int Py_SetStandardStreamEncoding(const char *encoding, const char *errors)"


(cffi:defctype pchar :pointer)



(cffi:defctype charp :pointer)



(defun c-function-to-lisp-function (function-name)
  (string-downcase ))
  

(cffi:define-foreign-type char-string ()
  ((encoding :reader char-string-encoding :initarg :encoding))
  (:actual-type :pointer))

(cffi:define-parse-method char-string (&key (encoding :utf-8))
  (make-instance ’my-string-type :encoding encoding))

(defmethod translate-to-foreign (string (type char-string))
    (foreign-string-alloc string :encoding (string-type-encoding type)))

(defmethod translate-from-foreign (pointer (type my-string-type))
    (foreign-string-to-lisp pointer :encoding (string-type-encoding type)))

(defmethod free-translated-object (pointer (type my-string-type) param)
  (declare (ignore param))
  (foreign-string-free pointer))




(cffi:defcfun ("Py_SetStandardStreamEncoding" py-set-standard-stream-encoding)
    :int (initsigs pchar) (errors  pchar))



(defun map-string-c-type-to-cffi-type (ctype)
  (cond
    ((string= "int" ctype) :int)
    ((string= "void" ctype) :void)))

(defun generate-cffi-args-list (c-args)  
  (let* ((c-args (remove "const" c-args :test #'string=))
	 (length (length c-args))
	 (c-args (reduce (lambda (rest arg)
			   (cond
			     ((eq #\* (elt arg 0))
			      (push (subseq arg 1 (1- length)) rest)
			      (push "*" rest))
			     ((eq #\* (elt arg (1- length)))
			      (cons (subseq arg 0 (- length 2))
				    (cons  "*" rest)))
			     (t
			       (cons arg rest))))
			 (butlast c-args)
			 :initial-value '()))
	 (arg-type-def
	  (mapcar
	   #'map-string-c-type-to-cffi-type	  
	   c-args))
	 (arg-symbol (intern (string-upcase (first (last c-args))))))
    `(,arg-symbol ,@arg-type-def)))
  

(defun generate-defcfun-list ()
  (multiple-value-bind (match parts)
      (scan-to-strings
       "([^(]*)\\((.*)\\)"
       "int Py_SetStandardStreamEncoding(const char *encoding, const char *errors)")
    (declare (ignore match))
    (let* ((function-and-return (split "\\s" (elt parts 0)))
	   (args (mapcar
		  (lambda (arg)
		    (split "\\s" (string-trim '(#\Space) arg)))
		  (split "\\,"  (elt parts 1))))
	   (return-spec (mapcar #'map-string-c-type-to-cffi-type
				(remove 'const (butlast function-and-return))))
	   (function 
		      (car (last function-and-return))))
      `(cffi:defcfun (,function
		      ,(intern (string-upcase
				(symbol-munger:camel-case->lisp-name function))))
	   ,@return-spec))))
  


	   
      

  

(cl-ppcre:split "\\s" "stitch \t in time saves nine.")



(scan-to-strings (str c-types) "const int "))
		      "\((\\w+\\s*,\\s*\\w+)*\)" "int Py_FinalizeEx()")




(let* ((parsed
	(cl-ppcre:split
	 "[()]" 
	 "int Py_SetStandardStreamEncoding(const char *encoding, const char *errors)"))
       (args (butlast parsed))
       (function 


 (split-sequence:SPLIT-SEQUENCE #\Space "A stitch in time saves nine.")
