

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
  


(defmacro str (&rest args)
  `(apply #'concatenate (list 'string ,@args)))

(defun get-cffi-type (pointer c-type)
  (unless (or (string= pointer "*")
	      (not pointer))
    (error (str "pointer must be a string made up of "
		"the asterisk character: #\* or nil")))
  (cond
    ((string= "*" pointer) :pointer)
    ((string= "int" c-type) :int)
    ((string= "void" c-type) :void)))

;; Return spec should be a list of pairs, the first element of being
;; the c pointer operator, *, or nil:
;;
;; For example:
;; const *int
;; would be represented as:
;; '((nil "const") ("*" "int"))
;;
;; Currently we only type of the following form:
;; <const> <*><int|char|long>
;; the goal should be to generalize this in the future.
(defun validate-and-cffi-ify-return-spec (return-spec)
  (cond
    ((and (= (length return-spec) 1)
	  (= (length (car return-spec)) 2))
     (get-cffi-type (nth 0 (car return-spec))
		    (nth 1 (car return-spec))))
    ((and (= (length return-spec) 2)
	  (string= (nth 1 (nth 0 return-spec))
		   "const")
	  (null (nth 0 (nth 0 return-spec)))
	  (= (length (nth 1 return-spec)) 2))
     (get-cffi-type (nth 0 (nth 1 return-spec))
		    (nth 1 (nth 1 return-spec))))
    (t
     (error "return-spec is mal-formed: ~s~%" return-spec))))


;; arg-spec should be a list of 2 or 3 items.
;;
;; if 3 items the first item should be the pair (nil "const") and
;; items 2 and 3 should be like items 1 and 2 of the two item list.
;;
;; if 2 items the first one should but of the form (nil <char|int|void|long>)
;; and the second should be of the form (<*> <SOME IDENTIFIER>)
;;
;; For example:
;; const char *encoding
;; would be represented as:
;; '((nil "const") (nil "char") ("*" encoding))
;; the function would return the list
;; '(encoding :pointer)
(defun validate-and-cffi-ify-function-arg-spec (function-arg-spec)
  (cond
    ((and (= (length return-spec) 2)
	  (= (length (nth 0 function-arg-spec)) 2)
	  (= (length (nth 1 function-arg-spec)) 2)
	  (nil (nth 0 (nth 0 function-arg-spec))))
     (get-cffi-type (nth 0 (nth 1 function-arg-spec))
		    (nth 1 (nth 0 function-arg-spec))))
    ((and (= (length return-spec) 3)
	  (= (length (nth 0 function-arg-spec)) 2)
	  (= (length (nth 1 function-arg-spec)) 2)
	  (= (length (nth 2 function-arg-spec)) 2)
	  (string= (nth 1 (nth 0 return-spec))
		   "const")
	  (null (nth 0 (nth 0 return-spec)))



	  
	  (= (length (nth 1 return-spec)) 2))
     (get-cffi-type (nth 0 (nth 1 return-spec))
		    (nth 1 (nth 1 return-spec))))
    (t
     (error "return-spec is mal-formed: ~s~%" return-spec))))

(defun run-validate-and-cffi-ify-return-spec ()
  (validate-and-cffi-ify-return-spec  '((NIL "const") ("*" "int"))))

(defun run-validate-and-cffi-ify-arg-spec ()
  (validate-and-cffi-ify-return-spec  '((NIL "const") ("*" "int"))))


(destructuring-bind (&key return-spec function-name args)
     (parse
      'function-signature
      " const *int Py_SetStandardStreamEncoding(const char *encoding, const char *errors)")
  (list (validate-and-cffi-ify-return-spec return-spec)
	function-name
	args))


(defun generate-defcfun-py-unicode-decode-utf-8 ()
  ;; (multiple-value-bind (match parts)
  ;;     (cl-ppcre:scan-to-strings
  ;;      "([^(]*)\\((.*)\\)"
  ;;      "int Py_SetStandardStreamEncoding(const char *encoding, const char *errors)")
  (destructuring-bind (&key return-spec function-name args)
      (parse
       'function-signature
       " const *int Py_SetStandardStreamEncoding(const char *encoding, const char *errors)")
    (list (validate-and-cffi-ify-return-spec return-spec)
	  function-name
	  args))
  (let* ((function-and-return (cl-ppcre:split "\\s" (elt parts 0)))
	 (args (mapcar
		(lambda (arg)
		  (cl-ppcre:split "\\s" (string-trim '(#\Space) arg)))
		(cl-ppcre:split "\\,"  (elt parts 1))))
	 (return-spec (mapcar #'map-string-c-type-to-cffi-type
			      (remove 'const (butlast function-and-return))))
	 (function 
	  (car (last function-and-return))))
    `(cffi:defcfun (,function
		    ,(intern (string-upcase
			      (symbol-munger:camel-case->lisp-name function))))
	 ,@return-spec)))



;; (cffi:defcfun ("PyUnicode_DecodeUTF8" py-unicode-decode-utf-8) :pointer
;;   (cstring :pointer)
;;   (string-size-in-bytes :int)
;;   (errors :pointer))

(cl-ppcre:split "\\s" "stitch \t in time saves nine.")



(cl-ppcre:scan-to-strings 
 "\((\\w+\\s*,\\s*\\w+)*\)" "int Py_FinalizeEx()")

(let* ((parsed
	(cl-ppcre:split
	 "[()]" 
	 "int Py_SetStandardStreamEncoding(const char *encoding, const char *errors)"))))
;;       (args (butlast parsed))
;;       (function 


 (split-sequence:SPLIT-SEQUENCE #\Space "A stitch in time saves nine.")
