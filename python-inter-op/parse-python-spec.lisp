
(in-package :python-inter-op)

(defclass c-function-type ()
  (is-const
   c-type
   is-pointer))

(defclass c-function-argument ()
  (identifier
   c-function-type))

(defclass c-function-declaration ()
  (return-type
   c-name
   lisp-name
   arguments))

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

(defun get-is-const (is-const-snippet)
  (unless (= (length is-const-snippet) 2)
    (error "All type segments must lists with two elements"))
  (cond
    ((and (string= (second is-const-snippet) "const")
	  (null (first is-const-snippet)))
     t)
    ((not (string= (second is-const-snippet) "const"))
     nil)
    (t
     (error "in-const-snippet is malformed: ~s~%" is-const-snippet))))

(defun translate-string-to-c-type (string)
  (keyword (intern "NEWKW" "KEYWORD")))

(defun get-c-type (c-type-snippet)
  (unless (and (= (length c-type-snippet) 2)
	       (not (or (null (second c-type-snippet))
			(string= (second c-type-snippet) "")))
	       (or (string= (first c-type-snippet) "*")
		   (null (first c-type-snippet))))
    (error "All type segments must lists with two elements: ~s~%"
	   c-type-snippet))  
  (values (string= (first c-type-snippet) "*")
	  (intern (string-upcase (second c-type-snippet)) "KEYWORD")))

'((NIL "const") ("*" "int"))

(defun get-type-from-snippet (type-snippet)
  (let* ((type (make-instance 'c-function-type)))
    (cond
      ((= (length type-snippet) 2)
       ;; Currently only support two arguments if the first one is a
       ;; const
       (unless (get-is-const (first type-snippet))
	 (error "type-snippet is malformed: ~s~%"
		type-snippet))
       (setf (slot-value type 'is-const) t)
       (multiple-value-bind (is-pointer c-type)
	   (get-c-type (second type-snippet))
	 (setf (slot-value type 'is-pointer) is-pointer)
	 (setf (slot-value type 'c-type) c-type)))
      ((= (length type-snippet) 1)
       (setf (slot-value type 'is-const) nil)
       (multiple-value-bind (is-pointer c-type)
	   (get-c-type (first type-snippet))
	 (setf (slot-value type 'is-pointer) is-pointer)
	 (setf (slot-value type 'c-type) c-type)))
      (t
       (error "type-snippet is invalid: ~s~%"
	      type-snippet)))
    type))

(defun get-argument-from-snippet (argument-snippet)
  (let ((argument (make-instance 'c-function-argument)))
    ;; The last item in list should be a pair, the first element of
    ;; the pair being either "*" or nil and the second element being
    ;; the argument identifier.  If the first element of the last
    ;; argument is "*" than set the first element of the next to the
    ;; last argument to "*", since that's really the one that's
    ;; qualified. E.g.
    ;;
    ;; ((NIL "const") (NIL "char") ("*" "encoding")) -->
    ;; ((NIL "const") ("*" "char") ("*" "encoding"))
    ;;
    ;; We then disregard the "*" in the final element (but don't
    ;; delete it.
    (let* ((length-argument-snippet  (length argument-snippet))
	   (pointer-char (first (first (last argument-snippet))))
	   adjusted-snippet)
      (dotimes (i (1- length-argument-snippet) type)
	(if (= i (- length-argument-snippet 2))
	    (push (list pointer-char (second (nth i argument-snippet))) type)
	    (push (nth i argument-snippet))))
      (make-instance
       'c-function-type
       (get-type-from-snippet adjusted-snippet))
	   (type-segments (butlast argument-snippet))
	   (last-type-segment (first (last type-segments)))
	   (butlast type-segments)
	   (type (get-type-from-snippet )
    (setf (slot-value argument 'identifier))))))

(defun get-function-declaration-from-snippet (declaration-snippet)
  (unless (= (length declaration-snippet) 3)
    (error "declaration-snippet is malformed: ~s~%"
	   declaration-snippet))
  (let ((function-declaration (make-instance 'function-declaration)))
    (setf (slot-value 'function-declaration)
	  (get-type-from-snippet (first declaration-snippet)))
    (setf (slot-value 'c-name) (second declaration-snippet))
    (setf (slot-value 'lisp-name)
	  (intern (string-upcase
		   (symbol-munger:camel-case->lisp-name
		    (second declaration-snippet)))
		  "KEYWORD"))
    (setf (slot-value 'arguments)
	  (mapcar #'get-argument-from-snippet
	   (third declaration-snippet)))))
    
    


(destructuring-bind (&key return-spec function-name args)
     (parse
      'function-signature
      " const *int Py_SetStandardStreamEncoding(const char *encoding, const char *errors)")
  (list (validate-and-cffi-ify-return-spec return-spec)
	function-name
	args))

(defparameter *py-function-declaration*
  '(:POINTER (NIL "Py_SetStandardStreamEncoding")
    ((((NIL "const") (NIL "char") ("*" "encoding")) NIL)
     (((NIL "const") (NIL "char") ("*" "errors")) NIL))))

(defrule whitespace
    (+ (or #\Space #\Tab #\Newline))
  (:constant nil))

(defun non-first-identifier-character (character)
  (or (alphanumericp character)
      (char= character #\_)))

(defrule identifier
    (and (alpha-char-p character)
	 (* (non-first-identifier-character character)))
  (:text t))

;;	 (? (or #\* #\&))

;; no need to provide initial white space.  That's the job of whoever
;; calls us, we are, however responsible for ending white space.
;; pehaps we should have a rule that all sub-expressions only match
;; 'trimmed' sequence of characters?
(defrule identifier/ws
    (and (? (or #\* #\&))
	 (? whitespace)
	 identifier
	 (? whitespace))
  (:function (lambda (match)
	       (list (nth 0 match)
		     (nth 2 match)))))

(defrule argument-spec/comma/ws
    (and (? whitespace)
	 (+ identifier/ws)
	 (? #\,)
	 (? whitespace))
  (:function (lambda (match)
	       (list (nth 1 match)))))
		     ;;(nth 3 match)))))

(defrule function-signature
    (and (? whitespace)
	 (+ identifier/ws)
	 #\(
	 (* argument-spec/comma/ws)
	 #\)
	 (? whitespace))
  (:function (lambda (match)
	       (let* ((return-spec (butlast (nth 1 match)))
		      (function-name (second (car (last (nth 1 match)))))
		      (args (nth 3 match)))
		 
		 (list :return-spec return-spec
		       :function-name function-name
		       :args args)))))

(defun test-rule ()
  (parse
   'function-signature
   " const *int Py_SetStandardStreamEncoding(const char *encoding, const char *errors)"))
