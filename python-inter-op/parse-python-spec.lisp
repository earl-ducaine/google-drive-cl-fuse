
(in-package :python-inter-op)

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
	       (list (nth 1 match)
		     (nth 3 match)))))

(defrule function-signature
    (and (? whitespace)
	 (+ identifier/ws)
	 #\(
	 (* argument-spec/comma/ws)
	 #\)
	 (? whitespace))
  (:function (lambda (match)
	       (let* ((return-spec (butlast (nth 1 match)))
		      (function-name (car (last (nth 1 match)))))
		 (list :return-spec return-spec
		       :function-name function-name
		       :args (nth 3 match))))))

(defun test-rule ()
  (parse
   'function-signature
   " const *int Py_SetStandardStreamEncoding(const char *encoding, const char *errors)"))
