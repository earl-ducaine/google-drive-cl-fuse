
(in-package :os.path)

;; getcwd
;; (defun expanduser (file)
;;   (python-inter-op::call-function-from-module
;;    "os.path"
;;    "expanduser"
;;    (list file)))

;; (eval-when (:load-toplevel :compile-toplevel :execute)
;;   os.path.join(path, *paths)

;; (symbol-function 'twice)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (python-inter-op::create-python-function 'exists)
  (python-inter-op::create-python-function 'expanduser)
  (python-inter-op::create-python-function 'join))


(defun create-curried (name op arg1)
  (setf (symbol-function name)
        (lambda (&rest args) (apply op (cons arg1 args)))))

(defmacro defun-curried (newname oldname arg)
  (if (and (symbolp newname) (symbolp oldname))
      `(create-curried ',newname (function ,oldname) ,arg)
      (error "Newname and Oldname need to be symbols")))


;; (defun join (file)
;;   (python-inter-op::call-function-from-module
;;    'expanduser
;;    (list file)))
