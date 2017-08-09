
(in-package :quick-start)

(defparameter *store* nil)

(defun get-credentials ()
  (let* ((home-dir (os.path::expanduser "~")))
    home-dir))


;; (credential-dir (os.path::join home-dir ".google-drive-cl-fuse-credentials"))
;;     (unless (os.path::exists credential-dir)
;;       (os::makedirs credential-dir))
;;     credential-dir))
;;     ;; (let* ((credential-path (os.path::join credential-dir
;;     ;; 					   "drive-python-quickstart.json")))
;;     ;;   credential-path)))


;; (setf store  (oauth2client.file::storage *credential-path*)))))




;; (credentials  (get store)))
;; (when (or (not credentials) (invalid credentials))
;;   (let* ((flow  (flow-from-clientsecrets client client-secret-file scopes)))
;;     (setf (user-agent flow)  application-name)
;;     (if flags
;; 	(setf credentials (run-flow tools flow store flags))
;; 	(setf credentials (run tools flow store)))
;;     (format t (str "storing credentials to " credential-path))
;;     credentials)))))







(defun get-credentials ()
  (os.path::expanduser "~")
  )


(defun quickstart ()
  )
