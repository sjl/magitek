(in-package :magitek)

(defparameter *accounts* (make-hash-table))

(defun dump-auth-plist (account-name)
  (list :account-name account-name
        :api-key chirp:*oauth-api-key*
        :api-secret chirp:*oauth-api-secret*
        :access-token chirp:*oauth-access-token*
        :access-secret chirp:*oauth-access-secret*))

(defun add-account (account)
  (setf (gethash (getf account :account-name) *accounts*)
        account))

(defun load-accounts ()
  (map nil #'add-account *credentials*))

(defun authorize (account-name)
  (format t "Visit ~A to get a PIN~%"
          (chirp:initiate-authentication :api-key *api-key*
                                         :api-secret *api-secret*))
  (princ "Enter PIN: ")
  (finish-output)
  (chirp:complete-authentication (read-line))
  (chirp:account/verify-credentials)
  (dump-auth-plist account-name))

(defmacro with-account (account-name &body body)
  (once-only (account-name)
    `(if-found account (gethash ,account-name *accounts*)
       (let ((chirp:*oauth-api-key* (getf account :api-key))
             (chirp:*oauth-api-secret* (getf account :api-secret))
             (chirp:*oauth-access-token* (getf account :access-token))
             (chirp:*oauth-access-secret* (getf account :access-secret)))
         ,@body)
       (error "Account ~S not found, use (authorize ~S) to get creds"
              ,account-name ,account-name))))
