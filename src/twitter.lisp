(in-package :magitek.twitter)

(defparameter *api-key* nil)
(defparameter *api-secret* nil)
(defparameter *credentials* nil)
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


(defmacro with-account (account-name &body body)
  (once-only (account-name)
    `(if-found (account (gethash ,account-name *accounts*))
       (let ((chirp:*oauth-api-key* (getf account :api-key))
             (chirp:*oauth-api-secret* (getf account :api-secret))
             (chirp:*oauth-access-token* (getf account :access-token))
             (chirp:*oauth-access-secret* (getf account :access-secret)))
         ,@body)
       (error "Account ~S not found, use (tt-authorize ~S) to get creds"
              ,account-name ,account-name))))


(defun tt-authorize (account-name)
  (format t "Visit ~A to get a PIN~%"
          (chirp:initiate-authentication :api-key *api-key*
                                         :api-secret *api-secret*))
  (princ "Enter PIN: ")
  (finish-output)
  (chirp:complete-authentication (read-line))
  (chirp:account/verify-credentials)
  (dump-auth-plist account-name))

(defun tt-load-credentials ()
  (load "creds.lisp")
  (load-accounts))

(defun tt-tweet (account text)
  (with-account account
    (chirp:tweet text)))

(defun tt-tweetable-p (text)
  (< 30 (length text) 138))

