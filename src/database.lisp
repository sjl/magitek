(in-package :magitek.database)

(defvar *database* nil)

(defun db-connect (&key (path "database.sqlite"))
  (setf *database* (connect path))
  (values))

(defun initialize-database ()
  (execute-non-query *database*
    "CREATE TABLE IF NOT EXISTS tweets(
        id INTEGER PRIMARY KEY,
        account TEXT NOT NULL,
        content TEXT NOT NULL,
        timestamp DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL
     )")
  (execute-non-query *database*
    "CREATE INDEX IF NOT EXISTS idx_tweets_account_timestamp
     ON tweets (account, timestamp)")
  (values))

(defun db-insert-tweet (account tweet)
  (execute-non-query *database*
    "INSERT INTO tweets (account, content) VALUES (?, ?)"
    (aesthetic-string account)
    (aesthetic-string tweet))
  (values))

(defun db-tweeted-since-p (account minutes-ago)
  (check-type minutes-ago (integer 1))
  (ensure-boolean
    (execute-single *database*
      "SELECT content FROM tweets
        WHERE account = ?
          AND timestamp > datetime('now', ?)
        LIMIT 1
      "
      (aesthetic-string account)
      (format nil "-~D minutes" minutes-ago))))
