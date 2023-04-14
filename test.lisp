(defpackage slog-test
  (:use cl slog)
  (:import-from timestamp new-timestamp)
  (:export run))

(in-package slog-test)

(defun run ()
  (assert (string= (with-output-to-string (result)
		     (with-slog (:stream result
				 :format :text)
		       (slog-write-record *log* t "hello" :tag :http :tag :request)))
		   (format nil "time=t message=\"hello\"~%")))
  
  (assert (string= (with-output-to-string (result)
		     (with-slog (:stream result
				 :format :text
				 :include '(:http))
		       (slog-write-record *log* t "hello" :tag :http :tag :request)))
		   (format nil "time=t message=\"hello\"~%")))
  
  (assert (string= (with-output-to-string (result)
		     (with-slog (:stream result
				 :format :text
				 :include '((:http :request)))
		       (slog-write-record *log* t "hello" :tag :http :tag :request)))
		   (format nil "time=t message=\"hello\"~%")))

  (assert (string= (with-output-to-string (result)
		     (with-slog (:stream result :format :text)
		       (slog-write-record *log* (new-timestamp 2000 :jan 2 10 20 30 40) "hello")))
		   (format nil "time=\"2000-1-2 10:20:30.40\" message=\"hello\"~%")))

  (assert (string= (with-output-to-string (result)
		     (with-slog (:stream result :format :json)
		       (slog-write-record *log* (new-timestamp 2000 :jan 2 10 20 30 40) "hello")))
		   (format nil "{\"time\":\"2000-1-2 10:20:30.40\", \"message\":\"hello\"}~%")))

  (assert (string= (with-output-to-string (result)
		     (with-slog (:stream result
				 :format :text
				 :exclude '(:http))
		       (slog-write-record *log* t "hello" :tag :http :tag :request)))
		   ""))
  
  (assert (string= (with-output-to-string (result)
		     (with-slog (:stream result
				 :format :text
				 :include '(:http)
				 :exclude '(:request))
		       (slog-write-record *log* t "hello" :tag :http :tag :request)))
		   ""))
  (assert (string= (with-output-to-string (result)
		     (with-slog (:stream result
				 :format :text
				 :include '((:http :request))
				 :exclude '(:request))
		       (slog-write-record *log* t "hello" :tag :http :tag :request)))
		   ""))
  (assert (string= (with-output-to-string (result)
		     (with-slog (:stream result
				 :format :text)
		       (with-slog-context (:x 42)
			 (slog-write-record *log* t "hello"))))
		   (format nil "time=t message=\"hello\" x=42~%")))

  (assert (string= (with-output-to-string (result)
		     (with-slog (:stream result
				 :format :json)
		       (slog-write-record *log* t "hello" :foo 1 :bar "baz")))
		   (format nil "{\"time\":true, \"message\":\"hello\", \"foo\":1, \"bar\":\"baz\"}~%"))))


