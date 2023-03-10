(defpackage slog
  (:use cl)
  (:import-from local-time +iso-8601-format+ format-timestring now timestamp)
  (:export *log*
	   *text-time-format*
	   slog-context
	   slog-format
	   slog-format-attribute
	   slog-format-key
	   slog-format-record
	   slog-format-value
	   slog-include
	   slog-include?
	   slog-exclude
	   slog-exclude?
	   slog-new
	   slog-separator
	   slog-stream
	   slog-tags
	   slog-write
	   slog-write-record
	   with-slog
	   with-slog-context
	   test))

(in-package slog)

(defvar *log*)

(defvar +text-time-format+ '(:year #\- :month #\- :day #\space :hour #\: :min #\: :sec #\: :msec))

(defmacro with-slog ((&rest args) &body body)
  `(let ((*log* (slog-new ,@args)))
     ,@body))

(defmacro with-slog-context ((&rest args) &body body)
  `(with-slog (:context (append (slog-context *log*) (list ,@args))
	       :format (slog-format *log*)
	       :stream (slog-stream *log*)
	       :include (slog-include *log*)
	       :exclude (slog-exclude *log*))
     ,@body))

(defstruct slog
  (context nil :type list)
  (format :lisp :type keyword)
  (stream *standard-output* :type stream)
  (include nil :type list)
  (exclude nil :type list))

(defun slog-new (&key context (format :lisp) (stream *standard-output*) include exclude)
  (make-slog :context context :format format :stream stream :include include :exclude exclude))

(defmethod slog-format-key (fmt key)
  (string-downcase (symbol-name key)))

(defmethod slog-format-value (fmt val)
  (with-output-to-string (out)
    (print-object val out)))

(defmethod slog-format-value ((fmt (eql :json)) (val (eql t)))
  "true")

(defmethod slog-format-value ((fmt (eql :text)) (val (eql t)))
  "t")

(defmethod slog-format-value ((fmt (eql :json)) (val (eql nil)))
  "false")

(defmethod slog-format-value ((fmt (eql :text)) (val (eql nil)))
  "f")

(defmethod slog-format-value ((fmt (eql :json)) (val timestamp))
  (with-output-to-string (out)
    (write-char #\" out)
    (format-timestring out val :format +iso-8601-format+)
    (write-char #\" out)))

(defmethod slog-format-value ((fmt (eql :text)) (val timestamp))
  (format-timestring nil val :format +text-time-format+))

(defmethod slog-format-attribute ((fmt (eql :json)) key val)
  (format nil "\"~a\":~a" (slog-format-key fmt key) (slog-format-value fmt val)))

(defmethod slog-format-attribute ((fmt (eql :text)) key val)
  (format nil "~a=~a" (slog-format-key fmt key) (slog-format-value fmt val)))

(defmethod slog-separator ((fmt (eql :json)))
  ", ")

(defmethod slog-separator ((fmt (eql :text)))
  " ")

(defmethod slog-format-record (fmt &rest ats)
  (with-output-to-string (out)
    (labels ((rec (in)
	       (when in
		 (let ((k (pop in))
		       (v (pop in)))
		   (write-string (slog-format-attribute fmt k v) out)
		   (when in
		     (write-string (slog-separator fmt) out)))
		 (rec in))))
      (rec ats))))

(defmethod slog-format-record ((fmt (eql :json)) &rest ats)
  (with-output-to-string (out)
    (write-char #\{ out)
    (write-string (apply #'call-next-method fmt ats) out)
    (write-char #\} out)))

(defmethod slog-format-record ((fmt (eql :lisp)) &rest ats)
  (with-output-to-string (out)
    (print-object ats out)))

(defun flatten (lst)
  (labels ((rec (in out)
	     (if in
		 (let ((x (pop in)))
		   (if (listp x)
		       (rec in (append (rec x nil) out))
		       (rec in (cons x out))))
		 out)))
    (nreverse (rec lst nil))))

(defun slog-tags (&rest ats)
  (labels ((rec (in tags ats)
	     (if in
		 (let ((k (pop in))
		       (v (pop in)))
		   (if (eq k :tag)
		       (rec in (cons v tags) ats)
		       (rec in tags `(,v ,k ,@ats))))
		 (values (flatten tags) (nreverse ats)))))
    (rec ats nil nil)))

(defun tags-match? (xs ys)
  (dolist (x xs)
    (if (listp x)
	(when (= (length (intersection x ys)) (length x))
	  (return-from tags-match? t))
	(when (member x ys)
	  (return-from tags-match? t))))
  nil)	  

(defun slog-include? (log tags)
  (or (null (slog-include log)) (tags-match? (slog-include log) tags)))

(defun slog-exclude? (log tags)
  (tags-match? (slog-exclude log) tags))

(defmethod slog-write-record (log time msg &rest ats)
  (multiple-value-bind (tags ats) (apply #'slog-tags ats)
    (when (and (slog-include? log tags) (not (slog-exclude? log tags)))
      (format (slog-stream log)
	      "~a~%"
	      (apply #'slog-format-record
		     (slog-format log)
		     `(:time ,time :message ,msg ,@(append (slog-context log) ats))))))
    nil)

(defun slog-write (msg &rest ats)
  (apply #'slog-write-record *log* (now) msg ats))

(defun test ()
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
		   (format nil "{\"time\"=true, \"message\"=\"hello\", \"foo\"=1, \"bar\"=\"baz\"}~%"))))
