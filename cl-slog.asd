(asdf:defsystem cl-slog
  :name "cl-slog"
  :version "1"
  :maintainer "codr7"
  :author "codr7"
  :description "Structured logging"
  :licence "MIT"
  :depends-on ("local-time")
  :serial t
  :components ((:file "slog")))
