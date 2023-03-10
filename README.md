# cl-slog

### Intro
cl-slog implements structured logging for Common Lisp.

### Formats
The following formats are supported:

- :json
- :lisp
- :text

```
(with-slog (:format :json)
  (slog-write "hello" :tag :http :tag :request))

{"time"="2023-03-10T23:30:26.182261+01:00", "message"="hello"}
```

### Tags
Each log write may include multiple `:tag`-arguments,
```
(with-slog ()
  (slog-write "hello" :tag :http :tag :request))
```
and each `:tag`-argument may include multiple tags.
```
(with-slog ()
  (slog-write "hello" :tag '(:http :request)))
```

Tags may be included/excluded in `with-slog`.
```
(with-slog (:include '(:http)
            :exclude '(:request))
  (slog-write "hello" :tag :http :tag :request))
```

Nested lists allows matching multiple tags.
```
(with-slog (:include '((:http request)))
  (slog-write "hello" :tag http :tag request))
```

### Contexts
```with-slog-context``` may be used to add attributes to all writes within scope.

```
(with-slog (:format :json)
  (with-slog-context (:x 42)
    (slog-write "hello")))

{"time"="2023-03-10T23:30:26.182261+01:00", "message"="hello", "x"=42}
```

### Output
Passing `:stream` allows redirecting output.

```
(with-output-to-string (out)
  (with-slog (:stream out)
    (slog-write "hello")))

"(:TIME @2023-03-10T23:36:30.373368+01:00 :MESSAGE \"hello\")
"
```