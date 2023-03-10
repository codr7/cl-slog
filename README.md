# cl-slog

### Intro
cl-slog implements structured logging for Common Lisp.

### Formats
The following formats are supported:

#### :json
```
(with-slog (:format :json)
  (slog-write "hello"))

{"time":"2023-03-11T00:08:34.496032+01:00", "message":"hello"}
```
#### :lisp
```
(with-slog ()
  (slog-write "hello"))

(:TIME @2023-03-11T00:06:55.308025+01:00 :MESSAGE "hello")
```
#### :text
```
(with-slog (:format :text)
  (slog-write "hello"))

time=2023-3-11 0:7:54:624 message="hello"
```

### Tags
Rather than imposing a strict hierarchy of log levels, cl-slog supports using tags to filter logs.  

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
`with-slog-context` may be used to add attributes to all writes within scope.
```
(with-slog ()
  (with-slog-context (:x 42)
    (slog-write "hello")))

(:TIME @2023-03-11T00:10:12.366268+01:00 :MESSAGE "hello" :X 42)
```

### Output
Passing `:stream` allows redirecting output.
```
(with-output-to-string (out)
  (with-slog (:stream out)
    (slog-write "hello")))
```