# cl-apertium-stream
### _Vee Satayamas <5ssgdxltv@relay.firefox.com>_ 

cl-apertium-stream written in Common Lisp

## Example

```
CL-USER> (ql:quickload :cl-apertium-stream)
To load "cl-apertium-stream":
  Load 1 ASDF system:
    cl-apertium-stream
; Loading "cl-apertium-stream"
[package cl-apertium-stream].
(:CL-APERTIUM-STREAM)
CL-USER> (cl-apertium-stream:parse-stream "^a$")
((:LEXICAL-UNIT ((:FLAG) (:LING-FORM . "a") NIL)))
NIL
T
```

## License

Apache-2.0
