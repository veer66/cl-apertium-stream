# cl-apertium-stream-parser
### _Vee Satayamas <5ssgdxltv@relay.firefox.com>_ 

Apertium stream parser written in Common Lisp

## Example

```
CL-USER> (ql:quickload :cl-apertium-stream-parser)
To load "cl-apertium-stream-parser":
  Load 1 ASDF system:
    cl-apertium-stream-parser
; Loading "cl-apertium-stream-parser"
[package cl-apertium-stream-parser].
(:CL-APERTIUM-STREAM-PARSER)
CL-USER> (cl-apertium-stream-parser:parse-stream "^a$")
((:LEXICAL-UNIT ((:FLAG) (:LING-FORM . "a") NIL)))
NIL
T
```

## License

Apache-2.0
