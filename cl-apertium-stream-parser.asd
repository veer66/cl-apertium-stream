;;;; cl-apertium-stream-parser.asd

(asdf:defsystem #:cl-apertium-stream-parser
  :description "Apertium stream parser written in Common Lisp"
  :author "Vee Satayamas <5ssgdxltv@relay.firefox.com>"
  :license  "Apache-2.0"
  :version "0.0.2"
  :serial t
  :depends-on (#:esrap)
  :components ((:file "package")
               (:file "cl-apertium-stream-parser")))
