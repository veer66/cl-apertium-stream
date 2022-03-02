;;;; cl-apertium-stream.asd

(asdf:defsystem #:cl-apertium-stream
  :description "Apertium stream written in Common Lisp"
  :author "Vee Satayamas <5ssgdxltv@relay.firefox.com>"
  :license  "Apache-2.0"
  :version "0.0.3"
  :serial t
  :depends-on (#:esrap)
  :components ((:file "package")
               (:file "parser")))
