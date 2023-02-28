;;;; cl-apertium-stream.asd

(asdf:defsystem #:cl-apertium-stream
  :description "Apertium stream written in Common Lisp"
  :author "Vee Satayamas <5ssgdxltv@relay.firefox.com>"
  :license  "Apache-2.0"
  :version "0.0.3"
  :serial t
  :depends-on (#:esrap #:cl-ppcre)
  :components ((:file "package")
               (:file "parser")
	       (:file "generator")))


(asdf:defsystem #:cl-apertium-stream/tests
  :depends-on (#:fiveam #:cl-apertium-stream)
  :components ((:module "t"
                :serial t
                :components ((:file "parsing-tests")))))
