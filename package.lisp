;;;; package.lisp

(defpackage #:cl-apertium-stream
  (:use #:cl #:esrap #:cl-ppcre)
  (:export #:parse-stream #:to-stream))
