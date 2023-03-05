(defpackage #:parsing-tests
  (:use :cl :fiveam))

(in-package :parsing-tests)

(def-suite unescape-suite
  :description "Unescape test suite")

(in-suite unescape-suite)

(test unescape-do-not-escape
  "Test unescape a string which doesn't need to be unescaped"
  (is (equal (cl-apertium-stream::unescape "กา ka")
             "กา ka")))

(test unescape-basic
  "Test unescape a basic string"
  (is (equal (cl-apertium-stream::unescape "ก\\/ข\\\\\\@")
             "ก/ข\\@")))

(test unescape-a-character-that-does-not-need-to-be-escaped
  "Test unescape a character that does not need to be escaped"
  (is (equal (cl-apertium-stream::unescape "\\A")
             "A")))

(test unescape-only-an-escape-character
  "Test unescaping a string without only the escape character"
  (is (equal (cl-apertium-stream::unescape "\\")
             "")))

(run! 'unescape-suite)
