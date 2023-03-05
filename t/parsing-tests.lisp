(defpackage #:parsing-tests
  (:use :cl :fiveam))

(in-package :parsing-tests)

(def-suite test-parsing-suite)
(in-suite test-parsing-suite)

(test parsing-basic-stream
  "Parse the stream ^กาขา/กา<t1>+ขา<ม>$ ^กา<t1>+ขา<ม>$"
  (is (equal (caar (cl-apertium-stream:parse-stream "^กาขา/กา<t1>+ขา<ม>$ ^กา<t1>+ขา<ม>$"))
             :joined-lexical-unit)))

(test parsing-basic-stream-from-real-corpus
  "Parse the stream ^Round<pr>$ ^Plastic<n><sg>$ ^Handle<vblex><pres>$ ^Rubber<n><sg>$ ^Seal<vblex><pres>$ ^20<num>$ ^mm<n><sg>$^.<sent>$"
  (is (equal (caar (cl-apertium-stream:parse-stream "^Round<pr>$ ^Plastic<n><sg>$ ^Handle<vblex><pres>$ ^Rubber<n><sg>$ ^Seal<vblex><pres>$ ^20<num>$ ^mm<n><sg>$^.<sent>$"))
             :lexical-unit)))

(test parsing-a-string-with-an-escape-character
  "Test Parsing a string with an escape character"
  (let* ((stream (cl-apertium-stream:parse-stream "^gram<n><sg>$ \\/ ^pack<vblex><pres>$"))
         (unparsed-unit (cadr stream)))
    (is (eq (car unparsed-unit)
            :UNPARSED))
    (is (equal (cdr unparsed-unit)
               " / "))))

(run! 'test-parsing-suite)
