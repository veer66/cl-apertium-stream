(defpackage #:parsing-tests
  (:use :cl :fiveam))

(in-package :parsing-tests)

(def-suite :parsing-tag-results)
(in-suite :parsing-tag-results)

(test parsing-basic-stream
  "Parse the stream ^กาขา/กา<t1>+ขา<ม>$ ^กา<t1>+ขา<ม>$"
  (is (equal (caar (cl-apertium-stream:parse-stream "^กาขา/กา<t1>+ขา<ม>$ ^กา<t1>+ขา<ม>$"))
             :joined-lexical-unit)))

(test parsing-basic-stream-from-real-corpus
  "Parse the stream ^Round<pr>$ ^Plastic<n><sg>$ ^Handle<vblex><pres>$ ^Rubber<n><sg>$ ^Seal<vblex><pres>$ ^20<num>$ ^mm<n><sg>$^.<sent>$"
  (is (equal (caar (cl-apertium-stream:parse-stream "^Round<pr>$ ^Plastic<n><sg>$ ^Handle<vblex><pres>$ ^Rubber<n><sg>$ ^Seal<vblex><pres>$ ^20<num>$ ^mm<n><sg>$^.<sent>$"))
             :lexical-unit)))



;; (cl-apertium-stream:parse-stream "^gram<n><sg>$ \\/ ^pack<vblex><pres>$")

(run :parsing-tag-results)
