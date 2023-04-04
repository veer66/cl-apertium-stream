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

(test parsing-with-at
  "Parse something with at"
  (let* ((stream (cl-apertium-stream:parse-stream "^ONLY@CENTRAL$"))
         (lu (car stream))
         (lu-data (cdr lu))
         (lu-data0 (car lu-data))
         (ling-form (cadr lu-data0))
         (ling-from-data (cdr ling-form)))
    (is (equal ling-from-data "ONLY@CENTRAL"))))

(test parsing-with-slash-at
  "Parse something with slash and at"
  (let* ((stream (cl-apertium-stream:parse-stream "^ONLY\\@CENTRAL$"))
         (lu (car stream))
         (lu-data (cdr lu))
         (lu-data0 (car lu-data))
         (ling-form (cadr lu-data0))
         (ling-from-data (cdr ling-form)))
    (is (equal ling-from-data "ONLY@CENTRAL"))))

(test parsing-with-sharp
  "Parse something with #"
  (is (cl-apertium-stream:parse-stream "^fell asleep/fall<vblex><past># asleep$")))

(test parsing-with-space-sharp
  "Parse something with space and #"
  (is (cl-apertium-stream:parse-stream "^fell asleep/fall<vblex><past> # asleep$")))

(test parsing-with-escaped-dollar-sign
  "Parse something with \\$"
  (is (cl-apertium-stream:parse-stream "^6<num>$ \\$^discount<n><sg>$")))


(test parsing-with-escaped-backslash-quote
  "Parse something with \\\\ \""
  (is (equal "\\ \"" (cdar (cl-apertium-stream:parse-stream "\\\\ \"")))))

(test parsing-with-sharp-in-unparsed
  "Parse something with # in unparsed"
  (is (cl-apertium-stream:parse-stream "^Item<n><sg>$ # ^*PO25P4S$")))

(test parsing-with-star-in-unparsed
  "Parse something with * in unparsed"
  (is (cl-apertium-stream:parse-stream "^.<sent>$ * ^*suckerfish$")))

(test parsing-with-star-in-unparsed
  "Parse something with / in unparsed"
  (is (cl-apertium-stream:parse-stream "^.<sent>$ \\/ ^*suckerfish$")))

(run! 'test-parsing-suite)
