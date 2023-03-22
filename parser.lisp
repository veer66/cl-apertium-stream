;;;; cl-apertium-stream.lisp

(in-package #:cl-apertium-stream)

(defun concat-string (lst)
  (format nil "~{~a~}" lst))

(define-condition invalid-char-list-element (error)
  ((lst :initarg :lst :reader lst)))

(defun char-and-list-to-string (l)
  (loop with buf = '()
        for k in l
        do
           (cond
             ((characterp k) (push k buf))
             
             ((and (listp k)
                   (stringp #1=(cadr k))
                   (= 1 (length #1#)))
              (push (car (coerce #1# 'list))
                    buf))
             
             (t (error 'invalid-char-list-element :lst l)))
        finally
           (return (coerce (reverse buf) 'string))))

(defrule format (and #\[
                     (+ (or (not (or #\[ #\]))
                            (and #\\ #\[)
                            (and #\\ #\])))
                     #\])
  (:lambda (lst)
    (cons :format (concat-string (cadr lst)))))

(defparameter *lu-special-char-tab*
  (let* ((ch-list (coerce "^$@*/<>{}\[]" 'list))
         (tab (make-hash-table)))
    (loop for ch in ch-list do
      (setf (gethash ch tab) t))
    tab))

(defun not-lu-special-char (ch)
  (not (gethash ch *lu-special-char-tab*)))

(defrule ling-form-char (or (not (or #\^ #\$ #\* #\# #\/ #\< #\> #\{ #\} #\\ #\[ #\]))
                            (and #\\ #\^)
                            (and #\\ #\$)
                            (and #\\ #\@)
                            (and #\\ #\*)
                            (and #\\ #\/)
                            (and #\\ #\<)
                            (and #\\ #\>)
                            (and #\\ #\{)
                            (and #\\ #\})
                            (and #\\ #\\)
                            (and #\\ #\[)
                            (and #\\ #\])
                            (and #\\ #\#)))

(defrule tag (and #\< (+ (not (or #\< #\>))) #\>)
  (:lambda (lst)
    (concat-string (cadr lst))))

(defrule flag (? (or #\* #\# #\@))
  (:lambda (lst)
    (cons :flag lst)))

(defrule ling-form (* ling-form-char)
  (:lambda (lst)
    (cons :ling-form (char-and-list-to-string lst))))

(defrule sub-invariable-part (and #\# ling-form)
  (:lambda (lst)
    (cdadr lst)))

(defrule invariable-part
    (* sub-invariable-part)
  (:lambda (lst)
    (cons :invariable-part (car lst))))

(defrule tags (* tag)
  (:lambda (lst)
    (cons :tags lst)))

(defrule sub-lu (and flag
                     ling-form
		     (* " ")
                     invariable-part
                     tags
		     (* " ")
                     invariable-part))

(defrule sub-lus (and sub-lu (* (and #\/ sub-lu)))
  (:lambda (lst)
    (let* ((analyses lst)
           (first-analysis (car analyses))
           (rest-analyses (cadr analyses))
           (reformatted-rest-analyses (loop for a in rest-analyses
                                            collect
                                            (cadr a)))
           (reformatted-analyses (cons first-analysis reformatted-rest-analyses)))      
      reformatted-analyses)))

(defrule basic-lu (and #\^ sub-lus #\$)
  (:lambda (lst)
    (cons :lexical-unit (cadr lst))))

(defrule joined-sub-lu (and sub-lu (* (and #\+ sub-lu)))
  (:lambda (sub-lus)
    (cons (car sub-lus)
          (loop for del-lu in (cadr sub-lus)
                collect
                (cadr del-lu)))))

(defrule joined-sub-lus (and joined-sub-lu (* (and "/" joined-sub-lu)))
  (:lambda (joined-sub-lus)
    (cons (car joined-sub-lus)
          (loop for del-lu in (cadr joined-sub-lus)
                collect
                (cadr del-lu)))))

(defrule joined-lu (and #\^ joined-sub-lus #\$)
  (:lambda (lst)
    (cons :joined-lexical-unit (cadr lst))))

(defrule unparsed (+ (or (not (or #\^ #\$ #\* #\# #\/ #\< #\> #\{ #\} #\\ #\[ #\]))
			 (and #\\ #\^)
			 (and #\\ #\$)
			 (and #\\ #\@)
			 (and #\\ #\*)
			 (and #\\ #\/)
			 (and #\\ #\<)
			 (and #\\ #\>)
			 (and #\\ #\{)
			 (and #\\ #\})
			 (and #\\ #\\)
			 (and #\\ #\[)
			 (and #\\ #\])
			 (and #\\ #\#)))
  (:lambda (lst)
    (cons :unparsed (unescape (char-and-list-to-string lst)))))

(defrule chunk-children (and #\{
                             (* (or format
                                    basic-lu
                                    joined-lu
				    unparsed
                                    ))
                             #\})
  (:lambda (lst)
    (cadr lst)))

(defrule chunk (and sub-lu
                    chunk-children)
  (:lambda (lst)    
    (list :chunk
	  (cons :head (car lst))
          (cons :children (car (cdr lst))))))


(defrule stream-unit (or unparsed format basic-lu joined-lu chunk))

(defrule stream (* stream-unit))

(defun parse-stream (s)
  (parse 'stream s))

;; (parse 'stream "^กาขา/กา<t1>+ขา<ม>$ ^กา<t1>+ขา<ม>$")
;; (parse 'chunk "N1<SN><a>{^i$[<o>]^j$[</o>] ^k$}")
;; (parse 'joined-lu "^กาขา/กา<t1>+ขา<ม>$")
;; (parse 'joined-sub-lus "กาขา/กา<t1>+ขา<ม>")
;; (parse 'joined-sub-lus "กาขาตัว/กา<t1>+ขา<ม>#ตัว")
;; (parse 'joined-sub-lus "กาขาตัว/กา<t1>+ขา#ตัว<ม>")
;; (parse 'joined-sub-lu "กา<t1>+ขา<ม>")
;; (parse 'basic-lu "^กา<t1>$")
;; (parse 'basic-lu "^กา<t1>/กา<t2>/กา<t3>$")
;; (parse 'sub-lu "*กา<t1><t2>")
;; (parse 'sub-lu "กา<t1><t2>")
;; (parse 'sub-lu "#<t1><t2>")
;; (parse 'sub-lu "<t1><t2>")
;; (parse 'sub-lu "กา")
;; (parse 'sub-lu "หมาบ้าน# ตัว<n><sg>")
;; (parse 'flag "")
;; (print (parse 'sub-lu "หมาบ้าน<n><sg># ตัว"))
;; (print (parse 'format "[HTML]"))
;; (print (parse 'unparsed "           "))
