(in-package #:cl-apertium-stream)

(define-condition generation-error (error)
  ((%nodes :initarg :nodes
	   :reader nodes)
   (%expected-symbol :initarg :expected-symbol
		     :reader expected-symbol)))

(defun to-spaces-substream (node)
  (unless (eq (car node) :SPACES)
    (error 'generation-error :nodes (list node) :expected-symbol :SPACES))
  (cdr node))

(defun to-format-substream (node)
  (unless (eq (car node) :FORMAT)
    (error 'generation-error :nodes (list node) :expected-symbol :FORMAT))
  (let ((format-text (cdr node)))
    (if format-text
	(format nil "[~A]" format-text)
	"")))

(defun to-flag-substream (node)
  (unless (eq (car node) :FLAG)
    (error 'generation-error :nodes (list node) :expected-symbol :FLAG))
  (or (cdr node) ""))

(defvar LING-FORM-ESCAPE-PAT "([\\^\\$@\\*\\\\<>{}/\\]\\[])")

(defun escape-ling-form (text)
  (regex-replace-all LING-FORM-ESCAPE-PAT
		     text
		     "\\\\\\1"
		     :preserve-case t))

(defun to-ling-form-substream (node)
  (unless (eq (car node) :LING-FORM)
    (error 'generation-error :generation-error :nodes (list node)
					       :expected-symbol :LING-FORM))
  (let ((text (cdr node)))
    (if text
	(escape-ling-form text)
	"")))

(defun to-invariable-part-substream (node)
  (unless (eq (car node) :INVARIABLE-PART)
    (error 'generation-error :generation-error :nodes (list node)
					       :expected-symbol :INVARIABLE-PART))
  (let ((text (cdr node)))
    (if text
	(format nil "#~A" (escape-ling-form text))
	"")))

(defun to-tags-substream (node)
  (unless (eq (car node) :TAGS)
    (error 'generation-error :generation-error :nodes (list node)
					       :expected-symbol :TAGS))
  (let ((texts (cdr node)))
    (if texts
	(format nil "~{~A~}"
		(loop for text in texts collect (format nil "<~A>" text)))
	"")))

(defun to-sub-lu-substream (node)
  (format nil "~{~A~}"
	  (loop for child-node in node
		collect
		(let ((sym (car child-node)))
		  (case sym
		    (:FLAG (to-flag-substream child-node))
		    (:LING-FORM (to-ling-form-substream child-node))
		    (:TAGS (to-tags-substream child-node))
		    (:INVARIABLE-PART (to-invariable-part-substream child-node))
		    (otherwise ""))))))

(defun child-nodes-to-lexical-unit-substream (child-nodes)
  (if child-nodes
      (format nil "~{~A~^/~}"
	      (loop for child-node in child-nodes
		    collect
		    (to-sub-lu-substream child-node)))
      ""))

(defun to-lexical-unit-substream (node)
  (unless (eq (car node) :LEXICAL-UNIT)
    (error 'generation-error :generation-error :nodes (list node)
					       :expected-symbol :LEXICAL-UNIT))
  (let ((child-nodes (cdr node)))
    (child-nodes-to-lexical-unit-substream child-nodes)))

(defun to-joined-lexical-unit-substream (node)
  (unless (eq (car node) :JOINED-LEXICAL-UNIT)
    (error 'generation-error :generation-error :nodes (list node)
						 :expected-symbol :JOINED-LEXICAL-UNIT))
  (let ((lexical-unit-nodes (cdr node)))
    (if lexical-unit-nodes
	(format nil
		"~{~A~^+~}"
		(loop for sub-lexical-units in lexical-unit-nodes
		      collect
		      (child-nodes-to-lexical-unit-substream sub-lexical-units)))
	"")))

(defun to-chunk-head-substream (node)
  (unless (eq (car node) :HEAD)
    (error 'generation-error :generation-error :nodes (list node)
					       :expected-symbol :HEAD))
  (to-sub-lu-substream (cdr node)))

(defun to-chunk-child-substream (child)
  (ecase (car child)
    (:SPACES (to-spaces-substream child))
    (:LEXICAL-UNIT (to-lexical-unit-substream child))
    (:JOINED-LEXICAL-UNIT (to-joined-lexical-unit-substream child))
    (:FORMAT (to-format-substream child))))

(defun to-chunk-children-substream (node)
  (unless (eq (car node) :CHILDREN)
    (error 'generation-error :generation-error :nodes (list node)
					       :expected-symbol :CHILDREN))
  (format nil
	  "~{~A~}"
	  (loop for child in (cdr node)
		collect
		(to-chunk-child-substream child))))

(defun to-chunk-substream (node)
  (unless (eq (car node) :CHUNK)
    (error 'generation-error :generation-error :nodes (list node)
					       :expected-symbol :CHUNK))
  (let ((head-substream (to-chunk-head-substream (assoc :HEAD (cdr node))))
	(children-substream (to-chunk-children-substream (assoc :CHILDREN (cdr node)))))
    (format nil "~A{~A}" head-substream children-substream)))

(defun to-stream (nodes)
  (format nil
	  "~{~A~}"
	  (loop for node in nodes
		collect
		(ecase (car node)
		  (:SPACES (to-spaces-substream node))
		  (:LEXICAL-UNIT (to-lexical-unit-substream node))
		  (:JOINED-LEXICAL-UNIT (to-joined-lexical-unit-substream node))
		  (:FORMAT (to-format-substream node))
		  (:CHUNK (to-chunk-substream node))))))

;; (to-stream
;;  '((:JOINED-LEXICAL-UNIT
;;     (((:FLAG) (:LING-FORM . "กาขา") (:INVARIABLE-PART) (:TAGS)
;;       (:INVARIABLE-PART)))
;;     (((:FLAG) (:LING-FORM . "กา") (:INVARIABLE-PART) (:TAGS "t1")
;;       (:INVARIABLE-PART))
;;      ((:FLAG) (:LING-FORM . "ขา") (:INVARIABLE-PART) (:TAGS "ม")
;;       (:INVARIABLE-PART))))
;;    (:SPACES . " ")
;;    (:JOINED-LEXICAL-UNIT
;;     (((:FLAG) (:LING-FORM . "กา") (:INVARIABLE-PART) (:TAGS "t1")
;;       (:INVARIABLE-PART))
;;      ((:FLAG) (:LING-FORM . "ขา") (:INVARIABLE-PART) (:TAGS "ม")
;;       (:INVARIABLE-PART))))))
;; (to-chunk-substream
;;  '(:CHUNK
;;  (:HEAD (:FLAG) (:LING-FORM . "N1") (:INVARIABLE-PART) (:TAGS "SN" "a")
;;   (:INVARIABLE-PART))
;;  (:CHILDREN
;;   (:LEXICAL-UNIT
;;    ((:FLAG) (:LING-FORM . "i") (:INVARIABLE-PART) (:TAGS) (:INVARIABLE-PART)))
;;   (:SPACES . " ") (:FORMAT . "<o>")
;;   (:LEXICAL-UNIT
;;    ((:FLAG) (:LING-FORM . "j") (:INVARIABLE-PART) (:TAGS) (:INVARIABLE-PART)))
;;   (:FORMAT . "</o>")
;;   (:LEXICAL-UNIT
;;    ((:FLAG) (:LING-FORM . "k") (:INVARIABLE-PART) (:TAGS) (:INVARIABLE-PART))))))
;; (to-joined-lexical-unit-substream
;;  '(:JOINED-LEXICAL-UNIT
;;    (((:FLAG) (:LING-FORM . "กาขา") (:INVARIABLE-PART) (:TAGS)
;;      (:INVARIABLE-PART)))
;;    (((:FLAG) (:LING-FORM . "กา") (:INVARIABLE-PART) (:TAGS "t1")
;;      (:INVARIABLE-PART))
;;     ((:FLAG) (:LING-FORM . "ขา") (:INVARIABLE-PART) (:TAGS "ม")
;;      (:INVARIABLE-PART)))))
;; (to-lexical-unit-substream '(:LEXICAL-UNIT
;; 			     ((:FLAG) (:LING-FORM . "กา") (:INVARIABLE-PART) (:TAGS "t1")
;; 			      (:INVARIABLE-PART))
;; 			     ((:FLAG) (:LING-FORM . "กา") (:INVARIABLE-PART) (:TAGS "t2")
;; 			      (:INVARIABLE-PART))
;; 			     ((:FLAG) (:LING-FORM . "กา") (:INVARIABLE-PART) (:TAGS "t3")
;; 			      (:INVARIABLE-PART))))
;; (to-sub-lu-substream '((:FLAG) (:LING-FORM . "หมาบ้าน") (:INVARIABLE-PART) (:TAGS "n" "sg") (:INVARIABLE-PART . " ตัว")))
;; (to-invariable-part-substream '(:INVARIABLE-PART . " ตัว"))
;; (to-tags-substream '(:TAGS . "n"))
;; (parse 'sub-lu "หมาบ้าน<n><sg># ตัว")
;; ((:FLAG) (:LING-FORM . "หมาบ้าน") (:INVARIABLE-PART) (:TAGS "n" "sg") (:INVARIABLE-PART " ตัว"))
;; (to-spaces-substream-stream '(:SPACES . "           "))
;; (to-format-substream '(:FORMAT . "HTML"))
;; (to-flag-substream '(:FLAG . "*"))


