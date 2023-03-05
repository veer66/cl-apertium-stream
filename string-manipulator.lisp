(in-package #:cl-apertium-stream)

(defvar LING-FORM-ESCAPE-PAT "([\\^\\$@\\*\\\\<>{}/\\]\\[])")

(defun escape-ling-form (text)
  (regex-replace-all LING-FORM-ESCAPE-PAT
		     text
		     "\\\\\\1"
		     :preserve-case t))

(defun unescape (escaped-text)
  (loop with out = '()
        with going-to-unescape-p = nil
        for ch across escaped-text
        do
           (cond
             (going-to-unescape-p (progn (push ch out)
                                         (setq going-to-unescape-p nil)))
             ((eq #\\ ch) (setq going-to-unescape-p t))
             (t (push ch out)))
        finally
           (return (coerce (reverse out) 'string))))
