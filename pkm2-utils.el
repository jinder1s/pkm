;;; pkm2-utils.el -*- lexical-binding: t; -*-

(require 'ts)
(require 'dash)

(defun pkm2--db-sanatize-text (input)
  "Single-quote (scalar) input for use in a SQL expression."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (while (re-search-forward "'" nil t)
      (replace-match "''"))
    (format "'%s'" (buffer-string))))


(defun pkm2--db-format-value-correctly (value &optional type)
  (cond ((or (not type) (eq 'TEXT type)) (pkm2--db-sanatize-text value ) )
        ((or (eq 'INTEGER type) (eq 'DATETIME type) ) (format "%d" value) )
        ((eq 'REAL type) (format "%d" value) )))


(defun pkm2-get-current-timestamp ()
  (truncate (ts-unix (ts-now))))

(defun pkm2--db-convert-object-to-string (object)
  (cond ((stringp object) (pkm2--db-sanatize-text object))
        ((numberp object) (format "%d" object))
        ((symbolp object) (pkm2--db-sanatize-text (symbol-name object)))))

(defun pkm2--convert-object-to-string (object &optional type)
  (cond ((stringp object) object)
        ((and (equal type 'DATETIME) (numberp object))
         (format-time-string "%Y-%m-%d %H:%M:%S %z" object))
        ((numberp object) (format "%d" object))
        (t (error (format "Could not convert object: %S, %S" object type)))))


(defun pkm2-get-user-selected-timestamp (&optional prompt default-time)
  (--> (read-string (or prompt "Timestamp: ") (format-time-string "%FT%T %z" (or default-time (current-time)) ))
       (date-to-time it)
       (time-convert it 'integer)))

(defun doom-plist-keys (plist)
  "Return the keys in PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    keys))
(defun mm-alist-to-plist (alist)
  "Convert association list ALIST into the equivalent property-list form.
The plist is returned.  This converts from

\((a . 1) (b . 2) (c . 3))

into

\(a 1 b 2 c 3)

The original alist is not modified."
  (let (plist)
    (while alist
      (let ((el (car alist)))
	(setq plist (cons (cdr el) (cons (car el) plist))))
      (setq alist (cdr alist)))
    (nreverse plist)))

(provide 'pkm2-utils)
