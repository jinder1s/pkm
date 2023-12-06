;;; pkm2-utils.el -*- lexical-binding: t; -*-

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
