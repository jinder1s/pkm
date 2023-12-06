;;; pkm2-ewoc-capture.el -*- lexical-binding: t; -*-

(defvar pkm2-buffer-capture-data-equal-plist ())

(defun pkm2--capture-verify-insert (buffer-name text read-only create-markers)
  (message "insert: %S, %S, %S, %S" buffer-name text read-only create-markers)
  (with-current-buffer buffer-name
    (let ((inhibit-read-only t)
          (from (when create-markers  (set-marker (make-marker) (point)) )))
      (insert (propertize text 'read-only read-only 'face (if read-only font-lock-warning-face font-lock-doc-face)))
      (when create-markers
        (let ((to (set-marker (make-marker) (point))))
          (set-marker-insertion-type from nil)
          (set-marker-insertion-type to t)
          (cons from to))))))

(defun pkm2--object-capture-node (asset-spec values buffer-name)
  (let* ((asset-name (plist-get asset-spec :name))
         (prompt (or (plist-get asset-spec :prompt)
                     (when (plist-get asset-spec :name)
                       (format "%s: " (plist-get asset-spec :name)))))
         (content (plist-get asset-spec :content))
         (dont-ask-for-input (plist-get asset-spec :no-input))
         (db-id (plist-get asset-spec :db-id))
         (content (progn
                    (when buffer-name (pkm2--capture-verify-insert buffer-name (format  "%s: " asset-name) t nil))
                    (cond (db-id
                           (message "node: db-id")
                           "" )
                          ((assoc-default asset-name values)
                           (message "values: %S" (assoc-default asset-name values))
                           (assoc-default asset-name values) )
                          (content
                           (message "content: %S" content)
                           content)
                          (dont-ask-for-input
                           (message "No input")
                           "")
                          (t
                           (message "Should ask for string")
                           (read-string prompt)))))
         (from-to (when buffer-name (pkm2--capture-verify-insert buffer-name
                                                                 (if db-id
                                                                     (propertize (format "db-id: %d" db-id) :db-id db-id)
                                                                   content)
                                                                 nil
                                                                 t)))
         (from (car from-to))
         (to (cdr from-to)))
    (list :asset-schema asset-spec :asset-capture-info (list :content content :db-id db-id :from from :to to))))

(defun pkm2--object-capture-kvd (asset-spec values buffer-name)
  (let* ((asset-name (plist-get asset-spec :name))
         (prompt (or (plist-get asset-spec :prompt)
                     (when (plist-get asset-spec :name)
                       (format "%s: " (plist-get asset-spec :name)))
                     (when (plist-get asset-spec :key)
                       (format "%s: " (plist-get asset-spec :key)) )))
         (type (or (plist-get asset-spec :data-type) 'TEXT))
         (key (plist-get asset-spec :key))
         (choices (plist-get asset-spec :choices))
         (value (progn
                  (when buffer-name (pkm2--capture-verify-insert buffer-name prompt t nil))
                                        ; Insert promt so that you see it on buffer as mini-buffer is asking you for things.
                  (--> (plist-get asset-spec :value) (cond
                                                      ((assoc-default asset-name values) (assoc-default asset-name values))
                                                      ((functionp it) (funcall it))
                                                      ((and it (not (listp it))) it)
                                                      ((progn (message "Choces: %S" choices)  choices)
                                                       (cond ((eq type 'TEXT) (completing-read prompt choices))
                                                             ((or  (eq type 'INTEGER) (eq type 'DATETIME) ) (string-to-number (completing-read prompt choices)))
                                                             ((eq type 'REAL) (string-to-number (completing-read prompt choices)))
                                                             ((eq type 'BLOB) (progn (display-warning 'pkm-object "BLOB type kvd inputs are not yet implemented")
                                                                                     (error "Tried to insert blob kvd, Not yet implemented.")))
                                                             (t (message "Choices type not defined"))))
                                                      (t (cond ((eq type 'TEXT) (read-string prompt))
                                                               ((eq type 'INTEGER) (read-number prompt))
                                                               ((eq type 'REAL) (read-number prompt))
                                                               ((eq type 'DATETIME) (pkm2-get-user-selected-timestamp prompt))
                                                               ((eq type 'BLOB) (progn (display-warning 'pkm-object "BLOB type kvd inputs are not yet implemented")
                                                                                       (error "Tried to insert blob kvd. Not yet implemented."))))))) ))
         (from-to (when buffer-name
                    (message "value: %S, type: %S, output: %s" value type (pkm2--convert-object-to-string value type))
                    (pkm2--capture-verify-insert
                     buffer-name
                     (pkm2--convert-object-to-string value type)
                     nil
                     t)))
         (from (car from-to))
         (to (cdr from-to)))
    (list :asset-schema asset-spec :asset-capture-info (list :key key :value value :from from :to to))))


(defun pkm2--object-capture-object-verify (structure-name-or-schema &optional external-buffer-name skip-verify values is-sub)
  (let* ((structure-schema (if (plistp structure-name-or-schema)
                               structure-name-or-schema
                             (pkm--object-define structure-name-or-schema)))
         (structure-name (if (plistp structure-name-or-schema)
                             (plist-get structure-name-or-schema :name)
                           structure-name-or-schema))
         (object-definition-is-valid (pkm--object-validate-structure-schema structure-schema))
         (asset-specs (plist-get structure-schema :assets))
         (structure-names (doom-plist-keys pkm-structure-required-kvds-plist))
         (buffer-name (unless skip-verify
                        (message "sv: %S, creating buffer name" skip-verify)
                        (or external-buffer-name (format "*PKM-create-%s-%s*" (symbol-name structure-name) (format-time-string "%H%M%S")) )))
         (buffer (unless skip-verify
                   (message "sv: %S, get-or-create-buffer: %S" skip-verify buffer-name)
                   (get-buffer-create buffer-name)))
         (captured-assets (progn
                            (unless object-definition-is-valid (error "Object defition is not valid."))
                            (unless (or external-buffer-name skip-verify)
                              (message "sv: %S, e-b-n:%S, or: %S, setting up buffer" skip-verify external-buffer-name (or external-buffer-name skip-verify))

                              (with-current-buffer buffer-name
                                (switch-to-buffer buffer)
                                (local-set-key (kbd "C-c C-c") #'pkm--object-ewoc-capture-finializer)
                                (local-set-key (kbd "C-c C-k") (lambda ()
                                                                 (interactive)
                                                                 (kill-buffer buffer-name)))
                                (pkm2--capture-verify-insert buffer-name "Press C-c C-c to save and C-c C-k to cancel.\n" t nil)
                                (add-hook 'kill-buffer-hook #'pkm2-kill-buffer-hook nil t)))
                            (unless skip-verify
                              (pkm2--capture-verify-insert buffer-name (format  "Capturing %s.\n" (symbol-name structure-name)) t nil))
                            (-map (lambda (asset-spec)
                                    (message "asset-spec: %S" asset-spec)
                                    (let* ((asset-name (plist-get asset-spec :name))
                                           (prompt (or (plist-get asset-spec :prompt)
                                                       (when (plist-get asset-spec :name)
                                                         (format "%s: " (plist-get asset-spec :name)))
                                                       (when (plist-get asset-spec :key)
                                                         (format "%s: " (plist-get asset-spec :key)) )))

                                           (is-optional (plist-get asset-spec :optional))
                                           (is-managed (plist-get asset-spec :managed))
                                           (create-optional (when (or is-managed is-optional) (y-or-n-p (format "Create optional or managed asset with promp: %s" prompt)))))
                                      (if (or (not (or is-optional is-managed)) create-optional)
                                          (pcase (plist-get asset-spec :pkm-type)
                                            ('node (pkm2--object-capture-node asset-spec values buffer-name)  )
                                            ('kvd (pkm2--object-capture-kvd asset-spec values buffer-name))
                                            ((pred (lambda (pkm-type) (member pkm-type structure-names)))
                                             (list :asset-schema asset-spec
                                                   :asset-capture-info (pkm2--object-capture-object-verify
                                                                        (plist-get asset-spec :pkm-type)
                                                                        buffer-name
                                                                        nil
                                                                        (assoc-default asset-name values)
                                                                        t))))
                                        (list :asset-schema asset-spec :asset-capture-info nil))))
                                  asset-specs)))
         (output (list :pkm-type structure-name :schema structure-schema :capture-info captured-assets)))
    (if (and skip-verify (not is-sub))
        (pkm--object-capture-finializer structure-schema captured-assets)
      (unless (or is-sub external-buffer-name)
        (setq pkm2-buffer-capture-data-equal-plist (plist-put pkm2-buffer-capture-data-equal-plist buffer-name output #'equal))))
    output))

(defun pkm2-kill-buffer-hook ()
  (--> (buffer-name)
       (setq pkm2-buffer-capture-data-equal-plist
             (plist-put pkm2-buffer-capture-data-equal-plist it nil #'equal))))

(defun pkm--object-ewoc-capture-finializer ()
  (interactive)
  (--> (buffer-name)
       (plist-get pkm2-buffer-capture-data-equal-plist it #'equal)
       (progn
         (kill-buffer)
         (message "schema: %S" (plist-get it :schema))
         it)
       (pkm--object-capture-finializer (plist-get it :schema) (plist-get it :capture-info))))

(provide 'pkm2-ewoc-capture)
