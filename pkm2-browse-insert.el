;;; pblahrt>rt>rt>-*- lexical-binding: t; -*-



(defun pkm2--browse-format-insert (input browse-node)
  "Single-quote (scalar) input for use in a SQL expression."
  (with-temp-buffer
      (insert input)
      (goto-char (point-min))
      (while (re-search-forward "<insert>\\(.*?\\)</insert>" nil t)
        (let* ((current-match (match-string 1))
               (spec (read current-match))
               (replacement-string (pkm2--browse-format-get-string spec browse-node)))
          (replace-match replacement-string)))
      (format "%s" (buffer-string))))


(defun pkm2--browse-format-get-string (spec browse-node)
  ""
  (let* ((pkm-node (pkm2-browse-node-pkm-node browse-node))
         (state (pkm2-browse-node-state browse-node))
         (show-hidden (pkm2-browse-node-state-show-hidden state))
         (types (pkm2-node-types pkm-node))
         (types-definitions (-map (lambda (type)
                                    (plist-get pkm-structure-defined-schemas-plist type))
                                  types))
         (type-parents (-map (lambda (type-definition)
                               (plist-get type-definition :parents)) types-definitions))
         (highest-types (--> (-map-indexed (lambda (index parents)
                                             (unless (member (nth index types) parents) t))
                                           type-parents)
                             (-map-indexed (lambda (index is-not-parent)
                                             (when is-not-parent
                                               (nth index types))) it )
                             (-non-nil it)))
         (output-string (cond ((equal 'kvd-value (plist-get spec :display))
                               (let* ((key  (plist-get spec :key))
                                      (key-kvd-specs (-non-nil
                                                      (-map (lambda (structure-name)
                                                              (pkm-object-kvd-get-kvd-spec-for-key-in-structure key structure-name))
                                                            highest-types) ))
                                      (value-types (-map (lambda (spec)
                                                           (plist-get spec :data-type))
                                                         key-kvd-specs))
                                      (node-kvds (pkm2-node-get-kvds-with-key pkm-node key))
                                      (values (-map (lambda (kvd)
                                                      (pkm2-db-kvd-value kvd) )
                                                    node-kvds))
                                      (face-value (plist-get spec :face-value))
                                      (face-value (when face-value (symbol-value face-value)))
                                      (values-strings (-map-indexed
                                                       (lambda (index value)
                                                         (let* ((o-s (propertize
                                                                      (pkm2--convert-object-to-string value (when
                                                                                                                (member 'DATETIME value-types)
                                                                                                              'DATETIME))
                                                                      :db-kvd (nth index node-kvds)) )
                                                                (face (when (stringp value) (plist-get face-value (intern value)) )))
                                                           (if face
                                                               (propertize o-s 'face face)
                                                             o-s)))
                                                       values)))
                                 (string-join values-strings " ")))

                              ((equal 'content (plist-get spec :display))
                               (-->  (pkm2-node-db-node pkm-node) (propertize (pkm2-db-node-content it)
                                                                              :db-node it)))
                              ((equal 'hidden (plist-get spec :display))
                               (if show-hidden
                                   (concat (plist-get spec :prefix) (pkm2-browse--default-hidden-info-string browse-node) )
                                 "")))))
    (when (plist-get spec :face)
      (setq output-string (propertize output-string 'face (plist-get spec :face))))
    output-string ))
