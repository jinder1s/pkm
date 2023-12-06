;;; pkm/pkm2-log.el -*- lexical-binding: t; -*-



(defun pkm2--update-node-with-log (pkm-node db-id new-content timestamp)
  (message "In update log node")
  (let* ((node-types (pkm2-node-types pkm-node))
         (log-node (-any (lambda (type)
                           (--> (plist-get pkm-structure-defined-schemas-plist type)
                                (plist-get it :log-primary)))
                         node-types))
         (old-content (when log-node (--> (pkm2-node-db-node pkm-node) (pkm2-db-node-content it))))
         (content-type (when log-node
                         (if (stringp old-content)
                             'TEXT
                           (error "Haven't figure out how to do history for node content that is not text."))))
         (history-kvd (when (and log-node old-content)
                        (pkm2--db-get-or-insert-kvd "history"
                                             old-content
                                             content-type)))
         (db-id (or db-id (--> (pkm2-node-db-node pkm-node) (pkm2-db-node-id it))))
         (new-db-node (pkm2--db-update-node db-id  new-content timestamp)))
    (when (and log-node history-kvd)
      (pkm2--db-insert-link-between-node-and-kvd db-id (pkm2-db-kvd-id history-kvd) timestamp content-type nil t))
    new-db-node))

(defun pkm2--update-node-kvd (pkm-node old-kvd new-kvd-id kvd-type context-id old-link-id timestamp)
  (let* ((node-types (pkm2-node-types pkm-node))
         (kvd-key (pkm2-db-kvd-key old-kvd))
         (key-kvd-specs (-non-nil
                         (-map (lambda (structure-name)
                                 (pkm-object-kvd-get-kvd-spec-for-key-in-structure kvd-key structure-name))
                               node-types)))
         (log-changes (-any (lambda (spec)
                              (plist-get spec :log-change))
                            key-kvd-specs))
         (log-changes-ask-timestamp (-any (lambda (spec)
                              (plist-get spec :log-ask-timestamp))
                            key-kvd-specs))
         (timestamp (if log-changes-ask-timestamp
                        (pkm2-get-user-selected-timestamp)
                        timestamp))
         (node-id (--> (pkm2-node-db-node pkm-node) (pkm2-db-node-id it)))
         (new-link  (when new-kvd-id
                      (pkm2--db-insert-link-between-node-and-kvd node-id new-kvd-id timestamp kvd-type context-id))))
    (if log-changes
        (pkm2-db-archive-link-between-node-and-kvd old-link-id kvd-type)
      (pkm2--db-delete-link-between-node-and-kvd old-link-id kvd-type))
    new-link))

(provide 'pkm2-log)
