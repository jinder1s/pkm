;;; pkm-object-capture.el -*- lexical-binding: t; -*-

(defun pkm--object-add-node-to-db (s-node)
  (when (plist-get s-node :asset-capture-info)
    (let* ((temp (plist-get s-node :asset-capture-info))
           (content (plist-get temp :content))
           (db-id (plist-get temp :db-id))
           (timestamp (pkm2-get-current-timestamp))
           (db-node (unless db-id
                      (pkm2--db-insert-node content timestamp))))
      (message "Newly inserted node: %S, id: %S" db-node (when db-node (pkm2-db-node-id db-node) ))
      (if (or db-node db-id)
          (list
           :asset-schema (plist-get s-node :asset-schema)
           :asset-capture-info temp
           :db-node db-node
           :db-id (or db-id (pkm2-db-node-id db-node) ))
        (progn (display-warning 'pkm-object "Did not succed in making node %S" s-node)
               (error "Did not succeed at creating node: %S" s-node)) )) ))

(defun pkm--object-add-kvd-to-db (s-kvd)
  (when (plist-get s-kvd :asset-capture-info)
    (if-let* ((asset-schema (plist-get s-kvd :asset-schema) )
              (type (or (plist-get asset-schema :data-type) 'TEXT))
              (temp (plist-get s-kvd :asset-capture-info))
              (key (plist-get temp :key))
              (value (plist-get temp :value))
              (db-kvd (or  (pkm2--db-get-or-insert-kvd key value type) )))
        (if db-kvd
            (list
             :asset-schema asset-schema
             :asset-capture-info temp
             :db-kvd  db-kvd
             :db-id (pkm2-db-kvd-id db-kvd))
          (progn (display-warning 'pkm-object "Did not succed in making kvd: %S" s-kvd)
                 (error "Did not succed in creating kvd: %S" s-kvd)))) ))

(setq pkm--object-capture-alist `((node ,#'pkm--object-capture-node)
                                    (kvd ,#'pkm--object-capture-kvd)))

(setq pkm--object-add-to-db-alist `((node ,#'pkm--object-add-node-to-db)
                                    (kvd ,#'pkm--object-add-kvd-to-db)))

(defun pkm--object-capture ()
  "if NUMERIC-PREFIX-ARGUMENT = 0: Assume point is on pkm-browse and is on the node that should be parent of to-be-captured node. Essentially, creating a sub node.
TODO TEST!"
  (interactive)
  (let* ((structure-name (-->  (doom-plist-keys pkm-structure-undefined-schemas-plist)
                               (completing-read "What type of object would you like to create?" it)
                               (intern it)))
         (structure-schema (pkm--object-define structure-name)))
    (pkm2--object-capture-object-verify structure-schema)))


(defun pkm--object-capture-sub (parent-node-db-id &optional structure-name link-label)
  (interactive)
  (let* ((structure-name (or structure-name
                             (-->  (doom-plist-keys pkm-structure-undefined-schemas-plist)
                                   (completing-read "What type of object would you like to create?" it)
                                   (intern it)) ))
         (link-type-between-objects (unless link-label
                                      (--> (completing-read "What type of link do you want to make?" pkm-links-types)
                                           (if (equal it (symbol-name 'HIERARCHICAL))
                                               it
                                             (error "Only Hierarchical types links are implemented right now."))
                                           (intern it ) ) ))
         (link-label  (or link-label (completing-read "What is the label for this link?" (plist-get pkm-links-type-to-label-eq-plist link-type-between-objects)) ))
         (link-definition (list :pkm-link-label link-label
                                    :parent 'parent
                                    :child 'child))
         (schema (list :name 'parent-child-node
                       :assets (list
                                `(:pkm-type node :name "parent-node" :parent-node t :db-id ,parent-node-db-id)
                                `(:pkm-type ,structure-name :name "child-node" :child-node t))
                       :links (list link-definition))))
    (pkm2--object-capture-object-verify schema)))

(defun pkm--object-capture-finializer (structure-schema created-assets &optional within-transaction)
  "Start sqlite transaction.
Filter nodes and kvds.
Commit nodes to database.
Commit kvds and their links to nodes to database.
Figure out links between kvds and assets.
Commit links between kvds and assets.
Instantiate links.
Commit links to database.
Commit everything.
"
  (setq jinder-final-schema structure-schema)
  (setq jinder-final-assets created-assets)
  (unless within-transaction
    (sqlite-transaction jinder_dbh) )
  (condition-case-unless-debug err
      (let* ((pkm-structure-types (doom-plist-keys pkm-structure-undefined-schemas-plist))
             (assets-by-group (-group-by (lambda (asset)
                                           (--> (plist-get asset :asset-schema)
                                                (plist-get it :pkm-type)
                                                (if (or (equal it 'node) (equal it 'kvd))
                                                    it
                                                  (if (member it pkm-structure-types)
                                                      'pkm-type
                                                    (progn
                                                      (error  (format "There is an unknown type amoung the assets, it is currently not being handled: %S" asset))
                                                      'unknown )))))
                                         created-assets))
             (uncommitted-pkm-structures (assoc-default 'pkm-type assets-by-group))
             (committed-pkm-structures-with-db-ids (-map (lambda (capture-info-structure)
                                                           (let* ((asset-level-schema (plist-get capture-info-structure :asset-schema))
                                                                  (asset-level-capture-info (plist-get capture-info-structure :asset-capture-info))
                                                                  (structure-level-schema (plist-get asset-level-capture-info :schema))
                                                                  (structure-level-assets (plist-get asset-level-capture-info :capture-info))
                                                                  (commited-structure (pkm--object-capture-finializer structure-level-schema structure-level-assets t)))
                                                             (list
                                                              :asset-schema asset-level-schema
                                                              :asset-capture-info asset-level-capture-info
                                                              :db-structure commited-structure
                                                              :db-id (plist-get commited-structure :db-id))))
                                                         uncommitted-pkm-structures))
             ;; TODO NEXT handle none-node and none-kvd assets
             (uncommited-nodes (assoc-default 'node assets-by-group))
             (commited-nodes-with-db-ids (-map #'pkm--object-add-node-to-db uncommited-nodes))
             (commited-nodes-and-structures-with-db-ids (-concat commited-nodes-with-db-ids committed-pkm-structures-with-db-ids))
             (uncommited-kvds (assoc-default 'kvd assets-by-group))
             (commited-kvds-with-db-ids  (-map #'pkm--object-add-kvd-to-db uncommited-kvds))
             ;; for each node specifier, get node-db-id and create link between kvd and node-db-id
             (commited-kvds-with-db-ids-and-links
              (-map (lambda (kvd-info)
                      (let* ((kvd-asset-schema (plist-get kvd-info :asset-schema))
                             (kvd-db-id (plist-get kvd-info :db-id))
                             (kvd-type (plist-get kvd-asset-schema :data-type))
                             (node-specifiers (plist-get kvd-asset-schema :link-to))
                             (links (-map (lambda (node-specifier)
                                            (let* ((node-db-ids
                                                    (pkm--object-get-specified-node-db-ids
                                                     node-specifier
                                                     commited-nodes-and-structures-with-db-ids)))
                                              (-map (lambda (node-db-id) (list
                                                                          :node-db-id node-db-id
                                                                          :kvd-db-id kvd-db-id
                                                                          :db-kvd-link (pkm2--db-insert-link-between-node-and-kvd
                                                                                        node-db-id
                                                                                        kvd-db-id
                                                                                        (pkm2-get-current-timestamp)
                                                                                        kvd-type)))
                                                    node-db-ids)))
                                          node-specifiers))
                             (output (-copy kvd-info)))
                        (plist-put output :links links)))
                    commited-kvds-with-db-ids))
             (links (--> (plist-get structure-schema :links) (-map (lambda (link)
                                                                     (when (not (equal (plist-get pkm-links-label-to-type-equal-plist (plist-get link :pkm-link-label) #'equal) 'HIERARCHICAL) )
                                                                       (error "Only hierarchical link-labels have been implemented in link capture funcs."))
                                                                     link)
                                                                   it)))

             (commited-links (-map (lambda (link)
                                     (list :link-schema link
                                           :db-links (let* ((parent-node-db-ids (pkm--object-get-specified-node-db-ids
                                                                                 (plist-get link :parent)
                                                                                 commited-nodes-and-structures-with-db-ids))
                                                            (child-node-db-ids (pkm--object-get-specified-node-db-ids
                                                                                (plist-get link :child)
                                                                                commited-nodes-and-structures-with-db-ids))
                                                            (link-type  (plist-get link :pkm-link-label)))
                                                       (message "parents: %S\n children: %S" parent-node-db-ids child-node-db-ids)
                                                       (-flatten
                                                        (-map (lambda (parent-db-id)
                                                                (-map (lambda (child-db-id)
                                                                        (list
                                                                         :parent parent-db-id
                                                                         :child child-db-id
                                                                         :pkm-link-label link-type
                                                                         :db-nodes-link (pkm2--db-insert-link-between-parent-and-child
                                                                                         link-type
                                                                                         parent-db-id
                                                                                         child-db-id
                                                                                         (pkm2-get-current-timestamp))))
                                                                      child-node-db-ids))
                                                              parent-node-db-ids)))))
                                   links))
             (combined-assets (-concat commited-nodes-and-structures-with-db-ids commited-kvds-with-db-ids-and-links))
             (output (list :schema structure-schema
                           :assets combined-assets
                           :db-id (car (pkm--object-get-specified-node-db-ids 'primary combined-assets) )
                           :links commited-links)))
        (if within-transaction
            output
          (sqlite-commit jinder_dbh)
          ;; (sqlite-rollback jinder_dbh)
          ))
    (error (message "Received Error %s" (error-message-string err))
           (sqlite-rollback jinder_dbh))))


(defun pkm--object-get-specified-node-db-ids (node-specifier assets-infos)
  (message "ns: %S, %S" node-specifier assets-infos)
  (cond ((plistp node-specifier)
         (list  (plist-get node-specifier :db-id)))
        ((-flatten
           (--> (-filter (lambda (asset-info)
                           (pkm--object-does-specifier-match
                            node-specifier
                            (plist-get asset-info :asset-schema))) assets-infos)
                (progn (message "after filter: %S" it)
                       it)
                (-map (lambda (asset-info) (plist-get asset-info :db-id)) it))) )))

(provide 'pkm-object-capture)
