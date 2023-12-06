;;; pkm/pkm2-browse-interactivity.el -*- lexical-binding: t; -*-

(defun pkm2--browse-toggle-hidden-info-at-point ()
  (interactive)
  (save-excursion
    (let* ((browse-node (get-text-property (point) :pkm2-browse-node))
           (state (pkm2-browse-node-state browse-node))
           (show-hidden (pkm2-browse-node-state-show-hidden state))
           (new-show-hidden (not show-hidden)))
      (setf (pkm2-browse-node-state-show-hidden state) new-show-hidden)
      (pkm2-browse--refresh-insert-browse-node browse-node)) ))

(defun pkm2--browse-edit-object-at-point ()
  (interactive)
  (let* ((current-point (point))
         (browse-node (get-text-property current-point :pkm2-browse-node ))
         (pkm-node (pkm2-browse-node-pkm-node browse-node))
         (db-node (get-text-property current-point :db-node))
         (kvd (get-text-property current-point :db-kvd)))
    (message "b-n: %S\n p-n: %S" browse-node pkm-node)
    (message "db_node: %S, kvd: %S" db-node kvd)
    (if  (not (xor db-node kvd) )
        (error "there should be one and only one type of object at point: either db-node or kvd.")
      (if db-node
          (let* ((old-content (pkm2-db-node-content db-node))
                 (new-content (cond ((numberp old-content) (read-number "New value: " old-content))
                                    ((stringp old-content) (read-string "New value: " old-content))))
                 (id (pkm2-db-node-id db-node))
                 (new-db-node (pkm2--update-node-with-log pkm-node id new-content (pkm2-get-current-timestamp))))
            (setf (pkm2-node-db-node pkm-node) new-db-node))
        (let* ((kvds (pkm2-node-kvds pkm-node))
               (key (pkm2-db-kvd-key kvd))
               (old-kvd-id (pkm2-db-kvd-id kvd))
               (type (assoc-default key (-flatten (-map (lambda (type)
                                           (-map (lambda (key)
                                                   (cons key type))
                                                 (plist-get pkm-data-type-to-kvd-key-plist type)))
                                         (doom-plist-keys pkm-data-type-to-kvd-key-plist ))) ))

               (new-value (cond
                           ((equal type 'DATETIME) (pkm2-get-user-selected-timestamp "What time" (pkm2-db-kvd-value kvd)))
                           (t (--> (pkm2--db-query-get-all-values-with-key key type)
                                   (-map #'pkm2--browse-convert-object-to-string it)
                                   (completing-read "What value?" it nil 'confirm)
                                   (if (or (eq type 'REAL) (eq type 'INTEGER))
                                       (string-to-number  it)
                                     it)))))
               (context-id (pkm2-db-kvd-context-id kvd))
               (new-kvd (pkm2--db-get-or-insert-kvd key new-value type))
               (new-kvd-id (pkm2-db-kvd-id new-kvd))
               (old-link-id (pkm2-db-kvd-link-id kvd))
               (new-link (pkm2--update-node-kvd pkm-node kvd new-kvd-id type context-id old-link-id (pkm2-get-current-timestamp))))
          (setf (pkm2-db-kvd-link-id new-kvd) (pkm2-db-kvd-link-id2 new-link))
          (setf (pkm2-db-kvd-context-id new-kvd) context-id)
          (setf (pkm2-node-kvds pkm-node) (-map (lambda (temp-kvd)

                                                  (if (equal (pkm2-db-kvd-id temp-kvd) (pkm2-db-kvd-id kvd))
                                                      new-kvd ; Replace modified kvd with new kvd
                                                    temp-kvd)) kvds))
          new-kvd))
      ;; browse-node should have been modified by values above
      (pkm2-browse--refresh-insert-browse-node browse-node))))



(defun pkm2--browse-see-children-at-point ()
  (interactive)
  (pkm2--browse-see-children (get-text-property (point) :pkm2-browse-node-id)))


(defun pkm2--browse-see-children (browse-id &optional db-id level-children)
  (when (and browse-id db-id) (error "Specified both browse-id and db-id, only one should be specified."))
  (when (not (or browse-id db-id) ) (error "Did not specify either browse-id or db-id, at least one is needed."))
  (let* ((browse-id (if db-id
                        (pkm2--browse-get-browse-id-of-node-with-db-id db-id)
                      browse-id))
         (browse-node (assoc-default browse-id pkm2-browse--browse-nodes-alist))
         (state (pkm2-browse-node-state browse-node))
         (from (pkm2-browse-node-state-from state) )
         (base-level (pkm2-browse-node-state-level state))
         (section (pkm2-browse-node-state-section state))
         (db-id (--> (pkm2-browse-node-pkm-node browse-node) (pkm2-node-db-node it) (pkm2-db-node-id it)))
         (level-children (or level-children (read (completing-read "How many levels of children would you like?" `("1" "2" "3" "4" "5" "6" "7" "ALL")) ) ))
         (query-spec `((:or db-node (:db-id ,db-id )) (:convert-or convert-to-children (:levels ,level-children) )) )
         (nodes-ids (pkm2--browse-get-query-nodes query-spec))
         (browse-nodes
          (-map (lambda (id)
                  (make-pkm2-browse-node :pkm-node (pkm2--db-query-get-node-with-id id) :state (make-pkm2-browse-node-state :section section))) nodes-ids) )
         (nodes-in-hierarchy  (pkm2-browse--organize-into-hierarchies browse-nodes)))
    (-each browse-nodes
      (lambda (b-n)
        (--> (pkm2-browse-node-state b-n)
             (setf (pkm2-browse-node-state-level it) (+ (pkm2-browse-node-state-level it) base-level)))))
    (setf (pkm2-browse-section-state section) (-concat (pkm2-browse-section-state section) (list `(:action show-children :node-db-id ,db-id :level-children ,level-children)) ))
    (goto-char from)
    (pkm2--browse-remove-node browse-id)
    (setq buffer-read-only nil)
    (-each nodes-in-hierarchy #'pkm2-browse--insert-browse-node)
    (setq buffer-read-only t)))


(defun pkm2--browse-see-parents (browse-id &optional db-id level-parents)
  "Very hacky and not very well done. Needs to be fixed."
  (when (and browse-id db-id) (error "Specified both browse-id and db-id, only one should be specified."))
  (when (not (or browse-id db-id) ) (error "Did not specify either browse-id or db-id, at least one is needed."))
  (let* (
         (browse-id (if db-id
                        (pkm2--browse-get-browse-id-of-node-with-db-id db-id)
                      browse-id))
         (browse-node (assoc-default browse-id pkm2-browse--browse-nodes-alist))
         (state (pkm2-browse-node-state browse-node))
         (from (pkm2-browse-node-state-from state) )
         (base-level (pkm2-browse-node-state-level state))
         (section (pkm2-browse-node-state-section state))
         (db-id (--> (pkm2-browse-node-pkm-node browse-node) (pkm2-node-db-node it) (pkm2-db-node-id it)))
         (level-parents (or level-parents (read (completing-read "How many levels of parents would you like?" `("1" "2" "3" "4" "5" "6" "7" "ALL")) ) ))
         (query (pkm2--db-query-get-parent-nodes level-parents '("sub") (format "%s" db-id) t))
         (query-results (sqlite-select pkm2-database-connection query))
         (browse-nodes
          (-map (lambda (result)
                  (message "result: %S" result)
                  (make-pkm2-browse-node :pkm-node (pkm2--db-query-get-node-with-id (car result))
                                         :state (make-pkm2-browse-node-state :section section :level (if (cadr result) (+ 1 base-level) base-level))
                                         :parent (cadr result)))
                (-concat (list `(,db-id nil)) query-results))))
    (--> (-map (lambda (b-n) (-as-> b-n b-it (pkm2-browse-node-pkm-node b-it) (pkm2-node-db-node b-it) (pkm2-db-node-id b-it))) browse-nodes)
         (when (not (equal (length it ) (length (-distinct it) )))
           (error "At least 2 browse nodes have same db-id. Something unexpected has happend.")))
    (-each browse-nodes (lambda (b-n) (-as-> (pkm2-browse-node-pkm-node b-n)
                                             it2
                                             (pkm2-node-db-node it2)
                                             (pkm2-db-node-id it2)
                                             (-filter (lambda (b-n2) (equal it2  (pkm2-browse-node-parent b-n2))) browse-nodes )
                                             (setf (pkm2-browse-node-parents b-n) it2))))

    (setf (pkm2-browse-section-state section) (-concat (pkm2-browse-section-state section) (list `(:action show-parents :node-db-id ,db-id :level-parents ,level-parents)) ))
    (goto-char from)
    (pkm2--browse-remove-node browse-id)
    (setq buffer-read-only nil)
    (pkm2-browse--insert-browse-node (car browse-nodes))
    (setq buffer-read-only t)))


(defun pkm2--browse-see-parents-at-point ()
  (interactive)
  (pkm2--browse-see-parents (get-text-property (point) :pkm2-browse-node-id)))


(defun pkm2--browse-add-kvd-at-point ()
  (interactive)
  (let* ((current-point (point))
         (browse-node (get-text-property current-point :pkm2-browse-node ))
         (pkm-node (pkm2-browse-node-pkm-node browse-node))
         (db-node (pkm2-node-db-node pkm-node))
         (node-id (pkm2-db-node-id db-node))
         (potential-keys (-flatten (-map (lambda (type)
                                           (-map (lambda (key)
                                                   (cons key type))
                                                 (plist-get pkm-data-type-to-kvd-key-plist type)))
                                         (doom-plist-keys pkm-data-type-to-kvd-key-plist ))))
         (key (completing-read "What key would you like to add?" potential-keys nil 'confirm))
         (type (assoc-default key potential-keys))
         (value (-->
                 (pkm2--db-query-get-all-values-with-key key type)
                 (-map #'pkm2--browse-convert-object-to-string it)
                 (completing-read "What value?" it nil 'confirm)
                 (if (or (eq type 'REAL) (eq type 'INTEGER))
                     (string-to-number  it)
                   it)))
         (new-kvd (pkm2--db-get-or-insert-kvd key value type))
         (new-kvd-id (pkm2-db-kvd-id new-kvd))
         (new-link (pkm2--db-insert-link-between-node-and-kvd node-id new-kvd-id (pkm2-get-current-timestamp) type)))
    (setf (pkm2-db-kvd-link-id new-kvd) (pkm2-db-kvd-link-id2 new-link))
    (setf (pkm2-node-kvds pkm-node) (-concat (pkm2-node-kvds pkm-node) (list new-kvd)))))


(defun pkm2--browse-delete-link-to-kvd-at-point ()
  (interactive)
  (let* ((current-point (point))
         (browse-node (get-text-property current-point :pkm2-browse-node ))
         (pkm-node (pkm2-browse-node-pkm-node browse-node))
         (kvd (get-text-property current-point :db-kvd))
         (type (pkm2-db-kvd-type kvd))
         (link-id (pkm2-db-kvd-link-id kvd))
         (kvds (pkm2-node-kvds pkm-node)))
    (pkm2--update-node-kvd pkm-node kvd nil type nil link-id)
    (setf (pkm2-node-kvds pkm-node) (-filter (lambda (temp-kvd)
                                               (if (equal (pkm2-db-kvd-id temp-kvd) (pkm2-db-kvd-id kvd))
                                                   nil ; delete this kvd from node
                                                 t))
                                             kvds))
    (pkm2-browse--refresh-insert-browse-node browse-node)))
(defun pkm2--browse-delete-link-to-parent-of-node-at-point ()
  (let* ((current-point (point))
         (browse-node (get-text-property current-point :pkm2-browse-node ))
         (pkm-node (pkm2-browse-node-pkm-node browse-node))
         (db-id (--> (pkm2-node-db-node pkm-node) (pkm2-db-node-id it)))
         (parent-browse-node (pkm2-browse-node-parent browse-node))
         (parent-pkm-node (pkm2-browse-node-pkm-node parent-browse-node))
         (parent-db-node (pkm2-node-db-node parent-pkm-node))
         (parent-node-db-id (pkm2-db-node-id parent-db-node))

         (links-between-nodes (--> (pkm2-node-parent-links pkm-node) (-filter (lambda (link)
                                                                                (and (equal (pkm2--link-get-link-child-id link) db-id)
                                                                                     (equal (pkm2--link-get-link-parent-id link) parent-node-db-id)))
                                                                              it)))
         (link (if (length= links-between-nodes 1)
                   (car links-between-nodes)
                 (error "There are either zero or more than 1 links between node and its parent")))
         (link-db-id (pkm2-db-nodes-link-id link))
         )
    (pkm2--db-delete-link-between-nodes link-db-id)))


(defun pkm2--refresh-current-buffer (&optional buffer-name)
  (interactive)
  (let* ((buffer-name (or buffer-name (buffer-name) ))
         (buffer-state (plist-get pkm2-browse-buffer-states-equal-plist buffer-name #'equal)))
    (pkm2--browse buffer-state buffer-name)))

(defun pkm2--browse-add-node-as-child-to-node-at-point ()
  (interactive)
  (let* ((selected-node-id (pkm2-nodes-search "Select node to add as child: "))
         (browse-node (get-text-property (point) :pkm2-browse-node ))
         (pkm-node (pkm2-browse-node-pkm-node browse-node))
         (db-node (pkm2-node-db-node pkm-node))
         (node-id (pkm2-db-node-id db-node))
         (link-type-between-objects (intern (completing-read "What type of link do you want to make?" pkm-links-types) ))
         (link-label  (completing-read "What is the label for this link?" (plist-get pkm-links-type-to-label-eq-plist link-type-between-objects)))
         (new-link (pkm2--db-insert-link-between-parent-and-child
                    link-label
                    node-id
                    selected-node-id
                    (pkm2-get-current-timestamp))))
    new-link))

(defun pkm2--browse-add-node-as-parent-to-node-at-point ()
  (interactive)
  (let* ((selected-node-id (pkm2-nodes-search "Select node to add as parent "))
         (browse-node (get-text-property (point) :pkm2-browse-node ))
         (pkm-node (pkm2-browse-node-pkm-node browse-node))
         (db-node (pkm2-node-db-node pkm-node))
         (node-id (pkm2-db-node-id db-node))
         (link-type-between-objects (intern (completing-read "What type of link do you want to make?" pkm-links-types) ))
         (link-label  (completing-read "What is the label for this link?" (plist-get pkm-links-type-to-label-eq-plist link-type-between-objects)))
         (new-link (pkm2--db-insert-link-between-parent-and-child
                    link-label
                    selected-node-id
                    node-id
                    (pkm2-get-current-timestamp))))
    new-link))

(defun pkm2--browse-delete-node-at-point ()
  (interactive)
  (--> (get-text-property (point) :pkm2-browse-node)
       (pkm2-browse-node-pkm-node it)
       (pkm2-node-db-node it)
       (pkm2-db-node-id it)
       (pkm2--db-delete-node it)))


(defun pkm2--browse-promote-node-at-point ()
  ;;  TODO Complete and test
  (let* ((buffer-name (buffer-name))
         (browse-node (get-text-property (point) :pkm2-browse-node))
         (pkm-node (pkm2-browse-node-pkm-node browse-node))
         (db-node (pkm2-node-db-node pkm-node))
         (node-id (pkm2-db-node-id db-node))
         (parent-browse-node (pkm2-browse-node-parent browse-node))
         (parent-pkm-node (pkm2-browse-node-pkm-node parent-browse-node))
         (parent-db-node (pkm2-node-db-node parent-pkm-node))
         (parent-node-id (pkm2-db-node-id parent-db-node))
         (connecting-link (--> (pkm2-node-parent-links pkm-node)
                               (-filter (lambda (link)
                                          (equal parent-node-id (pkm2-db-nodes-link-node_a link)))
                                        it)
                               (if (length> it 1)
                                   (error "Promote function only supports 1 parent link between child and parent.")
                                 (car it))))
         (connecting-link-id (pkm2-db-nodes-link-id connecting-link))
         (connecting-link-label (pkm2-db-nodes-link-type connecting-link))
         (connecting-context-id (pkm2-db-nodes-link-context connecting-link))
         (parent-parent-browse-node (pkm2-browse-node-parent parent-browse-node))
         (parent-parent-pkm-node (pkm2-browse-node-pkm-node parent-parent-browse-node))
         (parent-parent-db-node (pkm2-node-db-node parent-parent-pkm-node))
         (parent-parent-node-id (pkm2-db-node-id parent-parent-db-node)))
    (pkm2--db-insert-link-between-parent-and-child connecting-link-label
                                                   parent-parent-node-id
                                                   node-id
                                                   (pkm2-get-current-timestamp)
                                                   connecting-context-id)
    (pkm2--db-delete-link-between-nodes connecting-link-id)
    (pkm2--refresh-current-buffer buffer-name)
    ;; TODO move node up in hierarchy in browser
    ))

(defun pkm2--browse-filter-children-nodes-at-point (&optional arg)
  (interactive "p")
  (let* ((browse-node (get-text-property (point) :pkm2-browse-node))
         (browse-id (pkm2-browse-node-browse-id browse-node))
         (db-id (--> (pkm2-browse-node-pkm-node browse-node)
                     (pkm2-node-db-node it)
                     (pkm2-db-node-id it)))
         (c-b-ns (pkm2-browse-node-children browse-node))
         (c-b-n-ids (-map (lambda (c-b-n)
                            (--> (pkm2-browse-node-pkm-node c-b-n) (pkm2-node-db-node it) (pkm2-db-node-id it)))
                          c-b-ns))
         (sub-query (list (list :or 'db-nodes `(:db-node-ids ,c-b-n-ids) ) ))
         (sub-query (if (equal arg 0)
                        (list `(:or db-nodes (:db-node-ids (,db-id)) )
                              `(:convert-and convert-to-children ,(pkm--convert-into-get-spec-covert-children) ) )
                      sub-query))
         (full-query (pkm2--create-query nil sub-query :and))
         (sql-query (pkm2--compile-full-db-query full-query))
         (db-nodes-ids (-flatten (sqlite-select pkm2-database-connection sql-query) )))
    (pkm2--browse-see-nodes-as-children browse-id db-nodes-ids)))


(defun pkm2--browse-see-nodes-as-children (browse-id nodes-ids)
  (let* ((browse-node (assoc-default browse-id pkm2-browse--browse-nodes-alist))
         (state (pkm2-browse-node-state browse-node))
         (from (pkm2-browse-node-state-from state) )
         (base-level (pkm2-browse-node-state-level state))
         (section (pkm2-browse-node-state-section state))
         (browse-nodes
          (-map (lambda (id)
                  (make-pkm2-browse-node :pkm-node (pkm2--db-query-get-node-with-id id) :state (make-pkm2-browse-node-state :section section)))
                nodes-ids))
         (nodes-in-hierarchy  (pkm2-browse--organize-into-hierarchies browse-nodes))
         (db-id (--> (pkm2-browse-node-pkm-node browse-node) (pkm2-node-db-node it) (pkm2-db-node-id it)))
         (browse-node (make-pkm2-browse-node :pkm-node (pkm2--db-query-get-node-with-id db-id) :state (make-pkm2-browse-node-state :section section))))
    (-each browse-nodes
      (lambda (b-n)
        (--> (pkm2-browse-node-state b-n)
             (setf (pkm2-browse-node-state-level it)
                   (+ (pkm2-browse-node-state-level it) base-level 1)))))
    (setf (pkm2-browse-node-children browse-node) nodes-in-hierarchy)
    (goto-char from)
    (pkm2--browse-remove-node browse-id)
    (setq buffer-read-only nil)
    (pkm2-browse--insert-browse-node browse-node)
    (setq buffer-read-only t)))


(defun pkm--browse-capture-node-as-child-of-node-at-point ()
  (interactive)
  (--> (get-text-property  (point) :pkm2-node-db-id) (pkm--object-capture-sub it) ))
(defun pkm--browse-capture-node-as-sibling-of-node-at-point ()
  (interactive)
  (--> (get-text-property (point) :pkm2-browse-node)
       (pkm2-browse-node-parent it)
       (pkm2-browse-node-pkm-node it)
       (pkm2-node-db-node it)
       (pkm2-db-node-id it)
       (pkm--object-capture-sub it)))

(defun pkm2--narrow-to-node-and-children-at-point ()
  (interactive)
  (let* ((browse-node (get-text-property (point) :pkm2-browse-node))
         (db-id (--> (pkm2-browse-node-pkm-node browse-node)
                     (pkm2-node-db-node it)
                     (pkm2-db-node-id it)))
         (c-b-ns (pkm2-browse-node-children browse-node))
         (c-b-n-ids (-map (lambda (c-b-n)
                            (--> (pkm2-browse-node-pkm-node c-b-n) (pkm2-node-db-node it) (pkm2-db-node-id it)))
                          c-b-ns))
         (all-ids (-concat (list db-id) c-b-n-ids))
         (sub-query (list :or 'db-nodes `(:db-node-ids ,all-ids)))
         (section-spec `(:queries ((,sub-query))))
         (section (make-pkm2-browse-section :spec  section-spec) ))
    (pkm2--browse (make-pkm2-browse-buffer-state :sections (list section)) nil)))

(defun pkm2--add-query-to-section-at-point ()
  (interactive)
  (let* ((buffer-name (buffer-name))
         (section-id (get-text-property (point) :pkm2-browse-section-id))
         (section (assoc-default section-id pkm2-browse--browse-sections-alist))
         (section-spec (pkm2-browse-section-spec section))
         (queries (plist-get section-spec :queries))
         (new-query (pkm2--create-section-query))
         (new-section-spec (plist-put section-spec :queries (-concat queries (list new-query)))))
    (setf (pkm2-browse-section-spec section) new-section-spec)
    (pkm2--refresh-current-buffer buffer-name)))

(provide 'pkm2-browse-interactivity)
