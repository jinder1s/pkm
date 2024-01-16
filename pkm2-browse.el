;;; pkm2-browse.el -*- lexical-binding: t; -*-


(require 'persist)
(require 'dash)
(require 'cl-lib)
(require 'cl-macs)
(require 'pkm-new-core)
(require 'pkm-create-pkm-query)
(require 'pkm-create-pkm-query-ewoc)
(require 'pkm-compile-db-query)
(require 'ewoc)



(cl-defstruct pkm2-browse-node
  pkm-node
  (browse-id nil)
  (level nil)
  (show-hidden nil)
  (children nil)
  (dependents nil)
  (parents nil)
  (parent nil)
  (parent-link nil)
  (ewoc-node nil)
  (section nil))

(cl-defstruct pkm2-browse-buffer-state
  (sections nil)
  ; (buffer-nodes nil)
  ; (sections-nodes nil)
  )


(cl-defstruct pkm2-browse-section
  (section-id nil)
  (spec nil)
  (sorter nil)
  (state nil)
  (name nil)
  (hidden nil)
  (show-as-hierarchy t)
  (show-active-specifiers nil)
  (start-ewoc-node nil)
  (end-ewoc-node nil))




;;;; Persistent buffer
(defvar pkm2-browse-buffer "*pkm2-browse*"
  "The persistent pkm2 buffer name. Must be surround with \"*\".")
(defvar pkm2-browse-buffer-states-equal-plist ())


(persist-defvar pkm2-browse-saved-section-specs () "Created Section specs")
(persist-defvar pkm2-browse-saved-named-section-specs () "Created Section specs")
(unless  pkm2-browse-saved-section-specs (persist-load 'pkm2-browse-saved-section-specs) )
(unless  pkm2-browse-saved-named-section-specs (persist-load 'pkm2-browse-saved-named-section-specs) )


(persist-defvar pkm2-browse-saved-named-browse-specs () "Created browse specs")
(unless  pkm2-browse-saved-named-browse-specs (persist-load 'pkm2-browse-saved-named-browse-specs) )
(defvar-local pkm2-browse--browse-nodes-alist ())
(defvar-local pkm2-browse--browse-sections-alist ())
(defvar-local pkm2-browse-ewoc nil)




(defun pkm2-browse (&optional numeric-prefix-argument)
  (interactive "p")
  (pcase numeric-prefix-argument
    (1 (let* ((completing-read-choices (-concat '("NEW" "LAST") pkm2-browse-saved-named-section-specs (-map (lambda (section-spec) (cons  section-spec section-spec)) pkm2-browse-saved-section-specs)))
              (chosen-sections (if (length> completing-read-choices 1)
                                   (completing-read-multiple "Which sections would you like to add to this section: "
                                                             completing-read-choices)
                                 (list "NEW")))
              (sections-specs (-non-nil (-map (lambda (chosen-section)
                                                (cond ((equal "NEW" chosen-section) (pkm2--create-section-spec))
                                                      ((equal "LAST" chosen-section) nil)
                                                      (t (--> (assoc-default chosen-section completing-read-choices)
                                                              (cond
                                                               ((stringp it) (read it))
                                                               ((listp it) it))))))
                                              chosen-sections) ))

              (sections-specs (if (member "LAST" chosen-sections)
                                  (--> (plist-get pkm2-browse-buffer-states-equal-plist pkm2-browse-buffer #'equal) (pkm2-browse-buffer-state-sections it) (-map #'pkm2-browse-section-spec it) (-concat sections-specs it))
                                sections-specs))
              (sections (-map (lambda (sections-spec)
                                (make-pkm2-browse-section :spec  sections-spec :sorter #'pkm2-sort-by-created-on))
                              sections-specs)))
         (pkm2--browse (make-pkm2-browse-buffer-state :sections sections) nil)))
    (3 (let* ((query-create-callback (lambda (browse-spec)
                                       (let* ((sections (-map (lambda (section-spec)
                                                                (make-pkm2-browse-section :spec  section-spec :sorter #'pkm2-sort-by-created-on))
                                                              (plist-get browse-spec :sections)))
                                              (browse-state (make-pkm2-browse-buffer-state :sections sections)))
                                         (pkm2--browse browse-state nil)))))
         (pkm-compile-create-browse-spec query-create-callback)))
    (2 (let* ((browse-spec (--> (completing-read "Which Browse spec?" pkm2-browse-saved-named-browse-specs )
                                (assoc-default it pkm2-browse-saved-named-browse-specs)
                                (cond ((stringp it) (read it))
                                      ((listp it) it))))
              (sections (-map (lambda (section-spec)
                                 (make-pkm2-browse-section :spec section-spec
                                                           :name (plist-get section-spec :name)
                                                           :hidden (plist-get section-spec :hidden)
                                                           :sorter (or (plist-get section-spec :sorter)
                                                                       #'pkm2-sort-by-created-on)))
                              (plist-get browse-spec :sections)))
              (browse-state (make-pkm2-browse-buffer-state :sections sections)))
         (pkm2--browse browse-state nil)))))


(defun pkm2--browse (buffer-state &optional buffer-name)
  (let* ((buffer-name (or buffer-name (format "%s %s" pkm2-browse-buffer (pkm-random-id))))
         (buffer (get-buffer-create buffer-name))
         (inhibit-read-only t))
    ;; (display-buffer-in-side-window (get-buffer-create buffer-name) '((side . right)))
    (with-current-buffer buffer-name
      ;; (set-window-dedicated-p (get-buffer-window buffer-name) t)
      (+word-wrap-mode )                ; TODO remove this doom specific function
      (pkm2-browse-mode)
      (tooltip-mode)
      (erase-buffer)
      (setq pkm2-browse-ewoc (ewoc-create #'pkm-browse--insert-ewoc-item nil nil))
      (setq pkm2-browse--browse-nodes-alist ())
      (setq pkm2-browse--browse-sections-alist ())
      (setq pkm2-browse-buffer-states-equal-plist (plist-put pkm2-browse-buffer-states-equal-plist buffer-name buffer-state #'equal))
      (-each-indexed
          (pkm2-browse-buffer-state-sections buffer-state)
        (lambda (index section)
          (when (> index 0))
          (pkm2-browse--insert-section section)))
      (setq buffer-read-only t) )
    (display-buffer-same-window (get-buffer-create buffer-name) nil)))


(defun pkm2--browse-get-browse-id-of-node-with-db-id (db-id)
  (car (-find (lambda (b-n-cons) (equal
                                  db-id
                                  (--> (cdr b-n-cons)
                                       (pkm2-browse-node-pkm-node it)
                                       (pkm2-node-db-node it)
                                       (pkm2-db-node-id it))))
              pkm2-browse--browse-nodes-alist) ) )
(defun pkm2-browse--organize-into-hierarchies (nodes)
  (pkm2-browse-hierarchy-organize-as-hierarchy nodes))


(defun pkm2-sort-by-created-on (browse-nodes)
  (-sort (lambda (b-n1 b-n2)
           (let ((b1-c (--> (pkm2-browse-node-pkm-node b-n1)
                            (pkm2-node-db-node it)
                            (pkm2-db-node-created_at it)))
                 (b2-c (--> (pkm2-browse-node-pkm-node b-n2)
                            (pkm2-node-db-node it)
                            (pkm2-db-node-created_at it))))
             (when (and b1-c b2-c)
               (>= b1-c b2-c))))
         browse-nodes))

(defun pkm2-sort-by-modified-on (browse-nodes)
  (-sort (lambda (b-n1 b-n2)
           (let ((b1-c (--> (pkm2-browse-node-pkm-node b-n1)
                            (pkm2-node-db-node it)
                            (pkm2-db-node-modified_at it)))
                 (b2-c (--> (pkm2-browse-node-pkm-node b-n2)
                            (pkm2-node-db-node it)
                            (pkm2-db-node-modified_at it))))
             (when (and b1-c b2-c)
               (>= b1-c b2-c))))
         browse-nodes))

(defvar section-nodes-sorting-functions  (list #'pkm2-sort-by-created-on #'pkm2-sort-by-modified-on ))



(defun pkm2-browse--insert-section (section)
  (let* ((nodes-spec (pkm2-browse-section-spec section))
         (show-as-hierarchy (pkm2-browse-section-show-as-hierarchy section))
         (pkm-nodes (--> (pkm2--browse-get-section-nodes-db-ids nodes-spec)
                         (-map #'pkm2--db-query-get-node-with-id it)))
         (sorter (pkm2-browse-section-sorter section))
         (browse-nodes (when pkm-nodes (cond (show-as-hierarchy
                              (--> (pkm2-browse-hierarchy-organize-as-hierarchy2 pkm-nodes)
                                   (-map (lambda (branch)
                                           (pkm2-convert-pkm-nodes-tree-to-browse-nodes-tree branch section 0)) it)))
                             (t (-map (lambda (pkm-node)
                                        (make-pkm2-browse-node :pkm-node pkm-node :section section :level 0))
                                      pkm-nodes))) ))
         (sorted-sorted-nodes  (if sorter
                                  (pkm2-browse-sort-browse-nodes browse-nodes sorter)
                                browse-nodes) )
         (section-id (or (pkm2-browse-section-section-id section)
                         (setf (pkm2-browse-section-section-id section) (pkm-random-id))))
         (section-start-ewoc (when pkm-nodes (or  (pkm2-browse-section-start-ewoc-node section)
                                  (ewoc-enter-last pkm2-browse-ewoc (format "---%s---" (or (pkm2-browse-section-name section) "section") )) ) ))
         (section-end-ewoc (when pkm-nodes (or (pkm2-browse-section-end-ewoc-node section)
                               (ewoc-enter-after pkm2-browse-ewoc section-start-ewoc "----")) )))
    (-each sorted-sorted-nodes (lambda (b-n)
                                 (pkm2-browse--insert-browse-node b-n `(before . ,section-end-ewoc))))

    (setf (pkm2-browse-section-start-ewoc-node section) section-start-ewoc)
    (setf (pkm2-browse-section-end-ewoc-node section) section-end-ewoc)
    (setq pkm2-browse--browse-sections-alist (assoc-delete-all section-id pkm2-browse--browse-sections-alist) )
    (push (cons section-id section) pkm2-browse--browse-sections-alist)))


(defun pkm-browse--insert-ewoc-item (ewoc-item)
  (cond ((assoc-default ewoc-item pkm2-browse--browse-nodes-alist)
         (let* ((browse-node (assoc-default ewoc-item pkm2-browse--browse-nodes-alist))
                (level (pkm2-browse-node-level browse-node))
                (prefix2 (concat (propertize " " 'display `(space :width ,(*  2 (abs (or level 0)))))))
                ;; (prefix2 (format "%s " (make-string (+ 1 level ) ?*) ))

                (pkm-node (pkm2-browse-node-pkm-node browse-node))
                (browse-insert-format-string (or (--> (pkm2-node-types pkm-node)
                                                      (-map (lambda (type)
                                                              (plist-get (plist-get pkm-structure-defined-schemas-plist type) :browse-insert-format-string))
                                                            it)
                                                      (-non-nil it)
                                                      (-distinct it)
                                                      (progn (when (length> it 1)
                                                               (message "there should be warning")
                                                               (display-warning 'pkm2-browse (format "Node has multiple insert funcs: %S"   (pkm2-node-types (pkm2-browse-node-pkm-node browse-node) ) )))
                                                             (car it)))
                                                 "<insert>(:display content)</insert>")))
           (insert prefix2)
           (insert (pkm2--browse-format-insert browse-insert-format-string browse-node))
           (set-marker (make-marker) (point))))
        ((stringp ewoc-item) (insert ewoc-item))))

(defun pkm2-browse--insert-browse-node (browse-node  &optional where only-node)
  (let* ((id (pkm2-browse-node-browse-id browse-node))
         ewoc-node
         (children-browse-nodes (--> (pkm2-browse-node-children browse-node)
                                     (-map (lambda (c-b-n-id)
                                             (assoc-default c-b-n-id pkm2-browse--browse-nodes-alist))
                                           it)))
         (parents-browse-nodes (--> (pkm2-browse-node-parents browse-node)
                                    (-map (lambda (c-b-n-id)
                                            (assoc-default c-b-n-id pkm2-browse--browse-nodes-alist))
                                          it))))

    (setq ewoc-node (cond
                     ((eq (car where) 'after)
                      (ewoc-enter-after pkm2-browse-ewoc (cdr where) id))
                     ((eq (car where) 'before)
                      (ewoc-enter-before pkm2-browse-ewoc (cdr where) id))
                     (t
                      (ewoc-enter-last pkm2-browse-ewoc id))))

    (setf (pkm2-browse-node-ewoc-node browse-node) ewoc-node)
    (when (and children-browse-nodes (not only-node))
      (-each children-browse-nodes (lambda (c-b-n)
                                     (ewoc-goto-node pkm2-browse-ewoc ewoc-node)
                                     (pkm2-browse--insert-browse-node c-b-n `(after . ,ewoc-node)))))
    (when (and parents-browse-nodes (not only-node))
      (-each parents-browse-nodes (lambda (p-b-n)
                                        ; The seperator here can be better. Not the cleanest thing in the world
                                    (pkm2-browse--insert-browse-node p-b-n `(before . ,ewoc-node)))))))


(defun pkm2-browse--default-kvd-display-string (kvd)
  (let* ((key (pkm2-db-kvd-key kvd))
         (value (pkm2-db-kvd-value kvd))
         (types (doom-plist-keys pkm-data-type-to-kvd-key-plist))
         (type (or (-find (lambda (type) (member key (plist-get pkm-data-type-to-kvd-key-plist type))) types) (pkm2-db-kvd-type kvd) )))
    (propertize (format "%s: %s" (pkm2--convert-object-to-string key) (pkm2--convert-object-to-string value type)) :db-kvd kvd)))

(defun pkm2-browse--default-hidden-info-string (browse-node)
  (let* ((browse-id (pkm2-browse-node-browse-id browse-node))
         (pkm-node (pkm2-browse-node-pkm-node browse-node))
         (db-id (--> (pkm2-node-db-node pkm-node) (pkm2-db-node-id it)))
         (kvds (pkm2-node-kvds pkm-node))
         (level (pkm2-browse-node-level browse-node))
         (prefix2 (concat (propertize " " 'display `(space :width ,(*  2 (abs (or level 0)))))))
         (hidden-string (concat
                         (--> (-map #'pkm2-browse--default-kvd-display-string kvds)
                              (string-join it (concat "\n" prefix2))
                              (format "\n%s%s" prefix2 it))
                         "\n"
                         prefix2
                         (format "id: %d" db-id)
                         "\n"
                         prefix2
                         (format "browse-id: %d" browse-id))))
    hidden-string))

(defun pkm2--browse-convert-object-to-string (object)
  (cond ((stringp object) object)
        ((numberp object) (format "%d" object))))

(defun pkm2-browse--refresh-insert-browse-node (browse-node)
  (let* ((ewoc-node (pkm2-browse-node-ewoc-node browse-node))
         (inhibit-read-only t))
    (ewoc-goto-node pkm2-browse-ewoc ewoc-node)
    (pkm2-browse--insert-browse-node browse-node `(after . ,ewoc-node) t)
    (ewoc-delete pkm2-browse-ewoc ewoc-node)))









(defun pkm2--browse-remove-node (browse-node-or-id &optional only-current-node)
  ; Remove node from pkm2-browse--browse-nodes-alist
  ; Remove node from section pkm2-browse--browse-sections-alist
  ; Remove node from its children
  ; Remove node from its parent
  ; Also delete children
  (let* ((browse-node (pcase browse-node-or-id
                        ((pred pkm2-browse-node-p) browse-node-or-id)
                        ((pred stringp) (assoc-default browse-node-or-id pkm2-browse--browse-nodes-alist))))
         (browse-id (pcase browse-node-or-id
                      ((pred pkm2-browse-node-p) (pkm2-browse-node-browse-id browse-node-or-id))
                      ((pred stringp) browse-node-or-id)))
         (ewoc-node (pkm2-browse-node-ewoc-node browse-node))
         (inhibit-read-only t)
         (parent-id (pkm2-browse-node-parent browse-node))
         (parent-browse-node (assoc-default parent-id pkm2-browse--browse-nodes-alist))
         (children-ids (pkm2-browse-node-children browse-node))
         (parents-ids (pkm2-browse-node-parents browse-node)))
    (when (and (not only-current-node) children-ids )
      (-each children-ids  #'pkm2--browse-remove-node))
    (when (and (not only-current-node) parents-ids)
      (-each parents-ids  #'pkm2--browse-remove-node))
    (when parent-browse-node
      (setf (pkm2-browse-node-children parent-browse-node)
            (-filter (lambda (child)
                       (not (equal browse-id (pkm2-browse-node-browse-id child))))
                     (pkm2-browse-node-children parent-browse-node))))
    (ewoc-delete pkm2-browse-ewoc ewoc-node)
    (setq pkm2-browse--browse-nodes-alist (assq-delete-all browse-id pkm2-browse--browse-nodes-alist))
    nil
    ))

(defun pkm2--browse-get-section-nodes-db-ids (section-spec)
  (let* ((queries (plist-get section-spec :queries))
         (queries-db-query (-map #'pkm2--compile-full-db-query queries))
         (nodes-ids (-flatten (-map (lambda (query)
                                      (--> (sqlite-select pkm2-database-connection query)
                                           (-flatten it)))
                                    queries-db-query) )))
    nodes-ids))

(defun pkm2--browse-get-query-nodes (query-spec)
  (--> (pkm2--compile-full-db-query query-spec) (sqlite-select pkm2-database-connection it) (-flatten it)))


(defun pkm2--browse-section-next-node ())
(defun pkm2--browse-section-previous-node ())
(defun pkm2--browse-section-parent-node ())


;;; interactivity
;;;
(defun pkm2-sort-funcs-in-current-section ()
  (let* ((current-section (pkm2--browse-get-section-at-point))
         (sort-fuction (intern (completing-read "What sorting func?" (-map #'symbol-name section-nodes-sorting-functions)))))
    (setf (pkm2-browse-section-sorter current-section) sort-fuction)
    (pkm-browse-section-refresh current-section)))

(defun pkm2--browse-get-section-at-point ()
  (or
   (--> (pkm2--browse-get-browse-node-at-point)
        (when it (pkm2-browse-node-section it)))
   (--> (when pkm2-browse-ewoc (ewoc-locate pkm2-browse-ewoc (point)))
        (when it
          (-find (lambda (sections-cons)
                   (or
                    (eq (pkm2-browse-section-start-ewoc-node (cdr sections-cons))
                        it)
                    (eq (pkm2-browse-section-end-ewoc-node (cdr sections-cons))
                        it)))
                 pkm2-browse--browse-sections-alist) )
        (when it
          (cdr it)))))
(defun pkm2--browse-get-browse-node-at-point (&optional current-point)
  (if-let* ((browse-node-id (pkm2--browse-get-browse-node-id-at-point current-point) )
            (browse-node (assoc-default browse-node-id  pkm2-browse--browse-nodes-alist) ))
      browse-node))
(defun pkm2-get-pkm-node-at-point ()
  (--> (pkm2--browse-get-browse-node-at-point) (when it (pkm2-browse-node-pkm-node it) )))

(defun pkm2--browse-get-browse-node-id-at-point (&optional current-point)
  (if-let* ((current-ewoc-node (when pkm2-browse-ewoc (ewoc-locate pkm2-browse-ewoc (or current-point (point)))))
            (browse-node-id (ewoc-data current-ewoc-node)))
      browse-node-id))

(defun pkm2--browse-get-browse-node-db-node-id-at-point (&optional current-point)
  (--> (pkm2--browse-get-browse-node-at-point)
       (pkm2-browse-node-pkm-node it)
       (pkm2-node-db-node it)
       (pkm2-db-node-id it)))

(defun pkm2--browse-toggle-hidden-info-at-point ()
  (interactive)
  (save-excursion
    (let* ((browse-node (pkm2--browse-get-browse-node-at-point))
           (show-hidden (pkm2-browse-node-show-hidden browse-node))
           (new-show-hidden (not show-hidden)))
      (setf (pkm2-browse-node-show-hidden browse-node) new-show-hidden)
      (pkm2-browse--refresh-insert-browse-node browse-node))))

(defun pkm2--browse-edit-object-at-point ()
  (interactive)
  (let* ((current-point (point))
         (browse-node (pkm2--browse-get-browse-node-at-point))
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
               (type (assoc-default key (or (-flatten (-map (lambda (type)
                                                              (-map (lambda (key)
                                                                      (cons key type))
                                                                    (plist-get pkm-data-type-to-kvd-key-plist type)))
                                                            (doom-plist-keys pkm-data-type-to-kvd-key-plist )))
                                            (pkm2--db-query-get-all-keys-to-types-alist))))

               (prompt "What value?")
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
      ; browse-node should have been modified by values above
      (pkm2-browse--refresh-insert-browse-node browse-node))))



(defun pkm2--browse-see-children-at-point ()
  (interactive)
  (pkm2--browse-see-children (pkm2--browse-get-browse-node-id-at-point)))


(defun pkm2--browse-see-children (browse-id &optional level-children)
  (let* ((browse-node (assoc-default browse-id pkm2-browse--browse-nodes-alist))
         (ewoc-node (pkm2-browse-node-ewoc-node browse-node))
         (base-level (pkm2-browse-node-level browse-node))
         (section (pkm2-browse-node-section browse-node))
         (db-id (--> (pkm2-browse-node-pkm-node browse-node) (pkm2-node-db-node it) (pkm2-db-node-id it)))
         (level-children (or level-children (read (completing-read "How many levels of children would you like?" `("1" "2" "3" "4" "5" "6" "7" "ALL")) ) ))
         (query-spec `((:or db-node (:db-id ,db-id )) (:convert-or convert-to-children (:levels ,level-children) )) )
         (nodes-ids (pkm2--browse-get-query-nodes query-spec))
         (browse-nodes
          (-map (lambda (id)
                  (make-pkm2-browse-node :pkm-node (pkm2--db-query-get-node-with-id id)  :section section)) nodes-ids) )
         (nodes-in-hierarchy  (pkm2-browse--organize-into-hierarchies browse-nodes))
         (inhibit-read-only t))
    (-each browse-nodes
      (lambda (b-n)
        (setf (pkm2-browse-node-level b-n) (+ (pkm2-browse-node-level b-n) base-level))))
    (pkm2--browse-remove-node browse-id)
    (-each nodes-in-hierarchy (lambda (browse-node)
                                (pkm2-browse--insert-browse-node browse-node `(after . ,ewoc-node))))))


(defun pkm2--browse-see-parents (browse-id &optional db-id level-parents)
  "Very hacky and not very well done. Needs to be fixed."
  (error "Function hasn't yet been setup")
  (let* ((browse-node (assoc-default browse-id pkm2-browse--browse-nodes-alist))
         (ewoc-node (pkm2-browse-node-ewoc-node browse-node))
         (base-level (pkm2-browse-node-level browse-node))
         (section (pkm2-browse-node-section browse-node))
         (db-id (--> (pkm2-browse-node-pkm-node browse-node) (pkm2-node-db-node it) (pkm2-db-node-id it)))
         (level-parents (or level-parents (read (completing-read "How many levels of parents would you like?" `("1" "2" "3" "4" "5" "6" "7" "ALL")) ) ))
         (pkm-query `((:or db-node (:db-id ,db-id))
                      (:convert-and convert-to-parents (:levels ,level-parents :link-labels ("sub") ))))

         (query (pkm2--compile-full-db-query pkm-query))
         (query-results (sqlite-select pkm2-database-connection query))

         (inhibit-read-only t))
    (pkm2--browse-remove-node browse-id)
    ))


(defun pkm2--browse-see-parents-at-point ()
  (interactive)
  (pkm2--browse-see-parents (pkm2--browse-get-browse-node-id-at-point)))


(defun pkm2--browse-add-kvd-at-point ()
  (interactive)
  (let* ((current-point (point))
         (browse-node (pkm2--browse-get-browse-node-at-point))
         (pkm-node (pkm2-browse-node-pkm-node browse-node))
         (db-node (pkm2-node-db-node pkm-node))
         (node-id (pkm2-db-node-id db-node))
         (potential-keys (or (-flatten (-map (lambda (type)
                                               (-map (lambda (key)
                                                       (cons key type))
                                                     (plist-get pkm-data-type-to-kvd-key-plist type)))
                                             (doom-plist-keys pkm-data-type-to-kvd-key-plist )))
                             (pkm2--db-query-get-all-keys-to-types-alist) ))
         (key (completing-read "What key would you like to add?" potential-keys nil 'confirm))
         (type (assoc-default key potential-keys))
         (value
          (cond ((eq type 'DATETIME) (pkm2-get-user-selected-timestamp) )
                (t (-->
                    (pkm2--db-query-get-all-values-with-key key type)
                    (-map (lambda (value) (pkm2--convert-object-to-string value type)) it)
                    (completing-read "What value?" it nil 'confirm)
                    (if (or (eq type 'REAL)
                            (eq type 'INTEGER))
                        (string-to-number  it)
                      it)))))
         (new-kvd (pkm2--db-get-or-insert-kvd key value type))
         (new-kvd-id (pkm2-db-kvd-id new-kvd))
         (new-link (pkm2--db-insert-link-between-node-and-kvd node-id new-kvd-id (pkm2-get-current-timestamp) type)))
    (setf (pkm2-db-kvd-link-id new-kvd) (pkm2-db-kvd-link-id2 new-link))
    (setf (pkm2-node-kvds pkm-node) (-concat (pkm2-node-kvds pkm-node) (list new-kvd)))))


(defun pkm2--browse-delete-link-to-kvd-at-point ()
  (interactive)
  (let* ((current-point (point))
         (browse-node (pkm2--browse-get-browse-node-at-point))
         (pkm-node (pkm2-browse-node-pkm-node browse-node))
         (kvd (get-text-property current-point :db-kvd))
         (type (pkm2-db-kvd-type kvd))
         (link-id (pkm2-db-kvd-link-id kvd))
         (kvds (pkm2-node-kvds pkm-node))
         (timestamp (pkm2-get-current-timestamp)))
    (pkm2--update-node-kvd pkm-node kvd nil type nil link-id timestamp)
    (setf (pkm2-node-kvds pkm-node) (-filter (lambda (temp-kvd)
                                               (if (equal (pkm2-db-kvd-id temp-kvd) (pkm2-db-kvd-id kvd))
                                                   nil ; delete this kvd from node
                                                 t))
                                             kvds))
    (pkm2-browse--refresh-insert-browse-node browse-node)))
(defun pkm2--browse-delete-link-to-parent-of-node-at-point ()
  (let* ((current-point (point))
         (browse-node (pkm2--browse-get-browse-node-at-point))
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
         (buffer-state (plist-get pkm2-browse-buffer-states-equal-plist buffer-name #'equal))
         (sections (pkm2-browse-buffer-state-sections buffer-state)))
    (-each sections (lambda (section)
                     (setf (pkm2-browse-section-start-ewoc-node section) nil)
                     (setf (pkm2-browse-section-end-ewoc-node section) nil) ))
    (pkm2--browse buffer-state buffer-name)))

(defun pkm2--browse-add-node-as-child-to-node-at-point ()
  (interactive)
  (let* ((selected-node-id (pkm2-nodes-search "Select node to add as child: "))
         (browse-node (pkm2--browse-get-browse-node-at-point))
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
         (browse-node (pkm2--browse-get-browse-node-at-point))
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
  (let* ((b-n (pkm2--browse-get-browse-node-at-point)))
    (-->
     (pkm2-browse-node-pkm-node b-n)
     (pkm2-node-db-node it)
     (pkm2-db-node-id it)
     (pkm2--db-delete-node it))
    (pkm2--browse-remove-node b-n)))


(defun pkm2--browse-promote-node-at-point ()
  ;  TODO Complete and test
  (let* ((buffer-name (buffer-name))
         (browse-node (pkm2--browse-get-browse-node-at-point))
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
    ; TODO move node up in hierarchy in browser
    ))

(defun pkm2--browse-filter-children-nodes-at-point (&optional arg)
  (interactive "p")
  (let* ((browse-node (pkm2--browse-get-browse-node-at-point))
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


(defun pkm2--browse-see-nodes-as-children (browse-id nodes-db-ids)
  (let* ((browse-node (assoc-default browse-id pkm2-browse--browse-nodes-alist))
         (ewoc-node (pkm2-browse-node-ewoc-node browse-node))
         (base-level (pkm2-browse-node-level browse-node))
         (section (pkm2-browse-node-section browse-node))
         (browse-nodes
          (-map (lambda (id)
                  (let* ((pkm-node (pkm2--db-query-get-node-with-id id))
                         (b-n-id (pkm-random-id))
                         (b-n (make-pkm2-browse-node :pkm-node pkm-node :section section :browse-id b-n-id)))
                    (push (cons b-n-id b-n) pkm2-browse--browse-nodes-alist)
                    b-n))
                nodes-db-ids))
         (nodes-in-hierarchy  (pkm2-browse--organize-into-hierarchies browse-nodes))
         (db-id (--> (pkm2-browse-node-pkm-node browse-node) (pkm2-node-db-node it) (pkm2-db-node-id it)))
         (browse-node (make-pkm2-browse-node :pkm-node (pkm2--db-query-get-node-with-id db-id) :section section))
         (inhibit-read-only t))
    (-each browse-nodes
      (lambda (b-n)
        (setf (pkm2-browse-node-level b-n)
              (+ (pkm2-browse-node-level b-n) base-level 1))))
    (setf (pkm2-browse-node-children browse-node) nodes-in-hierarchy)
    (pkm2--browse-remove-node browse-id)
    (pkm2-browse--insert-browse-node browse-node `(after . ,ewoc-node))))

(defun pkm2--browse-see-node-as-child (browse-id child-db-id)
  (let* ((browse-node (assoc-default browse-id pkm2-browse--browse-nodes-alist))
         (ewoc-node (pkm2-browse-node-ewoc-node browse-node))
         (base-level (pkm2-browse-node-level browse-node))
         (section (pkm2-browse-node-section browse-node))
         (child-browse-id (pkm-random-id))
         (child-browse-node
          (let* ((pkm-node (pkm2--db-query-get-node-with-id child-db-id))
                 (b-n (make-pkm2-browse-node :pkm-node pkm-node :section section
                                             :browse-id child-browse-id
                                             :parent browse-id :level (+ base-level 1))))
            (push (cons child-browse-id b-n) pkm2-browse--browse-nodes-alist)
            b-n))
         (browse-node-children (pkm2-browse-node-children browse-node))
         (b-n-children-new (-concat (list child-browse-id ) browse-node-children))
         (inhibit-read-only t))
    (setf (pkm2-browse-node-children browse-node) b-n-children-new)
    (pkm2-browse--insert-browse-node child-browse-node `(after . ,ewoc-node))))

(defun pkm--browse-capture-node-as-child-of-node-at-point ()
  (interactive)
  (let* ((browse-node-id (pkm2--browse-get-browse-node-id-at-point))
         (browse-node (assoc-default browse-node-id pkm2-browse--browse-nodes-alist )))
    (-->
     (pkm2-browse-node-pkm-node browse-node)
     (pkm2-node-db-node it)
     (pkm2-db-node-id it)
     (pkm--object-capture-sub it))))


(defun pkm--browse-capture-node-as-sibling-of-node-at-point ()
  (interactive)
  (let* ((b-n (pkm2--browse-get-browse-node-id-at-point))
         (p-b-n (pkm2-browse-node-parent b-n)))
    (-->
     (pkm2-browse-node-pkm-node p-b-n)
     (pkm2-node-db-node it)
     (pkm2-db-node-id it)
     (pkm--object-capture-sub it))))

(defun pkm2--narrow-to-node-and-children-at-point ()
  (interactive)
  (let* ((browse-node (pkm2--browse-get-browse-node-at-point))
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
  (let* ((section (pkm2--browse-get-section-at-point)))
    (pkm-browse-section-add-query section)
    (pkm2--refresh-current-buffer (buffer-name))))


;;; hierarchy
(defun pkm2-browse-hierarchy-organize-as-hierarchy (browse-nodes)
  (let* ((pkm-nodes (-map #'pkm2-browse-node-pkm-node browse-nodes))
         (parent-links (-map #'pkm2-node-parent-links pkm-nodes))
         (parent-db-ids-list (-map (lambda (p-links)
                                     (-map #'pkm2-db-nodes-link-node_a p-links)) parent-links))
         (children-links (-map #'pkm2-node-children-links pkm-nodes))
         (children-db-ids-list (-map (lambda (c-links)
                                       (-map #'pkm2-db-nodes-link-node_b c-links)) children-links))
         (db-nodes (-map #'pkm2-node-db-node pkm-nodes))
         (db-ids (-map #'pkm2-db-node-id db-nodes))
         (parent-less-nodes (--> (-map-indexed (lambda (index p-ids)
                                                 (cond ((not p-ids) index)
                                                       ((not (-any? (lambda (p-id)
                                                                      (member p-id db-ids)) p-ids))
                                                        index)))
                                               parent-db-ids-list)
                                 (-non-nil it)
                                 (-map (lambda (index)
                                         (nth index browse-nodes)) it)))
         (present-children-indexes (-map (lambda (c-ids)
                                           (-non-nil
                                            (-map (lambda (c-id)
                                                    (-elem-index c-id db-ids))
                                                  c-ids)))
                                         children-db-ids-list)))
    (-each parent-less-nodes (lambda (b-n)
                               (setf (pkm2-browse-node-level b-n) 0)))
    (-each-indexed browse-nodes (lambda (index p-b-n)
                                  (when-let ((p-b-n-level (pkm2-browse-node-level p-b-n))
                                             (p-b-n-id (pkm2-browse-node-browse-id p-b-n))
                                             (children-indexes-list  (nth index present-children-indexes))
                                             (children-browse-nodes (-map (lambda (index)
                                                                            (nth index browse-nodes))
                                                                          children-indexes-list))
                                        ; Only modify child nodes that don't already have parent
                                        ; In the future, I might want to duplicate here
                                             (c-b-n-with-no-parent (-filter (lambda (c-b-n)
                                                                              (not (pkm2-browse-node-parent c-b-n)))
                                                                            children-browse-nodes))
                                             (c-b-n-with-no-parent-browse-node-ids (-map #'pkm2-browse-node-browse-id c-b-n-with-no-parent)))
                                    (setf (pkm2-browse-node-children p-b-n) c-b-n-with-no-parent-browse-node-ids)
                                    (-each c-b-n-with-no-parent (lambda (c-b-n) (setf (pkm2-browse-node-parent c-b-n) p-b-n-id)))
                                    (-each c-b-n-with-no-parent (lambda (c-b-n)
                                                                  (setf (pkm2-browse-node-level c-b-n) (+ p-b-n-level 1)))))))
    parent-less-nodes))


(defun pkm2-browse-hierarchy-organize-children (pkm-node pkm-nodes)
  (let* ((children-links (pkm2-node-children-links pkm-node))
         (pkm-node-children-node-db-ids (-map #'pkm2--link-get-link-child-id children-links))
         (present-children (-map (lambda (p-n) (when (member (--> (pkm2-node-db-node p-n)
                                                                  (pkm2-db-node-id it))
                                                             pkm-node-children-node-db-ids)
                                                 p-n))
                                 pkm-nodes))
         (present-children-organized (-map (lambda (present-child) (when present-child (pkm2-browse-hierarchy-organize-children present-child pkm-nodes) ))
                                           present-children) )
         (present-children-organized-with-links (-non-nil (-map-indexed (lambda (index child)
                                                                (when child
                                                                  (plist-put child :parent-link (nth index children-links))))
                                                              present-children-organized) )))
    `(:node ,pkm-node :children ,present-children-organized-with-links)))

(defun pkm2-browse-hierarchy-organize-as-hierarchy2 (pkm-nodes)
  (let* ((parent-links (-map #'pkm2-node-parent-links pkm-nodes))
         (parent-db-ids-list (-map (lambda (p-links)
                                     (-map #'pkm2--link-get-link-parent-id p-links))
                                   parent-links))
         (db-nodes (-map #'pkm2-node-db-node pkm-nodes))
         (db-ids (-map #'pkm2-db-node-id db-nodes))
         (parentless-pkm-nodes (--> (-map-indexed (lambda (index p-ids)
                                                    (cond ((not p-ids) index)
                                                          ((not (-any? (lambda (p-id)
                                                                         (member p-id db-ids)) p-ids))
                                                           index)))
                                                  parent-db-ids-list)
                                    (-non-nil it)
                                    (-map (lambda (index)
                                            (nth index pkm-nodes)) it))))
    (-map (lambda (parentless-pkm-node) (pkm2-browse-hierarchy-organize-children parentless-pkm-node pkm-nodes)) parentless-pkm-nodes)))


(defun pkm2-convert-pkm-nodes-tree-to-browse-nodes-tree (branch section level)
  (let* ((node (plist-get branch :node))
         (children (plist-get branch :children))
         (parents (plist-get branch :parents))
         (parent-link (plist-get branch :parent-link))
         (children-nodes  (-map (lambda (child-branch)
                                 (pkm2-convert-pkm-nodes-tree-to-browse-nodes-tree child-branch section (+ level 1)))
                               children) )
         (parents-nodes (-map (lambda (child-branch)
                                (pkm2-convert-pkm-nodes-tree-to-browse-nodes-tree child-branch section (+ level 1)))
                              parents))
         (children-node-ids (-map #'pkm2-browse-node-browse-id children-nodes ))
         (parents-node-ids (-map #'pkm2-browse-node-browse-id parents-nodes ))
         (id (pkm-random-id))
         (browse-node (make-pkm2-browse-node :browse-id id
                                             :level level
                                             :children children-node-ids
                                             :parents parents-node-ids
                                             :section section
                                             :pkm-node node
                                             :parent-link parent-link)))
    (-each (-concat parents-nodes children-nodes) (lambda (b-n)
                                                    (setf (pkm2-browse-node-parent b-n) id)))
    (push (cons id browse-node) pkm2-browse--browse-nodes-alist)
    browse-node))
(defun pkm2-browse-sort-browse-nodes (browse-nodes sorter-func)
   (-each browse-nodes (lambda (b-n)
                         (setf (pkm2-browse-node-children b-n)
                               (--> (pkm2-browse-node-children b-n)
                                    (-map (lambda (c-b-n-id)
                                            (assoc-default c-b-n-id pkm2-browse--browse-nodes-alist)) it)
                                    (pkm2-browse-sort-browse-nodes it sorter-func)
                                    (-map #'pkm2-browse-node-browse-id it)))
                         (setf (pkm2-browse-node-parents b-n) (--> (pkm2-browse-node-parents b-n)
                                    (-map (lambda (p-b-n-id)
                                            (assoc-default p-b-n-id pkm2-browse--browse-nodes-alist)) it)
                                    (pkm2-browse-sort-browse-nodes it sorter-func)
                                    (-map #'pkm2-browse-node-browse-id it)))))
  (funcall sorter-func browse-nodes))

(defun pkm2-browse-heirarchy-add-as-child-browse-node (parent child))


(defun pkm2-browse-heirarchy-goto-first-child (parent))
(defun pkm2-browse-heirarchy-goto-parent (child))
(defun pkm2-browse-see-child-nodes (parent levels))
(defun pkm2-browse-see-parent-nodes (child levels))
(defun pkm2-browse-refresh-tree (browse))
(defun pkm2-browse-get-tree-nodes (browse))

;;; browse-insert

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
         (show-hidden (pkm2-browse-node-show-hidden browse-node))
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
                               (-->  (pkm2-node-db-node pkm-node) (propertize (pkm2--convert-object-to-string (pkm2-db-node-content it))
                                                                              :db-node it)))
                              ((equal 'hidden (plist-get spec :display))
                               (if show-hidden
                                   (concat (plist-get spec :prefix)
                                           (pkm2-browse--default-hidden-info-string browse-node) )
                                 "")))))
    (when (plist-get spec :face)
      (setq output-string (propertize output-string 'face (plist-get spec :face))))
    output-string ))

;;; browse-mode
(defvar pkm2-browse-mode-map (make-sparse-keymap))

(map! :map pkm2-browse-mode-map
      ; Recently, a [tab] keybind in `outline-mode-cycle-map' has begun
      ; overriding org's [tab] keybind in GUI Emacs. This is needed to undo
      ; that, and should probably be PRed to org.

      :localleader
      [tab]        #'pkm2--browse-toggle-hidden-info-at-point
      (:prefix ("e" . "edit")
               "e" #'pkm2--browse-edit-object-at-point)
      (:prefix ("b" . "buffer")
               "r" #'pkm2--refresh-current-buffer
               "a" #'pkm2-browse-add-section
               "s" #'pkm2-browse-save-browse-spec
               )
      (:prefix ("s" . "section")
               "r" #'pkm2--refresh-current-buffer
               "q" #'pkm2--add-query-to-section-at-point
               "e" #'pkm-browse-section-edit-spec
               "s" #'pkm-browse-section-save-spec
               )
      (:prefix "n"
               (:prefix ("a" . "add")
                (:prefix ("r" . "relationship")
                         "p" #'pkm2--browse-add-node-as-parent-to-node-at-point
                         "c" #'pkm2--browse-add-node-as-child-to-node-at-point)
                :desc "Add kvd" "k" #'pkm2--browse-add-kvd-at-point
                "c" #'pkm--browse-capture-node-as-child-of-node-at-point
                "s" #'pkm--browse-capture-node-as-sibling-of-node-at-point )
               (:prefix ("c" . "clock")
                        "e" #'pkm2-clock-edit-clock-at-point
                        "i" #'pkm2-clock-in
                        "o" #'pkm2-clock-out
                        "s" #'pkm2-clock-switch)
               (:prefix ("s" . "show")
                :desc "Children" "c" #'pkm2--browse-see-children-at-point
                :desc "Parents"  "p" #'pkm2--browse-see-parents-at-point)
               "f" #'pkm2--browse-filter-children-nodes-at-point
               "n" #'pkm2--narrow-to-node-and-children-at-point
               "d" #'pkm2--browse-delete-node-at-point))



(define-minor-mode pkm2-browse-mode
  "Minor mode for simple finish/cancel keybindings."
  :keymap pkm2-browse-mode-map)



(defun pkm2-browse-add-section (&optional buffer-name)
  (interactive)
  (let* ((buffer-name (or buffer-name (buffer-name) ))
         (buffer-state (plist-get pkm2-browse-buffer-states-equal-plist buffer-name #'equal))
         (sections (pkm2-browse-buffer-state-sections buffer-state))
         (completing-read-choices (-concat (list '("NEW" . "NEW") ) pkm2-browse-saved-named-section-specs))
         (chosen-section (if (length> completing-read-choices 1)
                             (completing-read "Which section would you like to add: " completing-read-choices)
                           "NEW"))
         (new-section (make-pkm2-browse-section :spec  (if (equal "NEW" chosen-section)
                                                           (pkm2--create-section-spec)
                                                         (read (assoc-default chosen-section completing-read-choices) ))))
         (new-sections (-concat sections (list new-section))))
    (setf (pkm2-browse-buffer-state-sections buffer-state) new-sections)
    (pkm2--browse buffer-state buffer-name)))

(defun pkm2-browse-save-browse-spec (&optional buffer-name)
  (interactive)
  (let* ((buffer-name (or buffer-name (buffer-name) ))
         (buffer-state (plist-get pkm2-browse-buffer-states-equal-plist buffer-name #'equal))
         (sections (--> (pkm2-browse-buffer-state-sections buffer-state) (-map #'pkm2-browse-section-spec it) ))
         (browse-spec `(:sections ,sections) )
         (name (read-string "Browse name: ")))
    (setq pkm2-browse-saved-named-browse-specs (-concat (list (cons name (format "%S" browse-spec)) ) pkm2-browse-saved-named-browse-specs))))

;;; browse-section

(defun pkm2--create-section-spec ()
  (let* ((buffer-name "* create-section *")
         (completing-read-choices (-concat '("NEW") pkm2-browse-saved-named-queries))
         (chosen-queries (if (or pkm2-browse-saved-named-queries pkm2-browse-saved-queries)
                             (completing-read-multiple "Which queries would you like to add to this section: "
                                                       completing-read-choices)
                           (list "NEW")))

         (initial-queries (--> (-filter (lambda (spec) (not (equal "NEW" spec))) chosen-queries)
                               (-map (lambda (choice) (assoc-default choice completing-read-choices)) it)
                               (-map #'read it)))
         (queries (-copy initial-queries))
         output
         (create-query (member "NEW" chosen-queries))
         (print-section (lambda (queries)
                          (erase-buffer)
                          (princ (format "(queries:\n%s)"
                                         (string-join (-map (lambda (query)
                                                              (format "%S" query))
                                                            queries)
                                                      "\n" ))))))
    (with-output-to-temp-buffer buffer-name
      (unless (equal (buffer-name) buffer-name)
        (switch-to-buffer-other-window buffer-name))
      (while create-query
        (setq queries (--> (pkm2--create-query (lambda (query-spec)
                                                 (-as-> (-concat queries
                                                                 (list query-spec))
                                                        it2
                                                        (funcall print-section it2))))
                           (-concat queries (list it))))
        (funcall print-section queries)
        (setq create-query (y-or-n-p "Would you like to add another query to this section?"))))
    (kill-buffer buffer-name)
    (setq output `(:queries ,queries ))
    output))

(defun pkm2--create-section-query ()
  (let* ((buffer-name "* create-query *")
         (print-query (lambda (query)
                        (erase-buffer)
                        (princ (format "%S" query))))
         (output (with-output-to-temp-buffer buffer-name
                   (unless (equal (buffer-name) buffer-name)
                     (switch-to-buffer-other-window buffer-name))
                   (pkm2--create-query print-query))))

    (kill-buffer buffer-name)
    (setq pkm2-browse-saved-queries (-concat (list output) pkm2-browse-saved-queries))
    (persist-save pkm2-browse-saved-queries)
    output))



(defun pkm-browse-section-refresh (&optional section)
  (let* ((section (or section (pkm2--browse-get-section-at-point)))
         (section-nodes (--> (-map #'cdr  pkm2-browse--browse-nodes-alist)
                             (-filter (lambda (b-n)
                                        (eq  (pkm2-browse-node-section b-n)  section))
                                      it)
                             )))
    (-each section-nodes  (lambda (b-n) (pkm2--browse-remove-node b-n t)))
    (pkm2-browse--insert-section section)))

(defun pkm-browse-section-add-query (section)
  (let* ((section-spec (pkm2-browse-section-spec section))
         (queries (plist-get section-spec :queries))
         (new-query (pkm2--create-section-query))
         (new-section-spec (plist-put section-spec :queries (-concat queries (list new-query)))))
    (setf (pkm2-browse-section-spec section) new-section-spec)))

(defun  pkm-browse-section-edit-spec ()
  (interactive)
  (if-let*
      ((buffer-name (buffer-name))
       (section-id (get-text-property (point) :pkm2-browse-section-id))
       (section (assoc-default section-id pkm2-browse--browse-sections-alist))
       (section-spec (pkm2-browse-section-spec section))
       (callback (lambda (browse-spec)
                   (setf  (pkm2-browse-section-spec section) (car (plist-get browse-spec :sections)))
                   (pkm2--refresh-current-buffer buffer-name))))
      (pkm-compile-create-browse-spec callback nil section-spec)
    (error (format "Somethign is nill: section-id: %S, section: %S, section-spec: %S" section-id section section-spec) )))

(defun pkm-browse-section-save-spec ()
  (interactive)
  (let* ((buffer-name (buffer-name))
         (section-id (get-text-property (point) :pkm2-browse-section-id))
         (section (assoc-default section-id pkm2-browse--browse-sections-alist))
         (section-spec (pkm2-browse-section-spec section))
         (section-spec-string (format "%S" section-spec))
         (name (read-string (format "Name for: %s\n" section-spec-string))))
    (setq pkm2-browse-saved-named-section-specs (-concat (list (cons name section-spec-string)) pkm2-browse-saved-named-section-specs ) )))

;;; search
(defun pkm2-get-nodes-with-text (text )
  (let* ((single-query-spec `(text (:text ,text)))
         (query (pkm2--compile-db-query single-query-spec  "node") )
         (nodes-db-ids (-flatten (sqlite-select pkm2-database-connection query) ))
         (nodes-texts-and-id (sqlite-select pkm2-database-connection (format "SELECT content, id  FROM node WHERE id IN (%s)"
                                                                             (--> (-map (lambda (node-id)
                                                                                          (pkm2--db-format-value-correctly node-id 'INTEGER))
                                                                                        nodes-db-ids)
                                                                                  (string-join it ", ")))) ))
    (-map #'car nodes-texts-and-id)))

(defun pkm2-search ()
  (interactive)
  (pkm2-nodes-search)

  )

(defun pkm2-nodes-search (&optional prompt)
  "Read and return an `dbid' for a node whose text."
  (if-let* ((prompt (or prompt "Node: "))
         (node-text (completing-read
                     prompt
                     (lambda (input-string pred action)
                       (cond ((eq action 'metadata)
                              `(metadata
                                (category . pkm-node)))
                             ((eq action 'lambda)
                              (error "Lambda not defined"))
                             ((eq (car-safe action) 'boundaries)
                              `(0 . ,(length (cdr action) )))
                             (action (pkm2-get-nodes-with-text input-string))
                             ((not action) (let* ((matches (pkm2-get-nodes-with-text input-string)))
                                             (cond ((not matches) nil)
                                                   ((and (length= matches 1) (equal (car matches) input-string)) t)
                                                   (t input-string))))))))
         (query (format "SELECT id FROM node WHERE content = %s" (pkm2--db-format-value-correctly node-text 'TEXT)))
         (node-id (car-safe (car-safe (sqlite-select pkm2-database-connection query)))))
    node-id))

(defun pkm2-nodes-search-multiple (&optional prompt)
  "Read and return an `dbid' for a node whose text."
  (if-let* ((prompt (or prompt "Node: "))
         (node-texts (completing-read-multiple
                     prompt
                     (lambda (input-string pred action)
                       (cond ((eq action 'metadata)
                              `(metadata
                                (category . pkm-node)))
                             ((eq action 'lambda)
                              (error "Lambda not defined"))
                             ((eq (car-safe action) 'boundaries)
                              `(0 . ,(length (cdr action) )))
                             (action (pkm2-get-nodes-with-text input-string))
                             ((not action) (let* ((matches (pkm2-get-nodes-with-text input-string)))
                                             (cond ((not matches) nil)
                                                   ((and (length= matches 1) (equal (car matches) input-string)) t)
                                                   (t input-string))))))))
         (queries (-map (lambda (node-text)
                        (format "SELECT id FROM node WHERE content = %s" (pkm2--db-format-value-correctly node-text 'TEXT)))
                      node-texts))
         (node-ids (-map  (lambda (query)
                           (car-safe (car-safe (sqlite-select pkm2-database-connection query))) )
                         queries)))
    node-ids))



(provide 'pkm2-browse)
