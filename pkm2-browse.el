;;; pkm2-browse.el -*- lexical-binding: t; -*-


(require 'persist)
(require 'dash)
(require 'cl-lib)
(require 'cl-macs)
(require 'pkm-new-core)
(require 'pkm-create-pkm-query)
(require 'pkm-create-pkm-query-ewoc)
(require 'pkm-compile-db-query)

(cl-defstruct pkm2-browse-node-state
  (show-hidden nil)
  (from nil :type marker)
  (to nil :type marker)
  (level 0 :type number)
  (show-children 0 :type number)
  (sort-children-by nil)
  (specifiers-for-children nil)
  (section nil))

(cl-defstruct pkm2-browse-node
  pkm-node
  (browse-id nil)
  (state nil :type browse-node-state)
  (children nil)
  (dependents nil)
  (parents nil)
  (parent nil))

(cl-defstruct pkm2-browse-buffer-state
  (sections nil)
  ; (buffer-nodes nil)
  ; (sections-nodes nil)
  )

(cl-defstruct pkm2-browse-section
  (section-id nil)
  (spec nil)
  (state nil)
  (show-as-hierarchy t)
  (show-active-specifiers nil)
  (from nil :type marker)
  (to nil :type marker))



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




(defun pkm2-browse (&optional numeric-prefix-argument)
  (interactive "p")
  (message "prefix: %S" numeric-prefix-argument)
  (pcase numeric-prefix-argument
    (1 (let* ((completing-read-choices (-concat '("NEW" "LAST") pkm2-browse-saved-named-section-specs (-map (lambda (section-spec) (cons  section-spec section-spec)) pkm2-browse-saved-section-specs)))
              (chosen-sections (if (length> completing-read-choices 1)
                                   (completing-read-multiple "Which sections would you like to add to this section: "
                                                             completing-read-choices)
                                 (list "NEW")))
              (sections-specs (-non-nil (-map (lambda (chosen-section)
                                                (cond ((equal "NEW" chosen-section) (pkm2--create-section-spec))
                                                      ((equal "LAST" chosen-section) nil)
                                                      (t (read (assoc-default chosen-section completing-read-choices)))))
                                              chosen-sections) ))

              (sections-specs (if (member "LAST" chosen-sections)
                                  (--> (plist-get pkm2-browse-buffer-states-equal-plist pkm2-browse-buffer #'equal) (pkm2-browse-buffer-state-sections it) (-map #'pkm2-browse-section-spec it) (-concat sections-specs it))
                                sections-specs))
              (sections (-map (lambda (sections-spec)
                                (make-pkm2-browse-section :spec  sections-spec))
                              sections-specs)))
         (message "sections-spec: %S, chosed-sections: %S" sections-specs chosen-sections)
         (pkm2--browse (make-pkm2-browse-buffer-state :sections sections) nil)))
    (0 (let* ((query-create-callback (lambda (browse-spec)
                                       (let* ((sections (-map (lambda (section-spec)
                                                                (make-pkm2-browse-section :spec  section-spec))
                                                              (plist-get browse-spec :sections)))
                                              (browse-state (make-pkm2-browse-buffer-state :sections sections)))
                                         (message "b-s2: %S" browse-state)
                                         (pkm2--browse browse-state nil)))))
         (pkm-compile-create-browse-spec query-create-callback)))
    (2 (let* ((browse-spec (--> (completing-read "Which Browse spec?" pkm2-browse-saved-named-browse-specs )
                           (assoc-default it pkm2-browse-saved-named-browse-specs)
                           (read it)))
         (sections (-map (lambda (section-spec)
                           (make-pkm2-browse-section :spec  section-spec))
                         (plist-get browse-spec :sections)))
         (browse-state (make-pkm2-browse-buffer-state :sections sections)))
         (pkm2--browse browse-state nil)))))




(defun pkm2--browse (buffer-state &optional buffer-name )
  (setq buffer-name (or buffer-name pkm2-browse-buffer))
  (pcase (pkm2-browse--buffer--visibility buffer-name)
    ('visible
     (progn
       (message "Already visible")))
    (_
     (display-buffer-in-side-window (get-buffer-create buffer-name) '((side . right)))
     (with-current-buffer buffer-name
       ; (outline-mode)
       (+word-wrap-mode )
       (pkm2-browse-mode)
       (tooltip-mode))))
  (with-current-buffer buffer-name
    (set-window-dedicated-p (get-buffer-window buffer-name) t)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq pkm2-browse--browse-nodes-alist ())
    (setq pkm2-browse--browse-sections-alist ())
    (setq pkm2-browse-buffer-states-equal-plist (plist-put pkm2-browse-buffer-states-equal-plist buffer-name buffer-state #'equal))
    (-each-indexed
        (pkm2-browse-buffer-state-sections buffer-state)
      (lambda (index section)
        (when (> index 0)
          (insert "\n----------------\n"))
        (pkm2-browse--insert-section section)))
    (setq buffer-read-only t) ))



(defun pkm2--browse-create-new-id (browse-nodes)
  (--> (-map #'car browse-nodes) (if it (-max it) -1)  (+ 1 it)))

(defun pkm2-browse--get-nodes (section-spec)
  (if section-spec
      (--> (pkm2--browse-get-section-nodes section-spec)
           (-distinct it)
                                        ; (progn (message "out nodes: %S" it) it)
           (-map (lambda (id)
                   (make-pkm2-browse-node :pkm-node (pkm2--db-query-get-node-with-id id) :state (make-pkm2-browse-node-state))) it))
    (error "Received nil as section-spec")))
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

(defun pkm2-browse--insert-section (section)
  (setq buffer-read-only nil)
  (let* ((nodes-spec (pkm2-browse-section-spec section))
         (section-state (pkm2-browse-section-state section))
         (show-as-hierarchy (pkm2-browse-section-show-as-hierarchy section))
         (section-nodes (pkm2-browse--get-nodes nodes-spec))
         (section-nodes-in-hierarchy  (when show-as-hierarchy
                                        (pkm2-browse--organize-into-hierarchies section-nodes)))
         (sorter nil)
         (sorted-sorted-nodes (-->  (or section-nodes-in-hierarchy section-nodes)
                                    (if sorter
                                        (funcall sorter it)
                                      it)))
         (from (set-marker (make-marker) (point)))
         (section-id (or (pkm2-browse-section-section-id section)
                         (setf (pkm2-browse-section-section-id section) (pkm2--browse-create-new-id pkm2-browse--browse-sections-alist))))
         (to (progn (-each-indexed sorted-sorted-nodes (lambda (index b-n)
                                                         (when (> index 0)
                                                           (insert "\n"))
                                                         (pkm2-browse--insert-browse-node b-n) ))
                    (set-marker (make-marker) (point)))))
    (setf (pkm2-browse-section-from section) from)
    (setf (pkm2-browse-section-to section) to)
    (setf (pkm2-browse-section-state section) nil)
    (--> (-map #'pkm2-browse-node-state section-nodes)
         ( -each it (lambda (state)
                      (setf (pkm2-browse-node-state-section state) section))))

    (setq pkm2-browse--browse-sections-alist (assoc-delete-all section-id pkm2-browse--browse-sections-alist) )
    (push (cons section-id section) pkm2-browse--browse-sections-alist)
    (put-text-property from to :pkm2-browse-section-id section-id)
    (put-text-property from to :pkm2-browse-section section)
    ;; (-each section-state (lambda (action)
    ;;                        (message "action: %S" action)
    ;;                        (pcase (plist-get action :action)
    ;;                          ('show-children (pkm2--browse-see-children nil (plist-get action :node-db-id) (plist-get action :level-children) )))))
    ))

(defun pkm2-browse--insert-browse-node (browse-node &optional insert-func only-node)
  (setq buffer-read-only nil)
  (let* ((state (or (pkm2-browse-node-state browse-node) (setf (pkm2-browse-node-state browse-node)(make-pkm2-browse-node-state))))
         (level (pkm2-browse-node-state-level state))
         (prefix (concat (propertize " " 'display `(space :width (,(*  20 (abs (or level 0) )))))))
         (prefix2 (format "%s " (make-string (+ 1 level ) ?*) ))
         (from (set-marker (make-marker) (point)))
         (pkm-node (pkm2-browse-node-pkm-node browse-node))
         (insert-func (or insert-func
                          (--> (pkm2-node-types pkm-node)
                               (-map (lambda (type)
                                       (plist-get (plist-get pkm-structure-defined-schemas-plist type) :browse-insert-func ))
                                     it)
                               (-non-nil it)
                               (-distinct it)
                               (progn (when (length> it 1)
                                        (message "there should be warning")
                                        (display-warning 'pkm2-browse (format "Node has multiple insert funcs: %S"   (pkm2-node-types (pkm2-browse-node-pkm-node browse-node) ) )))
                                      (car it)))
                          #'pkm2-browse--default-insert-browse-node))
         (browse-insert-format-string (--> (pkm2-node-types pkm-node)
                                           (-map (lambda (type)
                                                   (plist-get (plist-get pkm-structure-defined-schemas-plist type) :browse-insert-format-string))
                                                 it)
                                           (-non-nil it)
                                           (-distinct it)
                                           (progn (when (length> it 1)
                                                    (message "there should be warning")
                                                    (display-warning 'pkm2-browse (format "Node has multiple insert funcs: %S"   (pkm2-node-types (pkm2-browse-node-pkm-node browse-node) ) )))
                                                  (car it))))
         (to (progn (insert prefix2)
                    (if browse-insert-format-string
                        (insert (pkm2--browse-format-insert browse-insert-format-string browse-node))
                      (funcall insert-func browse-node) )
                    (set-marker (make-marker) (point))))
         (id (or (pkm2-browse-node-browse-id browse-node)
                 (setf (pkm2-browse-node-browse-id browse-node) (pkm2--browse-create-new-id pkm2-browse--browse-nodes-alist))))
         (children-browse-nodes (pkm2-browse-node-children browse-node))
         (parents-browse-nodes (pkm2-browse-node-parents browse-node)))
    (setf (pkm2-browse-node-state-from state) from)
    (setf (pkm2-browse-node-state-to state) to)
    (setq pkm2-browse--browse-nodes-alist (assoc-delete-all id pkm2-browse--browse-nodes-alist) )
    (push (cons id browse-node) pkm2-browse--browse-nodes-alist)
                                        ; (put-text-property from to 'wrap-prefix prefix)
                                        ; (put-text-property from to 'line-prefix prefix)
    (--> (pkm2-browse-node-browse-id browse-node)
         (put-text-property from to :pkm2-browse-node-id it) )
    (--> (pkm2-browse-node-pkm-node browse-node) (pkm2-node-db-node it) (pkm2-db-node-id it)
         (put-text-property from to :pkm2-node-db-id it))
    (put-text-property from to :pkm2-browse-node browse-node)
    (when (and children-browse-nodes (not only-node))
      (-each children-browse-nodes (lambda (c-b-n)
                                     (insert "\n")
                                     (pkm2-browse--insert-browse-node c-b-n)) ))
    (when (and parents-browse-nodes (not only-node))
      (-each parents-browse-nodes (lambda (p-b-n)
                                    ; The seperator here can be better. Not the cleanest thing in the world
                                    (insert "\nParent\n")
                                    (pkm2-browse--insert-browse-node p-b-n)
                                    ))
      )))


(defun pkm2-browse--default-kvd-display-string (kvd)
  (let* ((key (pkm2-db-kvd-key kvd))
         (value (pkm2-db-kvd-value kvd))
         (types (doom-plist-keys pkm-data-type-to-kvd-key-plist))
         (type (or (-find (lambda (type) (member key (plist-get pkm-data-type-to-kvd-key-plist type))) types) (pkm2-db-kvd-type kvd) )))
    (propertize (format "%s: %s" (pkm2--db-convert-object-to-string key) (pkm2--convert-object-to-string value type)) :db-kvd kvd)))

(defun pkm2-browse--default-hidden-info-string (browse-node)
  (let* ((browse-id (pkm2-browse-node-browse-id browse-node))
         (pkm-node (pkm2-browse-node-pkm-node browse-node))
         (db-id (--> (pkm2-node-db-node pkm-node) (pkm2-db-node-id it)))
         (kvds (pkm2-node-kvds pkm-node))
         (hidden-string (concat
                         (--> (-map #'pkm2-browse--default-kvd-display-string kvds)
                              (string-join it "\n")
                              (format "\n%s" it))
                         "\n"
                         (format "id: %d" db-id)
                         "\n"
                         (format "browse-id: %d" browse-id)
                         )))
    hidden-string))

(defun pkm2-browse--default-insert-browse-node (browse-node)
  (let* ((pkm-node (pkm2-browse-node-pkm-node browse-node))
         (db-node (pkm2-node-db-node pkm-node))
         (content (pkm2-db-node-content db-node))
         (state (pkm2-browse-node-state browse-node))
         (show-hidden (pkm2-browse-node-state-show-hidden state))
         (string-to-display (concat (format "%s"  (propertize (pkm2--convert-object-to-string content) :db-node db-node))
                                    (when show-hidden
                                      (pkm2-browse--default-hidden-info-string browse-node)))))
    (insert  string-to-display)))


(defun pkm2--browse-convert-object-to-string (object)
  (cond ((stringp object) object)
        ((numberp object) (format "%d" object))))

(defun pkm2-browse--refresh-insert-browse-node (browse-node)
  (let* ((state (pkm2-browse-node-state browse-node))
         (from (pkm2-browse-node-state-from state))
         (to (pkm2-browse-node-state-to state)))
    (when (and from to)
      (setq buffer-read-only nil)
      (goto-char from)
      (delete-region from to)
      (set-marker from nil)
      (set-marker to nil)
      (pkm2-browse--insert-browse-node browse-node nil t)
      (setq buffer-read-only nil))))



(defun pkm2-browse--buffer--visibility (&optional buffer-name)
  "Return the current visibility state of the persistent `pkm2-browse-buffer'.
Valid states are 'visible, 'exists and 'none."

  (setq buffer-name (or buffer-name pkm2-browse-buffer))
  (cond
   ((get-buffer-window buffer-name) 'visible)
   ((get-buffer buffer-name) 'exists)
   (t 'none)))

(defun pkm2-browse--buffer-toggle ()
  "Toggle display of the persistent `pkm2-browse-buffer'."
  (interactive)
  (pcase (pkm2-browse--buffer--visibility)
    ('visible
     (progn
       (quit-window nil (get-buffer-window pkm2-browse-buffer))))
    ((or 'exists 'none)
     (progn
       (display-buffer (get-buffer-create pkm2-browse-buffer))
       (pkm2-browse-buffer-persistent-redisplay)))))

(defun pkm2-browse-buffer-persistent-redisplay ()
  "Recompute contents of the persistent `pkm2-browse-buffer'.
Has no effect when there's no `org-roam-node-at-point'."
  (with-current-buffer (get-buffer-create pkm2-browse-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      )))

(defun pkm2--browse-remove-node (browse-id)
  ; Remove node from pkm2-browse--browse-nodes-alist
  ; Remove node from section pkm2-browse--browse-sections-alist
  ; Remove node from its children
  ; Remove node from its parent
  ; Also delete children
  (let* ((browse-node (assoc-default browse-id pkm2-browse--browse-nodes-alist))
         (state (pkm2-browse-node-state browse-node))
         (section (pkm2-browse-node-state-section state))
         (parent (pkm2-browse-node-parent browse-node))
         (children (pkm2-browse-node-children browse-node))
         (from (pkm2-browse-node-state-from state) )
         (to (pkm2-browse-node-state-to state)))
    (when children
      (--> (-map #'pkm2-browse-node-browse-id children)
           (progn (message "%S" it)
                  it)
           (-each it  #'pkm2--browse-remove-node)))
    (when parent
      (setf (pkm2-browse-node-children parent) (-filter (lambda (child)
                                                          (not (equal browse-id (pkm2-browse-node-browse-id child))))
                                                        (pkm2-browse-node-children parent))))
    (when (and browse-node from to)
      (setq buffer-read-only nil)
      (delete-region from to)
      (set-marker from nil)
      (set-marker to nil)
      (setq pkm2-browse--browse-nodes-alist (assq-delete-all browse-id pkm2-browse--browse-nodes-alist))
      (setq buffer-read-only t))
    (unless (and browse-node from to)
      (error (format "no browse-node for node %S\n %S %S %S" browse-id browse-node from to)))))

(defun pkm2--browse-get-section-nodes (section-spec)
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
         (pkm-query `((:or db-node (:db-id ,db-id))
                      (:convert-and convert-to-parents (:levels ,level-parents :link-labels ("sub") ))))

         (query (pkm2--compile-full-db-query pkm-query))
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
         (potential-keys (or (-flatten (-map (lambda (type)
                                               (-map (lambda (key)
                                                       (cons key type))
                                                     (plist-get pkm-data-type-to-kvd-key-plist type)))
                                             (doom-plist-keys pkm-data-type-to-kvd-key-plist )))
                             (pkm2--db-query-get-all-keys-to-types-alist) ))
         (key (completing-read "What key would you like to add?" potential-keys nil 'confirm))
         (type (assoc-default key potential-keys))
         (value (-->
                 (pkm2--db-query-get-all-values-with-key key type)
                 (-map #'pkm2--browse-convert-object-to-string it)
                 (completing-read "What value?" it nil 'confirm)
                 (if (or (eq type 'REAL)
                         (eq type 'INTEGER))
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
  ;  TODO Complete and test
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
    ; TODO move node up in hierarchy in browser
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
         (section (assoc-default section-id pkm2-browse--browse-sections-alist)))
    (pkm-browse-section-add-query section)
    (pkm2--refresh-current-buffer buffer-name)))


;;; hierarchy
;;

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
                               (--> (pkm2-browse-node-state b-n) (setf (pkm2-browse-node-state-level it) 0))))
    (-each-indexed browse-nodes (lambda (index p-b-n)
                                  (when-let ((p-b-n-level (--> (pkm2-browse-node-state p-b-n) (pkm2-browse-node-state-level it)))
                                             (children-indexes-list  (nth index present-children-indexes))
                                             (children-browse-nodes (-map (lambda (index)
                                                                            (nth index browse-nodes))
                                                                          children-indexes-list))
                                        ; Only modify child nodes that don't already have parent
                                        ; In the future, I might want to duplicate here
                                             (c-b-n-with-no-parent (-filter (lambda (c-b-n)
                                                                              (not (pkm2-browse-node-parent c-b-n)))
                                                                            children-browse-nodes)))
                                    (setf (pkm2-browse-node-children p-b-n) c-b-n-with-no-parent)
                                    (-each c-b-n-with-no-parent (lambda (c-b-n) (setf (pkm2-browse-node-parent c-b-n) p-b-n)))
                                    (-each c-b-n-with-no-parent (lambda (c-b-n) (--> (pkm2-browse-node-state c-b-n) (setf (pkm2-browse-node-state-level it) (+ p-b-n-level 1))))))))
    parent-less-nodes))

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
                               (-->  (pkm2-node-db-node pkm-node) (propertize (pkm2--convert-object-to-string (pkm2-db-node-content it))
                                                                              :db-node it)))
                              ((equal 'hidden (plist-get spec :display))
                               (if show-hidden
                                   (concat (plist-get spec :prefix) (pkm2-browse--default-hidden-info-string browse-node) )
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



(defun pkm-browse-section-refresh (section-id)
  (error "Not implemented"))

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
         (nodes-ids (-flatten (sqlite-select pkm2-database-connection query) ))
         (nodes-texts-and-id (sqlite-select pkm2-database-connection (format "SELECT content, id  FROM node WHERE id IN (%s)"
                                                                             (--> (-map (lambda (node-id)
                                                                                          (pkm2--db-format-value-correctly node-id 'INTEGER))
                                                                                        nodes-ids)
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
