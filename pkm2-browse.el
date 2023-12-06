;;; pkm2-browse.el -*- lexical-binding: t; -*-


(require 'persist)
(require 'dash)
(require 'cl-lib)
(require 'cl-macs)
(require 'pkm-new-core)
(require 'pkm2-compile-queries)
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
  ;; (buffer-nodes nil)
  ;; (sections-nodes nil)
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
(persist-defvar pkm2-browse-saved-queries () "Individual queries to get pkm nodes")
(persist-defvar pkm2-browse-saved-section-specs () "Created Section specs")
(unless  pkm2-browse-saved-section-specs (persist-load 'pkm2-browse-saved-queries) )
(unless  pkm2-browse-saved-section-specs (persist-load 'pkm2-browse-saved-section-specs) )

(defvar-local pkm2-browse--browse-nodes-alist ())
(defvar-local pkm2-browse--browse-sections-alist ())

(defun pkm2-browse (&optional numeric-prefix-argument)
  (interactive "p")
  (let* ((sections-specs (if pkm2-browse-saved-section-specs
                             (-map (lambda (spec)
                                     (if (equal spec "NEW")
                                         spec
                                       (read spec))
                                     ) (completing-read-multiple "What sections-specs to display: " (-concat '("NEW") pkm2-browse-saved-section-specs )) )
                           (list "NEW")))
         (sections-specs (if (member "NEW" sections-specs)
                             (--> (pkm2--create-section-spec) (-concat (list it) (-filter (lambda (spec)
                                                                                            (not (equal "NEW" spec))) sections-specs )))
                           sections-specs))
         (sections (-map (lambda (sections-spec)
                           (make-pkm2-browse-section :spec  sections-spec))
                         sections-specs)))
    (message "sections-spec: %S" sections-specs)
    (pkm2--browse (make-pkm2-browse-buffer-state :sections sections) nil)))


(defvar pkm2-browse-buffer-states-equal-plist ())

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
           ;; (progn (message "out nodes: %S" it) it)
           (-map (lambda (id)
                   (make-pkm2-browse-node :pkm-node (pkm2--db-query-get-node-with-id id) :state (make-pkm2-browse-node-state))) it))
    (--> (pkm2--db-query-get-all-nodes-ids)
         (-map (lambda (id)
                 (make-pkm2-browse-node :pkm-node (pkm2--db-query-get-node-with-id id))) it))))
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
    (-each section-state (lambda (action)
                           (message "action: %S" action)
                           (pcase (plist-get action :action)
                             ('show-children (pkm2--browse-see-children nil (plist-get action :node-db-id) (plist-get action :level-children) )))))))

(defun pkm2-browse--insert-browse-node (browse-node &optional insert-func only-node)
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
         (value (pkm2-db-kvd-value kvd)))
    (propertize (format "%s: %s" (pkm2--db-convert-object-to-string key) (pkm2--db-convert-object-to-string value)) :db-kvd kvd)))

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



(defun pkm2-browse--add-another-query-to-section ()
  (interactive)
  (let* ((section (get-text-property (point) :pkm2-browse-section))
         (new-query (--> (pkm2--create-section-query ) (read it) ))
         (section-spec (pkm2-browse-section-spec section))
         (old-queries (plist-get section-spec :queries))
         (all-queries (-concat old-queries (list new-query)))
         (new-section-spec (plist-put section-spec :queries all-queries)))
    (setf (pkm2-browse-section-spec section) new-section-spec )))


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
  ;; Remove node from pkm2-browse--browse-nodes-alist
  ;; Remove node from section pkm2-browse--browse-sections-alist
  ;; Remove node from its children
  ;; Remove node from its parent
  ;; Also delete children
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

(provide 'pkm2-browse)
