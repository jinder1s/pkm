;;; pkm2-browse.el -*- lexical-binding: t; -*-


(require 'dash)
(require 'cl-lib)
(require 'cl-macs)
(require 'pkm-new-core)
(require 'pkm-create-pkm-query)
(require 'pkm-create-pkm-query-ewoc)
(require 'pkm-compile-db-query)
(require 'ewoc)

(defclass pkm-browse-base ()
  ((start-ewoc :initarg :start-ewoc :initform nil)
  (end-ewoc :initarg :end-ewoc :initform nil)
  (datum :initarg :datum :initform nil) ))

(defclass pkm-browse-node (pkm-browse-base)
  ((browse-id :initarg :browse-id :initform nil)
   (section :initarg :section :initform nil)
   (level :initarg :level :initform nil)
   (show-hidden :initarg :show-hidden :initform nil)
   (children-ids :initarg :children-ids :initform nil)
   (parents-ids :initarg :parents-ids :initform nil)
   (parent-id :initarg :parent-id :initform nil)
   (parent-link :initarg :parent-link :initform nil) ))



(defclass pkm-browse-buffer (pkm-browse-base)
  ((sections :initarg :sections :initform nil)))


(defclass pkm-browse-section (pkm-browse-base)
  ((section-id :initarg :section-id :initform nil)
   (spec :initarg :spec :initform nil)
   (sorter :initarg :sorter :initform nil)
   (state :initarg :state :initform nil)
   (name :initarg :name :initform nil)
   (hidden :initarg :hidden :initform nil)
   (show-as-hierarchy :initarg :show-as-hierarchy :initform t)
   (show-active-specifiers :initarg :show-active-specifiers :initform nil)))



;;;; Persistent buffer
(defvar pkm2-browse-buffer "pkm2-browse"
  "The persistent pkm2 buffer name. Must be surround with \"*\".")
(defvar pkm2-browse-buffer-states-equal-plist ())


(defvar pkm2-browse-saved-section-specs () "Created Section specs")
(defvar pkm2-browse-saved-named-section-specs nil "Created Section specs")
(defvar pkm2-browse-saved-named-queries nil "Created queries")
(defvar pkm2-browse-saved-named-browse-specs nil)


(defvar-local pkm2-browse--browse-nodes-alist ())
(defvar-local pkm2-browse--browse-sections-alist ())
(defvar-local pkm2-browse-ewoc nil)



(defun pkm2-browse-get-section-with-name (name &optional choices)
  (--> (assoc-default name (or choices pkm2-browse-saved-named-section-specs ))
       (plist-put it :name name)))
(defun pkm2-browse-convert-section-spec-to-section (section-spec)
  (setq section-spec (cond ((stringp section-spec) (pkm2-browse-get-section-with-name section-spec))
                                        (t section-spec)))
  (pkm-browse-section :spec section-spec
                      :name (plist-get section-spec :name)
                      :hidden (plist-get section-spec :hidden)
                      :sorter (or (plist-get section-spec :sorter)
                                  #'pkm2-sort-by-created-on)))

(defun pkm2-browse (&optional numeric-prefix-argument)
  (interactive "p")
  (pcase numeric-prefix-argument
    (1 (let* ((completing-read-choices (-concat '("NEW" "LAST") pkm2-browse-saved-named-section-specs (-map (lambda (section-spec) (cons  section-spec section-spec)) pkm2-browse-saved-section-specs)))
              (chosen-sections (if (length> completing-read-choices 1)
                                   (completing-read-multiple "Which sections would you like to add to this section: "
                                                             completing-read-choices)
                                 (list "NEW")))
              (buffer-name (format "*%s-%s*" pkm2-browse-buffer (apply #'concat chosen-sections) ))
              (sections-specs (-non-nil (-map (lambda (chosen-section)
                                                (cond ((equal "NEW" chosen-section) (pkm2--create-section-spec))
                                                      ((equal "LAST" chosen-section) nil)
                                                      (t (pkm2-browse-get-section-with-name chosen-section completing-read-choices))))
                                              chosen-sections) ))

              (sections-specs (if (member "LAST" chosen-sections)
                                  (--> (plist-get pkm2-browse-buffer-states-equal-plist pkm2-browse-buffer #'equal)
                                       (oref it :sections)
                                       (-map (lambda (section) (oref section :spec)) it)
                                       (-concat sections-specs it))
                                sections-specs))
              (sections (-map (lambda (sections-spec)
                                (pkm2-browse-convert-section-spec-to-section sections-spec))
                              sections-specs)))
         (pkm2--browse (pkm-browse-buffer :sections sections) buffer-name)))
    (3 (let* ((query-create-callback (lambda (browse-spec)
                                       (let* ((sections (-map
                                                         #'pkm2-browse-convert-section-spec-to-section
                                                         (plist-get browse-spec :sections)))
                                              (browse-state (pkm-browse-buffer :sections sections)))
                                         (pkm2--browse browse-state nil)))))
         (pkm-compile-create-browse-spec query-create-callback)))
    (2 (let* ((browse-spec-name (completing-read "Which Browse spec?" pkm2-browse-saved-named-browse-specs ))
              (browse-spec (--> (assoc-default browse-spec-name pkm2-browse-saved-named-browse-specs)
                                 (cond ((stringp it) (read it))
                                       ((listp it) it))))
              (buffer-name (format "*%s-%s*" pkm2-browse-buffer browse-spec-name ))
              (sections (-map #'pkm2-browse-convert-section-spec-to-section (plist-get browse-spec :sections)))
              (browse-state (pkm-browse-buffer :sections sections)))
         (pkm2--browse browse-state buffer-name)))))


(defun pkm2--browse (buffer-state &optional buffer-name)
  (let* ((buffer-name (or buffer-name (format "*%s-%s*" pkm2-browse-buffer (pkm-random-id))))
         (buffer (get-buffer-create buffer-name))
         (inhibit-read-only t))
    ;; (display-buffer-in-side-window (get-buffer-create buffer-name) '((side . right)))
    (with-current-buffer buffer-name
      ;; (set-window-dedicated-p (get-buffer-window buffer-name) t)
      (+word-wrap-mode )               ; TODO remove this doom specific function
      (pkm2-browse-mode)
      (tooltip-mode)
      (erase-buffer)
      (setq pkm2-browse-ewoc nil)
      (setq pkm2-browse-ewoc (ewoc-create #'pkm-browse--insert-ewoc-item nil nil))
      (setq pkm2-browse--browse-nodes-alist ())
      (setq pkm2-browse--browse-sections-alist ())
      (setq pkm2-browse-buffer-states-equal-plist (plist-put pkm2-browse-buffer-states-equal-plist buffer-name buffer-state #'equal))
      (-each
          (oref buffer-state :sections)
        #'pkm2-browse--insert-section)
      (setq buffer-read-only t) )
    (persp-add-buffer  buffer-name)
    (display-buffer-same-window (get-buffer-create buffer-name) nil)))


(defun pkm2--browse-get-browse-id-of-node-with-db-id (db-id)
  (car (-find (lambda (b-n-cons) (equal
                                  db-id
                                  (--> (cdr b-n-cons)
                                       (oref it :id))))
              pkm2-browse--browse-nodes-alist) ) )
(defun pkm2-browse--organize-into-hierarchies (nodes)
  (pkm2-browse-hierarchy-organize-as-hierarchy nodes))


(defun pkm2-sort-by-created-on (browse-nodes)
  (-sort (lambda (b-n1 b-n2)
           (let ((b1-c (oref (oref b-n1 :datum) :created_at))
                 (b2-c (oref (oref b-n2 :datum) :created_at)))
             (when (and b1-c b2-c)
               (>= b1-c b2-c))))
         browse-nodes))

(defun pkm2-sort-by-modified-on (browse-nodes)
  (-sort (lambda (b-n1 b-n2)
           (let ((b1-c (oref (oref b-n1 :datum) :modified_at))
                 (b2-c (oref (oref b-n2 :datum) :modified_at)))
             (when (and b1-c b2-c)
               (>= b1-c b2-c))))
         browse-nodes))

(defvar section-nodes-sorting-functions  (list #'pkm2-sort-by-created-on #'pkm2-sort-by-modified-on ))



(defun pkm2-browse--insert-section (section)
  (when (not pkm2-browse-ewoc) (error (format "No ewoc, %S" (buffer-name) )))
  (let* ((nodes-spec (oref section :spec))
         (show-as-hierarchy (oref section :show-as-hierarchy))
         (pkm-nodes (--> (pkm2--browse-get-section-nodes-db-ids nodes-spec)
                         (-map #'pkm2--db-query-get-node-with-id it)))
         (sorter (oref section :sorter))
         (browse-nodes (when pkm-nodes (cond (show-as-hierarchy
                              (--> (pkm2-browse-hierarchy-organize-as-hierarchy2 pkm-nodes)
                                   (-map (lambda (branch)
                                           (pkm2-convert-pkm-nodes-tree-to-browse-nodes-tree branch section 0)) it)))
                             (t (-map (lambda (pkm-node)
                                        (pkm-browse-node :datum pkm-node :section section :level 0))
                                      pkm-nodes))) ))
         (sorted-sorted-nodes  (if sorter
                                  (pkm2-browse-sort-browse-nodes browse-nodes sorter)
                                browse-nodes) )
         (section-id (or (oref  section section-id)
                         (oset section :section-id (pkm-random-id))))
         (section-start-ewoc (when pkm-nodes (or
                                              (oref section :start-ewoc)
                                              (ewoc-enter-last pkm2-browse-ewoc (format "---%s---" (or (oref section :name) "section") )) ) ))
         (section-end-ewoc (when pkm-nodes (or
                                            (oref section :end-ewoc)
                                            (ewoc-enter-after pkm2-browse-ewoc section-start-ewoc "\n")) )))
    (-each sorted-sorted-nodes (lambda (b-n)
                                 (pkm2-browse--insert-browse-node b-n `(before . ,section-end-ewoc))))

    (oset section :start-ewoc section-start-ewoc)
    (oset section :end-ewoc section-end-ewoc)
    (setq pkm2-browse--browse-sections-alist (assoc-delete-all section-id pkm2-browse--browse-sections-alist) )
    (push (cons section-id section) pkm2-browse--browse-sections-alist)))


(defun pkm-browse--insert-ewoc-item (ewoc-item)
  (cond ((assoc-default ewoc-item pkm2-browse--browse-nodes-alist)
         (let* ((browse-node (assoc-default ewoc-item pkm2-browse--browse-nodes-alist))
                (level (oref browse-node :level))
                (prefix2 (concat (propertize " " 'display `(space :width ,(*  2 (abs (or level 0)))))))
                ;; (prefix2 (format "%s " (make-string (+ 1 level ) ?*) ))

                (pkm-node (oref browse-node :datum))
                (browse-insert-format-string
                 (or
                  (-->
                   (oref pkm-node :types)
                   (-map (lambda (type)
                           (let* ((schema (plist-get pkm-structure-defined-schemas-plist type) )
                                  (parents (plist-get schema :parents))
                                  (f-s (plist-get schema :browse-insert-format-string)))
                             (when f-s
                               (list type parents f-s))))
                         it)
                   (-non-nil it)
                   (-distinct it)
                   (-filter (lambda (info)
                              (unless (member (car info) (-flatten (-map #'cadr it)))
                                t))
                            it)
                   (progn (when (length> it 1)
                            (display-warning 'pkm2-browse (format "Node has multiple insert funcs: %S"   it )))
                          (car it))
                   (-last-item it))
                  "<insert>(:display content)</insert>"))
                (start (set-marker (make-marker) (point)))
                (end (progn
                       (insert prefix2)
                       (insert (pkm2--browse-format-insert browse-insert-format-string browse-node))
                       (set-marker (make-marker) (point))))
                (next-line (save-excursion (goto-char start)
                                           (forward-line)
                                           (set-marker (make-marker) (point)))) )
           (when (< next-line end) (indent-rigidly next-line end (* 4 level)) )))
        ((stringp ewoc-item) (insert ewoc-item))))

(defun pkm2-browse-insert-node-in-hierarchy (browse-node parent-node parent-link section &optional where)
  (let* ((b-n-id (oref browse-node :browse-id))
         (p-b-n-id (when parent-node (oref parent-node :browse-id) ))
         (parent-level (when parent-node (oref parent-node :level) ))
         (current-node-level (if parent-level (+ parent-level 1) 0))
         (parent-children (when parent-node (oref parent-node :children-ids) ))
         (insert-direction (if (listp where)
                               (car where)
                             where))
         (sibling-node (if (listp where)
                           (cdr where)
                         nil))
         (sibling-id (when sibling-node
                       (oref sibling-node :browse-id) ))
         anchor-ewoc-node
         ewoc-insert-direction
         new-parent-children)
    (cond
     ((and (eq insert-direction 'after) sibling-node)
      (setq anchor-ewoc-node (oref sibling-node :start-ewoc))
      (setq ewoc-insert-direction 'after)
      (setq new-parent-children (when parent-node (--> (-elem-index sibling-id parent-children)
                                                       (when it (-insert-at (+ it 1) b-n-id parent-children) )) ) ))
     ((and (eq insert-direction 'before) sibling-id )
      (setq anchor-ewoc-node (oref sibling-node :start-ewoc))
      (setq ewoc-insert-direction 'before)
      (setq new-parent-children (when parent-node (--> (-elem-index sibling-id parent-children)
                                                       (when it (-insert-at  it  b-n-id parent-children) )) ) ))
     ((eq insert-direction 'first)
      (setq anchor-ewoc-node (if parent-node
                                 (oref parent-node :start-ewoc)
                               (oref section :start-ewoc)))
      (setq ewoc-insert-direction 'after)
      (setq new-parent-children (-concat (list b-n-id) parent-children)))
     ((eq insert-direction 'last)
      (setq anchor-ewoc-node (if parent-node
                                 (--> (-last-item parent-children)
                                      (assoc-default it pkm2-browse--browse-nodes-alist)
                                      (oref it :start-ewoc))
                               (oref section :end-ewoc)))
      (setq ewoc-insert-direction 'before)
      (setq new-parent-children (-concat (list b-n-id) parent-children))))
    (when parent-link  (oset browse-node :parent-link parent-link) )
    (when parent-node  (oset parent-node :children-ids new-parent-children) )
    (when parent-node  (oset browse-node :parent-id p-b-n-id) )
     (oset browse-node :section section)
     (oset browse-node :level current-node-level)
    (push (cons b-n-id browse-node) pkm2-browse--browse-nodes-alist)
    (pkm2-browse--insert-browse-node browse-node (cons ewoc-insert-direction anchor-ewoc-node))))

(defun pkm2-browse--insert-browse-node (browse-node  &optional where only-node)
  (let* ((id (oref browse-node :browse-id))
         ewoc-node
         (children-browse-nodes (--> (oref browse-node :children-ids)
                                     (-map (lambda (c-b-n-id)
                                             (assoc-default c-b-n-id pkm2-browse--browse-nodes-alist))
                                           it)))
         (parents-browse-nodes (--> (oref browse-node :parents-ids)
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

    (oset browse-node :start-ewoc ewoc-node)
    (when (and children-browse-nodes (not only-node))
      (-each children-browse-nodes (lambda (c-b-n)
                                     (ewoc-goto-node pkm2-browse-ewoc ewoc-node)
                                     (pkm2-browse--insert-browse-node c-b-n `(after . ,ewoc-node)))))
    (when (and parents-browse-nodes (not only-node))
      (-each parents-browse-nodes (lambda (p-b-n)
                                        ; The seperator here can be better. Not the cleanest thing in the world
                                    (pkm2-browse--insert-browse-node p-b-n `(before . ,ewoc-node)))))))


(defun pkm2-browse--default-kvd-display-string (kvd)
  (let* ((key (oref kvd :key))
         (value (oref kvd :value))
         (types (doom-plist-keys pkm-data-type-to-kvd-key-plist))
         (type (or (-find (lambda (type) (member key (plist-get pkm-data-type-to-kvd-key-plist type))) types) (pkm2-db-kvd-type kvd) )))
    (propertize (format "%s: %s" (pkm2--convert-object-to-string key) (pkm2--convert-object-to-string value type)) :db-kvd kvd)))

(defun pkm2-browse--default-hidden-info-string (browse-node)
  (let* ((browse-id (oref browse-node :browse-id))
         (pkm-node (oref browse-node :datum))
         (db-id (oref pkm-node :id))
         (kvds (oref pkm-node :kvds))
         (level (oref browse-node :level))
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
                         (format "browse-id: %s" browse-id)
                         "\n"
                         prefix2
                         (format "types: %S" (oref pkm-node :types))
                         )))
    hidden-string))

(defun pkm2--browse-convert-object-to-string (object)
  (cond ((stringp object) object)
        ((numberp object) (format "%d" object))))

(defun pkm2-browse--refresh-insert-browse-node (browse-node)
  (let* ((ewoc-node (oref browse-node :start-ewoc))
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
                      ((pred pkm2-browse-node-p) (oref browse-node-or-id :browse-id))
                      ((pred stringp) browse-node-or-id)))
         (ewoc-node (oref browse-node :start-ewoc))
         (inhibit-read-only t)
         (parent-id (oref browse-node :parent-id))
         (parent-browse-node (assoc-default parent-id pkm2-browse--browse-nodes-alist))
         (children-ids (oref browse-node :children-ids))
         (parents-ids (oref browse-node :parents-ids)))
    (when (and (not only-current-node) children-ids )
      (-each children-ids  #'pkm2--browse-remove-node))
    (when (and (not only-current-node) parents-ids)
      (-each parents-ids  #'pkm2--browse-remove-node))
    (when parent-browse-node
      (oset parent-browse-node :children-ids
            (-filter (lambda (child-id)
                       (not (equal browse-id (pkm2-browse-node-browse-id (assoc-default child-id pkm2-browse--browse-nodes-alist)))))
                     (oref parent-browse-node :children-ids))))
    (ewoc-delete pkm2-browse-ewoc ewoc-node)
    (setq pkm2-browse--browse-nodes-alist (assq-delete-all browse-id pkm2-browse--browse-nodes-alist))
    nil
    ))

(defun pkm2--browse-get-section-nodes-db-ids (section-spec)
  (let* ((queries (plist-get section-spec :queries))
         (queries (-map (lambda (query)
                          (if (and (stringp query)
                                   (assoc-default query pkm2-browse-saved-named-queries))
                              (assoc-default query pkm2-browse-saved-named-queries)
                            query))
                        queries))
         (queries-db-query (-map #'pkm2--compile-full-db-query queries))
         (nodes-ids (-flatten (-map (lambda (query)
                                      (--> (sqlite-select pkm2-database-connection query)
                                           (-flatten it)))
                                    queries-db-query) )))
    nodes-ids))

(defun pkm2--browse-get-query-nodes (query-spec)
  (--> (pkm2--compile-full-db-query query-spec) (sqlite-select pkm2-database-connection it) (-flatten it)))


(defun pkm2--browse-section-next-sibling-node (&optional reverse)
  (let* ((current-node (pkm2--browse-get-browse-node-at-point))
         (level (when current-node (oref current-node :level) ) )
         (section (when current-node (oref current-node :section) ))
         (current-ewoc (when current-node (oref current-node :start-ewoc) ))
         (next-ewoc (when current-ewoc
                      (if reverse (ewoc-prev pkm2-browse-ewoc current-ewoc)
                        (ewoc-next pkm2-browse-ewoc current-ewoc))))
         found-next-ewoc-node
         found-next-browse-node)
    (while (and next-ewoc (not found-next-ewoc-node))
      (let* ((next-b-n-id (ewoc-data next-ewoc))
             (next-b-n (assoc-default next-b-n-id pkm2-browse--browse-nodes-alist))
             (n-section (when next-b-n
                          (oref next-b-n :section) ))
             (n-level (when next-b-n
                        (oref next-b-n :level) )))
        (if (and section (eq section n-section) )
            (if (and level (equal level n-level) )
                (progn (setq found-next-ewoc-node next-ewoc)
                       (setq found-next-browse-node next-b-n))
              (if (< level n-level)
                  (setq next-ewoc (if reverse (ewoc-prev pkm2-browse-ewoc next-ewoc)
                                    (ewoc-next pkm2-browse-ewoc next-ewoc) ))
                (setq next-ewoc nil)))
          (setq next-ewoc nil))))
    (when found-next-ewoc-node
      (ewoc-goto-node pkm2-browse-ewoc found-next-ewoc-node))
    found-next-browse-node))

(defun pkm2--browse-section-previous-sibling-node ()
  (pkm2--browse-section-next-sibling-node t))


(defun pkm2--browse-section-parent-node ())


;;; interactivity
(defun pkm2-browse-move-browse-node-at-point-up (&optional down)
  (let* ((current-node (pkm2--browse-get-browse-node-at-point))
         (parent-link (oref current-node :parent-link))
         (parent-node (--> (oref current-node :parent)
                           (assoc-default it pkm2-browse--browse-nodes-alist)))
         (section (oref current-node :section))
         (next-node (pkm2--browse-section-next-sibling-node (not down)))
         (direction (if down 'after 'before)))
    (when next-node
      (pkm2--browse-remove-node current-node)
      (pkm2-browse-insert-node-in-hierarchy current-node parent-node parent-link section `(,direction . ,next-node)))))

(defun pkm2-sort-funcs-in-current-section ()
  (let* ((current-section (pkm2--browse-get-section-at-point))
         (sort-fuction (intern (completing-read "What sorting func?" (-map #'symbol-name section-nodes-sorting-functions)))))
    (oset current-section :sorter sort-fuction)
    (pkm-browse-section-refresh current-section)))

(defun pkm2--browse-get-section-at-point ()
  (or
   (--> (pkm2--browse-get-browse-node-at-point)
        (when it (oref it :section)))
   (--> (when pkm2-browse-ewoc (ewoc-locate pkm2-browse-ewoc (point)))
        (when it
          (-find (lambda (sections-cons)
                   (or
                    (eq (oref (cdr sections-cons) :start-ewoc) it)
                    (eq (oref (cdr sections-cons) :end-ewoc) it)))
                 pkm2-browse--browse-sections-alist))
        (when it
          (cdr it)))))
(defun pkm2--browse-get-browse-node-at-point (&optional current-point)
  (if-let* ((browse-node-id (pkm2--browse-get-browse-node-id-at-point current-point) )
            (browse-node (assoc-default browse-node-id  pkm2-browse--browse-nodes-alist) ))
      browse-node))
(defun pkm2-get-pkm-node-at-point ()
  (--> (pkm2--browse-get-browse-node-at-point) (when it (oref it :datum) )))

(setq pkm2-get-pkm-node-at-point-func #'pkm2-get-pkm-node-at-point)

(defun pkm2--browse-get-browse-node-id-at-point (&optional current-point)
  (if-let* ((current-ewoc-node (when pkm2-browse-ewoc (ewoc-locate pkm2-browse-ewoc (or current-point (point)))))
            (browse-node-id (ewoc-data current-ewoc-node)))
      browse-node-id))

(defun pkm2--browse-get-browse-node-db-node-id-at-point (&optional current-point)
  (--> (pkm2--browse-get-browse-node-at-point)
       (oref it :id)
       
       ))

(defun pkm2--browse-toggle-hidden-info-at-point ()
  (interactive)
  (save-excursion
    (let* ((browse-node (pkm2--browse-get-browse-node-at-point))
           (show-hidden (oref browse-node :show-hidden))
           (new-show-hidden (not show-hidden)))
      (oset browse-node :show-hidden new-show-hidden)
      (pkm2-browse--refresh-insert-browse-node browse-node))))

(defun pkm2--browse-edit-object-at-point ()
  (interactive)
  (let* ((current-point (point))
         (browse-node (pkm2--browse-get-browse-node-at-point))
         (pkm-node (oref browse-node :datum))
         (db-node (get-text-property current-point :db-node))
         (kvd (get-text-property current-point :db-kvd)))
    (if  (not (xor db-node kvd) )
        (error "there should be one and only one type of object at point: either db-node or kvd.")
      (if db-node
          (let* ((old-content (oref db-node :content))
                 (new-content (cond ((numberp old-content) (read-number "New value: " old-content))
                                    ((stringp old-content) (read-string "New value: " old-content))))
                 (id (oref db-node :id))
                 (new-db-node (pkm2--update-node-with-log pkm-node id new-content (pkm2-get-current-timestamp))))
            (oset browse-node :datum (pkm2--db-query-get-node-with-id id)))
        (let* ((kvds (oref pkm-node :kvds))
               (key (oref kvd :key))
               (old-kvd-id (oref kvd :id))
               (type (assoc-default key (or (-flatten (-map (lambda (type)
                                                              (-map (lambda (key)
                                                                      (cons key type))
                                                                    (plist-get pkm-data-type-to-kvd-key-plist type)))
                                                            (doom-plist-keys pkm-data-type-to-kvd-key-plist )))
                                            (pkm2--db-query-get-all-keys-to-types-alist))))

               (prompt "What value?")
               (new-value (cond
                           ((equal type 'DATETIME) (pkm2-get-user-selected-timestamp "What time" (oref kvd :value)))
                           (t (--> (pkm2--db-query-get-all-values-with-key key type)
                                   (-map #'pkm2--browse-convert-object-to-string it)
                                   (completing-read "What value?" it nil 'confirm)
                                   (if (or (eq type 'REAL) (eq type 'INTEGER))
                                       (string-to-number  it)
                                     it)))))
               (context-id (pkm2-db-kvd-context-id kvd))
               (new-kvd (pkm2--db-get-or-insert-kvd key new-value type))
               (new-kvd-id (oref new-kvd :id))
               (old-link-id (pkm2-db-kvd-link-id kvd))
               (new-link (pkm2--update-node-kvd pkm-node kvd new-kvd-id type context-id old-link-id (pkm2-get-current-timestamp))))
          (oset new-kvd :link-id  (oref new-link :id))
          (oset new-kvd :context-id context-id)
          (oset pkm-node :kvds (-map (lambda (temp-kvd)

                                                  (if (equal (oref temp-kvd :id) (oref kvd :id))
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
         (ewoc-node (oref browse-node :start-ewoc))
         (base-level (oref browse-node :level))
         (section (oref browse-node :section))
         (parent-id (oref browse-node :parent-id))
         (parent-browse-node (assoc-default parent-id pkm2-browse--browse-nodes-alist))
         (db-id (oref (oref browse-node :datum ) :id))
         (level-children (or level-children (read (completing-read "How many levels of children would you like?" `("1" "2" "3" "4" "5" "6" "7" "ALL")) ) ))
         (query-spec `((:or db-node (:db-id ,db-id )) (:convert-or convert-to-children (:levels ,level-children) )) )
         (pkm-nodes (--> (pkm2--browse-get-query-nodes query-spec)
                         (-map #'pkm2--db-query-get-node-with-id it)))
         (browse-nodes (when pkm-nodes (--> (pkm2-browse-hierarchy-organize-as-hierarchy2 pkm-nodes)
                                            (-map (lambda (branch)
                                                    (pkm2-convert-pkm-nodes-tree-to-browse-nodes-tree branch section base-level)) it))))
         (inhibit-read-only t))


    (when (length= browse-nodes 0)
        (error "Looking for children resulted in loosing all nodes")
      )
    (when (length> browse-nodes 1)
      (error "Children should be in a hierarchy with only one root."))
    (pkm2-browse-insert-node-in-hierarchy (car browse-nodes) parent-browse-node nil section (or (save-excursion
                                                                                                  (ewoc-goto-node pkm2-browse-ewoc ewoc-node)
                                                                                                  (--> (pkm2--browse-section-previous-sibling-node)
                                                                                                       (when it (cons 'after it))))
                                                                                                (save-excursion
                                                                                                  (ewoc-goto-node pkm2-browse-ewoc ewoc-node)
                                                                                                  (--> (pkm2--browse-section-next-sibling-node)
                                                                                                       (when it (cons 'before it))))
                                                                                                'first))
    (pkm2--browse-remove-node browse-id)))


(defun pkm2--browse-see-parents (browse-id &optional level-parents)
  (let* ((browse-node (assoc-default browse-id pkm2-browse--browse-nodes-alist))
         (ewoc-node (oref browse-node :start-ewoc))
         (previous-sibling-browse-node (save-excursion
                                         (ewoc-goto-node pkm2-browse-ewoc ewoc-node)
                                         (pkm2--browse-section-previous-sibling-node)))
         (next-sibling-browse-node (save-excursion
                                     (ewoc-goto-node pkm2-browse-ewoc ewoc-node)
                                     (pkm2--browse-section-previous-sibling-node)))
         (base-level (oref browse-node :level))
         (section (oref browse-node :section))
         (parent-id (oref browse-node :parent-id))
         (parent-browse-node (assoc-default parent-id pkm2-browse--browse-nodes-alist))
         (db-id (oref (oref browse-node :datum ) :id))
         (level-parents (or level-parents (read (completing-read "How many levels of parents would you like?" `("1" "2" "3" "4" "5" "6" "7" "ALL")) ) ))
         (query-spec `((:or db-node (:db-id ,db-id)) (:convert-or convert-to-parents (:levels ,level-parents))))

         (pkm-nodes (--> (pkm2--browse-get-query-nodes query-spec)
                         (-map #'pkm2--db-query-get-node-with-id it)))

         (browse-nodes (when pkm-nodes (--> (pkm2-browse-hierarchy-organize-as-hierarchy2-parents pkm-nodes)
                                            (-map (lambda (branch)
                                                    (pkm2-convert-pkm-nodes-tree-to-browse-nodes-tree branch section base-level)) it))))
         (inhibit-read-only t))
    (when (length> browse-nodes 1)
      (error "Children should be in a hierarchy with only one root."))

    (pkm2-browse-insert-node-in-hierarchy (car browse-nodes) parent-browse-node nil section (or (save-excursion
                                                                                                  (ewoc-goto-node pkm2-browse-ewoc ewoc-node)
                                                                                                  (--> (pkm2--browse-section-previous-sibling-node)
                                                                                                       (when it (cons 'after it))))
                                                                                                (save-excursion
                                                                                                  (ewoc-goto-node pkm2-browse-ewoc ewoc-node)
                                                                                                  (--> (pkm2--browse-section-next-sibling-node)
                                                                                                       (when it (cons 'before it))))
                                                                                                'first))
    (pkm2--browse-remove-node browse-id)))



(defun pkm2--browse-see-parents-at-point ()
  (interactive)
  (pkm2--browse-see-parents (pkm2--browse-get-browse-node-id-at-point)))


(defun pkm2--browse-add-kvd-at-point ()
  (interactive)
  (let* ((current-point (point))
         (browse-node (pkm2--browse-get-browse-node-at-point))
         (pkm-node (oref browse-node :datum))
         (node-id (oref browse-node :id))
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
         (new-kvd-id (oref new-kvd :id))
         (new-link (pkm2--db-insert-link-between-node-and-kvd node-id new-kvd-id (pkm2-get-current-timestamp) type)))
    (oset new-kvd :link-id (oref new-link :id))
    (oset pkm-node :kvds (-concat (oref pkm-node :kvds) (list new-kvd)))))


(defun pkm2--browse-delete-link-to-kvd-at-point ()
  (interactive)
  (let* ((current-point (point))
         (browse-node (pkm2--browse-get-browse-node-at-point))
         (pkm-node (oref browse-node :datum))
         (kvd (get-text-property current-point :db-kvd))
         (type (pkm2-db-kvd-type kvd))
         (link-id (pkm2-db-kvd-link-id kvd))
         (kvds (oref pkm-node :kvds))
         (timestamp (pkm2-get-current-timestamp)))
    (pkm2--update-node-kvd pkm-node kvd nil type nil link-id timestamp)
    (oset pkm-node :kvds (-filter (lambda (temp-kvd)
                                    (if (equal (oref temp-kvd :id) (oref kvd :id))
                                        nil ; delete this kvd from node
                                      t))
                                  kvds))
    (pkm2-browse--refresh-insert-browse-node browse-node)))
(defun pkm2--browse-delete-link-to-parent-of-node-at-point ()
  (let* ((current-point (point))
         (browse-node (pkm2--browse-get-browse-node-at-point))
         (pkm-node (oref browse-node :datum))
         (db-id (oref pkm-node :id))
         (parent-browse-node (oref browse-node :parent-id))
         
         
         (parent-node-db-id (oref parent-browse-node :id))

         (links-between-nodes (--> (oref pkm-node :parent-links) (-filter (lambda (link)
                                                                                (and (equal (pkm2--link-get-link-child-id link) db-id)
                                                                                     (equal (pkm2--link-get-link-parent-id link) parent-node-db-id)))
                                                                              it)))
         (link (if (length= links-between-nodes 1)
                   (car links-between-nodes)
                 (error "There are either zero or more than 1 links between node and its parent")))
         (link-db-id (oref link :id)))
    (pkm2--db-delete-link-between-nodes link-db-id)))


(defun pkm2--refresh-current-buffer (&optional buffer-name)
  (interactive)
  (let* ((buffer-name (or buffer-name (buffer-name)))
         (buffer-state (plist-get pkm2-browse-buffer-states-equal-plist buffer-name #'equal))
         (sections (oref buffer-state :sections)))
    (-each sections (lambda (section)
                      (oset section :start-ewoc nil)
                      (oset section :end-ewoc nil)))
    (pkm2--browse buffer-state buffer-name)))

(defun pkm2--browse-add-search-child ()
  (interactive)
  (let* ((selected-node-id (pkm2-nodes-search "Select node to add as child: "))
         (browse-node (pkm2--browse-get-browse-node-at-point))
         
         
         (node-id (oref browse-node :id))
         (link-type-between-objects (intern (completing-read "What type of link do you want to make?" pkm-links-types) ))
         (link-label  (completing-read "What is the label for this link?" (plist-get pkm-links-type-to-label-eq-plist link-type-between-objects)))
         (new-link (pkm2--db-insert-link-between-parent-and-child
                    link-label
                    node-id
                    selected-node-id
                    (pkm2-get-current-timestamp))))
    new-link))

(defun pkm2--browse-add-search-parent ()
  (interactive)
  (let* ((selected-node-id (pkm2-nodes-search "Select node to add as parent "))
         (browse-node (pkm2--browse-get-browse-node-at-point))
         (pkm-node (oref browse-node :datum))
         (db-node (pkm2-node-db-node pkm-node))
         (node-id (oref db-node :id))
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
     (oref b-n :pkm-node)
     (pkm2-node-db-node it)
     (oref it :id)
     (pkm2--db-delete-node it))
    (pkm2--browse-remove-node b-n)))


(defun pkm2--browse-promote-node-at-point ()
  "Move node higher in its current hierarchy."
  (let* ((browse-node (pkm2--browse-get-browse-node-at-point))
         (section (oref browse-node :section))
         (pkm-node (oref browse-node :datum))
         (db-node (pkm2-node-db-node pkm-node))
         (node-id (oref db-node :id))
         (parent-browse-node (--> (oref browse-node :parent-id)
                                  (assoc-default it pkm2-browse--browse-nodes-alist)))
         (connecting-link (--> (oref browse-node :parent-link)))
         (connecting-link-id (oref connecting-link :id))
         (connecting-link-label  (pkm2-db-nodes-link-type connecting-link))
         (connecting-context-id (pkm2-db-nodes-link-context connecting-link))
         (grandparent-browse-node (--> (oref parent-browse-node :parent)
                                       (assoc-default it pkm2-browse--browse-nodes-alist)))
         (grandparent-pkm-node (oref grandparent-browse-node :datum))
         (grandparent-db-node (pkm2-node-db-node grandparent-pkm-node))
         (grandparent-node-id (oref grandparent-db-node :id))
         (new-link (pkm2--db-insert-link-between-parent-and-child connecting-link-label
                                                                  grandparent-node-id
                                                                  node-id
                                                                  (pkm2-get-current-timestamp)
                                                                  connecting-context-id))
         (inhibit-read-only t))
    (pkm2--db-delete-link-between-nodes connecting-link-id)
    (pkm2--browse-remove-node browse-node)
    (pkm2-browse-insert-node-in-hierarchy browse-node
                                          grandparent-browse-node
                                          new-link
                                          section `(after . ,parent-browse-node))))


(defun pkm2-browse-demote-node-at-point ()
  "Move node lower in hierarchy."
  ;; TODO TEST
  (let* ((browse-node (pkm2--browse-get-browse-node-at-point))
         (section (oref browse-node :section))
         (pkm-node (oref browse-node :datum))
         (db-node (pkm2-node-db-node pkm-node))
         (node-id (oref db-node :id))
         (connecting-link (--> (oref browse-node :parent-link)))
         (connecting-link-id (oref connecting-link :id))
         (connecting-link-label (pkm2-db-nodes-link-type connecting-link))
         (connecting-context-id (pkm2-db-nodes-link-context connecting-link))
         (sibling-above-browse-node (save-excursion
                                      (pkm2--browse-section-previous-sibling-node)))
         (sibling-above-pkm-node (oref sibling-above-browse-node :datum))
         (sibling-above-db-node (pkm2-node-db-node sibling-above-pkm-node))
         (sibling-above-node-id (oref sibling-above-db-node :id))
         (new-link (pkm2--db-insert-link-between-parent-and-child connecting-link-label
                                                                  node-id
                                                                  sibling-above-node-id
                                                                  (pkm2-get-current-timestamp)
                                                                  connecting-context-id))
         (inhibit-read-only t))
    (pkm2--db-delete-link-between-nodes connecting-link-id)
    (pkm2--browse-remove-node browse-node)
    (pkm2-browse-insert-node-in-hierarchy browse-node
                                          sibling-above-browse-node
                                          new-link
                                          section `last)))

(defun pkm2--browse-filter-children-nodes-at-point (&optional arg)
  (interactive "p")
  (let* ((browse-node (pkm2--browse-get-browse-node-at-point))
         (browse-id (oref browse-node :browse-id))
         (db-id (--> (oref browse-node :datum)
                     (pkm2-node-db-node it)
                     (oref it :id)))
         (c-b-ns (oref browse-node :children-ids))
         (c-b-n-ids (-map (lambda (c-b-n)
                            (oref c-b-n :id))
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
         (ewoc-node (oref browse-node :start-ewoc))
         (base-level (oref browse-node :level))
         (section (oref browse-node :section))
         (pkm-nodes (-map #'pkm2--db-query-get-node-with-id nodes-db-ids))
         (browse-nodes (when pkm-nodes (--> (pkm2-browse-hierarchy-organize-as-hierarchy2 pkm-nodes)
                                            (-map (lambda (branch)
                                                    (pkm2-convert-pkm-nodes-tree-to-browse-nodes-tree branch section base-level)) it))))
         (inhibit-read-only t))
    (when (length> browse-nodes 1)
      (error "Children should be in a hierarchy with only one root."))
    (-each browse-nodes (lambda (b-n-to-insert)
                          (pkm2-browse-insert-node-in-hierarchy b-n-to-insert browse-node nil section 'first)))))

(defun pkm2--browse-see-node-as-child (browse-id child-db-id)
  (pkm2--browse-see-nodes-as-children browse-id (list child-db-id)))


(defun pkm--browse-capture-node-as-child-of-node-at-point ()
  (interactive)
  (let* ((browse-node-id (pkm2--browse-get-browse-node-id-at-point))
         (browse-node (assoc-default browse-node-id pkm2-browse--browse-nodes-alist )))
    (-->
     (oref browse-node :datum)
     (pkm2-node-db-node it)
     (oref it :id)
     (pkm--object-capture-sub it))))


(defun pkm--browse-capture-node-as-sibling-of-node-at-point ()
  (interactive)
  (let* ((b-n (pkm2--browse-get-browse-node-id-at-point))
         (p-b-n (oref b-n :parent)))
    (-->
     (oref p-b-n :datum)
     (pkm2-node-db-node it)
     (oref it :id)
     (pkm--object-capture-sub it))))

(defun pkm2--narrow-to-node-and-children-at-point ()
  (interactive)
  (let* ((browse-node (pkm2--browse-get-browse-node-at-point))
         (db-id (--> (oref browse-node :datum)
                     (pkm2-node-db-node it)
                     (oref it :id)))
         (c-b-ns (oref browse-node :children-ids))
         (c-b-n-ids (-map (lambda (c-b-n)
                            (oref c-b-n :id))
                          c-b-ns))
         (all-ids (-concat (list db-id) c-b-n-ids))
         (sub-query (list :or 'db-nodes `(:db-node-ids ,all-ids)))
         (section-spec `(:queries ((,sub-query))))
         (section (pkm-browse-section :spec  section-spec) ))
    (pkm2--browse (pkm-browse-buffer :sections (list section)) nil)))

(defun pkm2--add-query-to-section-at-point ()
  (interactive)
  (let* ((section (pkm2--browse-get-section-at-point)))
    (pkm-browse-section-add-query section)
    (pkm2--refresh-current-buffer (buffer-name))))


;;; hierarchy
(defun pkm2-browse-hierarchy-organize-as-hierarchy (browse-nodes)
  (let* ((pkm-nodes (-map (lambda (b-n) (oref b-n :datum)) browse-nodes))
         (parent-links (-map (lambda (p-n) (oref p-n :parent-links))  pkm-nodes))
         (parent-db-ids-list (-map (lambda (p-links)
                                     (-map #'pkm2--link-get-link-parent-id p-links)) parent-links))
         (children-links (-map #'pkm2-node-children-links pkm-nodes))
         (children-db-ids-list (-map (lambda (c-links)
                                       (-map #'pkm2--link-get-link-child-id c-links)) children-links))
         (db-nodes (-map #'pkm2-node-db-node pkm-nodes))
         (db-ids (-map (lambda (db-node) (oref db-node :id)) db-nodes))
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
                               (oset b-n :level 0)))
    (-each-indexed browse-nodes (lambda (index p-b-n)
                                  (when-let ((p-b-n-level (oref p-b-n :level))
                                             (p-b-n-id (oref p-b-n :browse-id))
                                             (children-indexes-list  (nth index present-children-indexes))
                                             (children-browse-nodes (-map (lambda (index)
                                                                            (nth index browse-nodes))
                                                                          children-indexes-list))
                                        ; Only modify child nodes that don't already have parent
                                        ; In the future, I might want to duplicate here
                                             (c-b-n-with-no-parent (-filter (lambda (c-b-n)
                                                                              (not (oref c-b-n :parent)))
                                                                            children-browse-nodes))
                                             (c-b-n-with-no-parent-browse-node-ids (-map (lambda (c-b-n-no-p)
                                                                                           (oref c-b-n-no-p :browse-id))
                                                                                         c-b-n-with-no-parent)))
                                    (oset p-b-n :children-ids c-b-n-with-no-parent-browse-node-ids)
                                    (-each c-b-n-with-no-parent (lambda (c-b-n) (oset c-b-n :parent-id p-b-n-id)))
                                    (-each c-b-n-with-no-parent (lambda (c-b-n)
                                                                   (oset c-b-n :level (+ p-b-n-level 1)))))))
    parent-less-nodes))


(defun pkm2-browse-hierarchy-organize-children (pkm-node pkm-nodes)
  (let* ((children-links (oref pkm-node :children-links) )
         (pkm-node-children-node-db-ids (-map #'pkm2--link-get-link-child-id children-links))
         (present-children (-map (lambda (c-id)
                                   (-find (lambda (p-n)
                                            (equal (oref p-n :id)
                                                   c-id))
                                          pkm-nodes))
                                 pkm-node-children-node-db-ids))
         (present-children-organized (-map (lambda (present-child)
                                             (when present-child
                                               (pkm2-browse-hierarchy-organize-children present-child pkm-nodes) ))
                                           present-children) )
         (present-children-organized-with-links (-non-nil (-map-indexed
                                                           (lambda (index child)
                                                             (when child
                                                               (plist-put child :parent-link (nth index children-links))))
                                                           present-children-organized))))


    `(:node ,pkm-node :children ,present-children-organized-with-links)))

(defun pkm2-browse-hierarchy-organize-as-hierarchy2 (pkm-nodes)
  (let* ((parent-links (-map (lambda (p-n) (oref p-n :parent-links))  pkm-nodes))
         (parent-db-ids-list (-map (lambda (p-links)
                                     (-map #'pkm2--link-get-link-parent-id p-links))
                                   parent-links))
         (db-ids (-map (lambda (p-n) (oref p-n :id)) pkm-nodes))
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



(defun pkm2-browse-hierarchy-organize-parent (pkm-node pkm-nodes)
  (let* ((parents-links (oref pkm-node :parent-links))
         (pkm-node-parent-node-db-ids (-map #'pkm2--link-get-link-parent-id parents-links))
         (present-parents (-map (lambda (p-id)
                                   (-find (lambda (p-n)
                                            (equal (--> (pkm2-node-db-node p-n)
                                                        (oref it :id))
                                                   p-id))
                                          pkm-nodes))
                                 pkm-node-parent-node-db-ids))
         (present-parents-organized (-map (lambda (present-parent)
                                             (when present-parent
                                               (pkm2-browse-hierarchy-organize-parent present-parent pkm-nodes) ))
                                           present-parents) )
         (present-parents-organized-with-links (-non-nil (-map-indexed
                                                           (lambda (index parent)
                                                             (when parent
                                                               (plist-put parent :parent-link (nth index parents-links))))
                                                           present-parents-organized))))


    `(:node ,pkm-node :parents ,present-parents-organized-with-links)))

(defun pkm2-browse-hierarchy-organize-as-hierarchy2-parents (pkm-nodes)
  (let* ((children-links (-map #'pkm2-node-children-links pkm-nodes))
         (children-db-ids-list (-map (lambda (p-links)
                                     (-map #'pkm2--link-get-link-child-id p-links))
                                   children-links))
         (db-nodes (-map #'pkm2-node-db-node pkm-nodes))
         (db-ids (-map (lambda (db-node) (oref db-node :id)) db-nodes))
         (childless-pkm-nodes (--> (-map-indexed (lambda (index c-ids)
                                                    (cond ((not c-ids) index)
                                                          ((not (-any? (lambda (c-id)
                                                                         (member c-id db-ids)) c-ids))
                                                           index)))
                                                  children-db-ids-list)
                                    (-non-nil it)
                                    (-map (lambda (index)
                                            (nth index pkm-nodes)) it))))
    (-map (lambda (parentless-pkm-node) (pkm2-browse-hierarchy-organize-parent parentless-pkm-node pkm-nodes)) childless-pkm-nodes)))

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
         (children-node-ids (-map (lambda (c-n) (oref c-n :browse-id)) children-nodes))
         (parents-node-ids (-map (lambda (p-n) (oref p-n :browse-id)) parents-nodes))
         (id (pkm-random-id))
         (browse-node (pkm-browse-node :browse-id id
                                       :level level
                                       :children-ids children-node-ids
                                       :parents-ids parents-node-ids
                                       :section section
                                       :datum node
                                       :parent-link parent-link)))
    (-each (-concat parents-nodes children-nodes) (lambda (b-n)
                                                    (oset b-n :parent-id id) ))
    (push (cons id browse-node) pkm2-browse--browse-nodes-alist)
    browse-node))

(defun pkm2-browse-sort-browse-nodes (browse-nodes sorter-func)
   ;; (-each browse-nodes (lambda (b-n)
   ;;                       (oset b-n :children
   ;;                             (--> (oref b-n :children-ids)
   ;;                                  (-map (lambda (c-b-n-id)
   ;;                                          (assoc-default c-b-n-id pkm2-browse--browse-nodes-alist)) it)
   ;;                                  (pkm2-browse-sort-browse-nodes it sorter-func)
   ;;                                  (-map (lambda (b-n) (oref b-n :browse-id)) it)))
   ;;                       (oref b-n :parents
   ;;                             (--> (oref b-n :parents)
   ;;                                  (-map (lambda (p-b-n-id)
   ;;                                          (assoc-default p-b-n-id pkm2-browse--browse-nodes-alist)) it)
   ;;                                  (pkm2-browse-sort-browse-nodes it sorter-func)
   ;;                                  (-map (lambda (b-n) (oref b-n :browse-id)) it)))))
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
  (let* ((pkm-node (oref browse-node :datum))
         (show-hidden (oref browse-node :show-hidden))
         (types (oref pkm-node :types))
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
                                                      (oref kvd :value) )
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
                               (propertize (pkm2--convert-object-to-string (oref pkm-node :content))
                                           :db-node pkm-node))
                              ((equal 'hidden (plist-get spec :display))
                               (if show-hidden
                                   (concat (plist-get spec :prefix)
                                           (pkm2-browse--default-hidden-info-string browse-node) )
                                 "")))))
    (when (plist-get spec :face)
      (setq output-string (propertize output-string 'face (plist-get spec :face))))
    output-string ))

;;; browse-mode




(define-derived-mode pkm2-browse-mode special-mode "pkm2-browse"
  "Major mode to browse pkm2 database.
\\<pkm2-browse-mode-map>
sfdsadfas
\\{pkm2-browse-mode-map}
")

(define-keymap
  :keymap pkm2-browse-mode-map
  "<tab>" #'pkm2--browse-toggle-hidden-info-at-point
  "e e" #'pkm2--browse-edit-object-at-point


  "s r" #'pkm2--refresh-current-buffer
  "s q" #'pkm2--add-query-to-section-at-point
  "s e" #'pkm-browse-section-edit-spec
  "s s" #'pkm-browse-section-save-spec

  "b r" #'pkm2--refresh-current-buffer
  "b a" #'pkm2-browse-add-section
  "b s" #'pkm2-browse-save-browse-spec

  "n a r p" #'pkm2--browse-add-search-parent
  "x a r c" #'pkm2--browse-add-search-child

  "n a k" #'pkm2--browse-add-kvd-at-point
  "n a c" #'pkm--browse-capture-node-as-child-of-node-at-point
  "n a s" #'pkm--browse-capture-node-as-sibling-of-node-at-point

  "n c e" #'pkm2-clock-edit-clock-at-point
  "n c i" #'pkm2-clock-in
  "n c o" #'pkm2-clock-out
  "n c s" #'pkm2-clock-switch

  "n s c" #'pkm2--browse-see-children-at-point
  "n s n p" #'pkm2--browse-see-parents-at-point

  "n f" #'pkm2--browse-filter-children-nodes-at-point
  "n n" #'pkm2--narrow-to-node-and-children-at-point
  "n d" #'pkm2--browse-delete-node-at-point)



(defun pkm2-browse-add-section (&optional buffer-name)
  (interactive)
  (let* ((buffer-name (or buffer-name (buffer-name) ))
         (buffer-state (plist-get pkm2-browse-buffer-states-equal-plist buffer-name #'equal))
         (sections (oref buffer-state :sections))
         (completing-read-choices (-concat (list '("NEW" . "NEW") ) pkm2-browse-saved-named-section-specs))
         (chosen-section-name (if (length> completing-read-choices 1)
                                  (completing-read "Which section would you like to add: " completing-read-choices)
                                "NEW"))

         (chosen-section (assoc-default chosen-section-name completing-read-choices))
         (new-section (pkm-browse-section :spec  (if (equal "NEW" chosen-section)
                                                           (pkm2--create-section-spec)
                                                         (cond ((stringp chosen-section) (read chosen-section))
                                                               ((listp chosen-section) chosen-section)
                                                               (t (error "Something went wrong with chosen-section: %S" chosen-section))))))
         (new-sections (-concat sections (list new-section))))
    (oset buffer-state :sections new-sections)
    (with-current-buffer buffer-name
      (pkm2-browse--insert-section new-section))))

(defun pkm2-browse-save-browse-spec (&optional buffer-name)
  (interactive)
  (let* ((buffer-name (or buffer-name (buffer-name) ))
         (buffer-state (plist-get pkm2-browse-buffer-states-equal-plist buffer-name #'equal))
         (sections (--> (oref buffer-state :sections) (-map (lambda (section) (oref section :spec)) it)))
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
                                        (eq  (oref b-n :section)  section))
                                      it)
                             )))
    (-each section-nodes  (lambda (b-n) (pkm2--browse-remove-node b-n t)))
    (pkm2-browse--insert-section section)))

(defun pkm-browse-section-add-query (section)
  (let* ((section-spec (oref section :spec))
         (queries (plist-get section-spec :queries))
         (new-query (pkm2--create-section-query))
         (new-section-spec (plist-put section-spec :queries (-concat queries (list new-query)))))
    (oset section :spec new-section-spec)))

(defun  pkm-browse-section-edit-spec ()
  (interactive)
  (if-let*
      ((buffer-name (buffer-name))
       (section-id (get-text-property (point) :pkm2-browse-section-id))
       (section (assoc-default section-id pkm2-browse--browse-sections-alist))
       (section-spec (oref section :spec))
       (callback (lambda (browse-spec)
                   (oset section :spec (car (plist-get browse-spec :sections)))
                   (pkm2--refresh-current-buffer buffer-name))))
      (pkm-compile-create-browse-spec callback nil section-spec)
    (error (format "Somethign is nill: section-id: %S, section: %S, section-spec: %S" section-id section section-spec) )))

(defun pkm-browse-section-save-spec ()
  (interactive)
  (let* ((buffer-name (buffer-name))
         (section-id (get-text-property (point) :pkm2-browse-section-id))
         (section (assoc-default section-id pkm2-browse--browse-sections-alist))
         (section-spec (oref section :spec))
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
