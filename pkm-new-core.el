;;; pkm-new-core.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-generic)
(require 'cl-macs)
(require 'ts)
(defgroup pkm2 nil
  "Personal knowledge management package."
  :group 'outlines)

(require 'dash)

;;; pkm utils
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

(defun doom-plist-keys (plist)
  "Return the keys in PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    keys))
(defun mm-alist-to-plist (alist)
  "Convert association list ALIST into the equivalent property-list form.
The plist is returned.  This converts from

\((a . 1) (b . 2) (c . 3))

into

\(a 1 b 2 c 3)

The original alist is not modified."
  (let (plist)
    (while alist
      (let ((el (car alist)))
	(setq plist (cons (cdr el) (cons (car el) plist))))
      (setq alist (cdr alist)))
    (nreverse plist)))



;;; pkm db stuff




(cl-defstruct pkm2-db-kvd
  id
  (type 'TEXT)
  key
  value
  created_at
  (link-id nil)
  (context-id nil))

(cl-defstruct pkm2-db-kvd-link
  id2
  (type 'TEXT)
  node
  key_value_data
  context
  created_at
  is_archive)

(cl-defstruct pkm2-db-nodes-link
  id
  type
  node_a
  node_b
  context
  created_at)

(cl-defstruct pkm2-db-node
  id
  content
  created_at
  modified_at)

(cl-defstruct pkm2-node
  db-node
  (types nil :type list)
  (parent-links nil :type list)         ; links to node's parents (where node is child)
  (children-links nil :type list)       ; links to node's children (where node is parent)
  (previous-sequencial-links nil :type list) ; links to previous in sequence
  (next-sequencial-links nil :type list)     ; links to next in sequence
  (flat-links nil :type list)
  (kvds nil :type list))

(defvar pkm2-database-connection nil)

(defun pkm2--core-trace-sqlite-execute (DB QUERY &optional VALUES RETURN-TYPE)
  (message "Running Query: %S" QUERY))
(advice-add #'sqlite-execute :before #'pkm2--core-trace-sqlite-execute)
; (advice-add #'sqlite-select :before #'pkm2--core-trace-sqlite-execute)

(defun pkm2-setup-database (database_handle)
  "Set up database with initial schema.
DATABASE_HANDLE is object returned from `sqlite-open` function"
  (sqlite-pragma database_handle "foreign_keys = ON")

  (sqlite-execute database_handle "CREATE TABLE IF NOT EXISTS node (
  id INTEGER PRIMARY KEY NOT NULL,
  content,
  created_at INTEGER NOT NULL,
  modified_at INTEGER) ;")

  (sqlite-execute database_handle "CREATE TABLE IF NOT EXISTS nodes_link (
  id INTEGER PRIMARY KEY NOT NULL,
  type TEXT,
  created_at INTEGER NOT NULL,
  node_a INTEGER NOT NULL,
  node_b INTEGER NOT NULL,
  context INTEGER,
  is_archive INTEGER,
  FOREIGN KEY(context) REFERENCES node(id),
  FOREIGN KEY(node_a) REFERENCES node(id) ON DELETE CASCADE,
  FOREIGN KEY(node_b) REFERENCES node(id) ON DELETE CASCADE);")

  (sqlite-execute database_handle "CREATE TABLE IF NOT EXISTS key_value_data (
  id INTEGER PRIMARY KEY NOT NULL,
  created_at INTEGER NOT NULL,
  key TEXT NOT NULL,
  value TEXT);" )

  (sqlite-execute database_handle "CREATE TABLE IF NOT EXISTS key_value_data_integer (
  id INTEGER PRIMARY KEY NOT NULL,
  created_at INTEGER NOT NULL,
  key TEXT NOT NULL,
  value INTEGER) STRICT;" )

  (sqlite-execute database_handle "CREATE TABLE IF NOT EXISTS key_value_data_real (
  id INTEGER PRIMARY KEY NOT NULL,
  created_at INTEGER NOT NULL,
  key TEXT NOT NULL,
  value REAL) STRICT;" )
  (sqlite-execute database_handle "CREATE TABLE IF NOT EXISTS key_value_data_blob (
  id INTEGER PRIMARY KEY NOT NULL,
  created_at INTEGER NOT NULL,
  key TEXT NOT NULL,
  value BLOB) STRICT;")

  (sqlite-execute database_handle "CREATE TABLE IF NOT EXISTS data_or_properties_link (
  id INTEGER PRIMARY KEY NOT NULL,
  created_at INTEGER NOT NULL,
  node INTEGER,
  key_value_data INTEGER,
  context INTEGER,
  is_archive INTEGER,
  FOREIGN KEY(context) REFERENCES node(id),
  FOREIGN KEY(node) REFERENCES node(id) ON DELETE CASCADE,
  FOREIGN KEY(key_value_data) REFERENCES key_value_data(id) ON DELETE CASCADE);")

  (sqlite-execute database_handle "CREATE TABLE IF NOT EXISTS data_or_properties_link_integer (
  id INTEGER PRIMARY KEY NOT NULL,
  created_at INTEGER NOT NULL,
  node INTEGER,
  key_value_data INTEGER,
  context INTEGER,
  is_archive INTEGER,
  FOREIGN KEY(context) REFERENCES node(id),
  FOREIGN KEY(node) REFERENCES node(id) ON DELETE CASCADE,
  FOREIGN KEY(key_value_data) REFERENCES key_value_data_integer(id) ON DELETE CASCADE);")

  (sqlite-execute database_handle "CREATE TABLE IF NOT EXISTS data_or_properties_link_real (
  id INTEGER PRIMARY KEY NOT NULL,
  created_at INTEGER NOT NULL,
  node INTEGER,
  key_value_data INTEGER,
  context INTEGER,
  is_archive INTEGER,
  FOREIGN KEY(context) REFERENCES node(id),
  FOREIGN KEY(node) REFERENCES node(id) ON DELETE CASCADE,
  FOREIGN KEY(key_value_data) REFERENCES key_value_data_real(id) ON DELETE CASCADE);" )

  (sqlite-execute database_handle "CREATE TABLE IF NOT EXISTS data_or_properties_link_blob (
  id INTEGER PRIMARY KEY NOT NULL,
  created_at INTEGER NOT NULL,
  node INTEGER,
  key_value_data INTEGER,
  context INTEGER,
  is_archive INTEGER,
  FOREIGN KEY(context) REFERENCES node(id),
  FOREIGN KEY(node) REFERENCES node(id) ON DELETE CASCADE,
  FOREIGN KEY(key_value_data) REFERENCES key_value_data_blob(id) ON DELETE CASCADE);" )
  (sqlite-execute database_handle "CREATE VIRTUAL TABLE IF NOT EXISTS search_node USING fts5(
   node,
   content
   );")
  (sqlite-execute database_handle
                  "CREATE TRIGGER IF NOT EXISTS insert_node_fts
                      after INSERT on node
                   begin
                      INSERT into search_node (node, content)
                      VALUES (NEW.id, NEW.content);
                   end;")
  (sqlite-execute database_handle
                  "CREATE TRIGGER IF NOT EXISTS update_node_fts
                      after UPDATE on node
                   begin
                      UPDATE search_node
                      SET
                        content = NEW.content
                      WHERE node = NEW.id;
                   end;")
  (sqlite-execute database_handle
                  "CREATE TRIGGER IF NOT EXISTS delete_node_fts
                      after DELETE on node
                   begin
                      DELETE FROM search_node
                      WHERE node = OLD.id;
                   end;")

  (sqlite-execute database_handle "CREATE UNIQUE INDEX IF NOT EXISTS unique_key_value_pair ON key_value_data(key, value);")
  (sqlite-execute database_handle "CREATE UNIQUE INDEX IF NOT EXISTS unique_key_value_pair_integer ON key_value_data(key, value);")
  (sqlite-execute database_handle "CREATE UNIQUE INDEX IF NOT EXISTS unique_key_value_pair_real ON key_value_data(key, value);")
  (sqlite-execute database_handle "CREATE UNIQUE INDEX IF NOT EXISTS unique_key_value_pair_blob ON key_value_data(key, value);")
  (sqlite-execute database_handle "CREATE UNIQUE INDEX IF NOT EXISTS unique_kvd_link ON data_or_properties_link(node, key_value_data, context);")
  (sqlite-execute database_handle "CREATE UNIQUE INDEX IF NOT EXISTS unique_kvd_link_integer ON data_or_properties_link_integer(node, key_value_data, context);")
  (sqlite-execute database_handle "CREATE UNIQUE INDEX IF NOT EXISTS unique_kvd_link_real ON data_or_properties_link_real(node, key_value_data, context);")
  (sqlite-execute database_handle "CREATE UNIQUE INDEX IF NOT EXISTS unique_kvd_link_blob ON data_or_properties_link_blob(node, key_value_data, context);")
  (sqlite-execute database_handle "CREATE UNIQUE INDEX IF NOT EXISTS unique_nodes_links ON nodes_link(node_a, node_b, type, context);")

  )


(defvar pkm2--key_value_data-type-to-table-plist '(INTEGER (:data-table "key_value_data_integer" :link-table "data_or_properties_link_integer")
                                                   DATETIME (:data-table "key_value_data_integer" :link-table "data_or_properties_link_integer")
                                                   TEXT (:data-table "key_value_data" :link-table "data_or_properties_link" )
                                                   BLOB (:data-table "key_value_data_blob" :link-table "data_or_properties_link_blob" )
                                                   REAL (:data-table "key_value_data_real" :link-table "data_or_properties_link_real" )))





(defun pkm2--db-get-kvd-data-table-for-type ( type )
  (--> pkm2--key_value_data-type-to-table-plist (plist-get it (or type 'TEXT ))(plist-get it :data-table )))

(defun pkm2--db-get-kvd-link-table-for-type ( type )
  (--> pkm2--key_value_data-type-to-table-plist (plist-get it (or type 'TEXT ))(plist-get it :link-table )))



(defun pkm2--db-compile-insert-statement (table-name value-names values)
  (concat
    (format "INSERT INTO %s "  table-name)
    (format "( %s ) " (string-join value-names ", "))
    "values "
    (format "( %s ) " (--> (-map #'pkm2--db-convert-object-to-string values)
                           (string-join it ", ")))
    "RETURNING id;"))

(defun pkm2--db-compile-update-statement (table-name value-names values where-clause &optional returning-column-names)
  (concat
   (format "UPDATE %s "  table-name)
   (format "SET %s " (--> (-zip-pair value-names values)
                          (-map (lambda (pair) (format "%s = %s" (car pair) (pkm2--db-convert-object-to-string (cdr pair)))) it)
                          (string-join it ", ")))
   (format "WHERE %s " where-clause)
   (when returning-column-names
     (format "RETURNING %s" (string-join returning-column-names ", ")))
   ";"))

(defun pkm2--db-compile-statement-unit (statement-unit)
  "TODO finish this. This is still exprimental and is not used."
  (cond ((or (stringp statement-unit) (numberp statement-unit)) (pkm2--db-convert-object-to-string statement-unit))
        ((keywordp statement-unit)
         (let ((name (symbol-name statement-unit)))
           (if (string-match-p ":" name)
               (cadr (split-string name ":"))
             (display-warning "Don't know what to do with %S" name))))
        ((symbolp statement-unit) (symbol-name statement-unit))
        ((listp statement-unit) (cond
                                 ((eq 'or (car statement-unit)) (--> (-map #'pkm2--db-compile-statement-unit (cdr statement-unit))
                                                                     (string-join it " OR ")
                                                                     (format "( %s )" it)))
                                 ((eq 'and (car statement-unit)) (--> (-map #'pkm2--db-compile-statement-unit (cdr statement-unit))
                                                                      (string-join it " AND ")
                                                                      (format "( %s )" it)))
                                 (t (--> (-map #'pkm2--db-compile-statement-unit statement-unit)
                                         (string-join it " ")))))))




(defun pkm2--db-insert-link-between-node-and-kvd (node-id key_value_data-id timestamp &optional type context-node-id is-archive)
  (let* ((table-name (pkm2--db-get-kvd-link-table-for-type type))
         (value-names '( "node" "key_value_data" "created_at" ))
         (values `(,node-id ,key_value_data-id ,timestamp)))
    (when context-node-id
      (push "context" value-names)
      (push context-node-id values))
    (when is-archive
      (push "is_archive" value-names)
      (push 1 values))
    (--> (pkm2--db-compile-insert-statement table-name value-names values)
         (sqlite-execute pkm2-database-connection it)
         (car it)
         (car it)
         (make-pkm2-db-kvd-link :id2 it  :type type :node node-id :key_value_data key_value_data-id :context context-node-id :created_at timestamp))))

(defun pkm2--db-delete-link-between-node-and-kvd (id type)
  (--> (pkm2--db-get-kvd-link-table-for-type type)
       (format "DELETE FROM %s WHERE id = %d" it id)
       (sqlite-execute pkm2-database-connection it) ))

(defun pkm2-db-archive-link-between-node-and-kvd (id type)
  (--> (pkm2--db-get-kvd-link-table-for-type type)
       (pkm2--db-compile-update-statement it (list "is_archive") (list 1) (format "id = %d" id) )
       (sqlite-execute pkm2-database-connection it)))

(defun pkm2--db-delete-node (id)
  (--> (format "delete from node where id = %d" id)
       (sqlite-execute pkm2-database-connection it)))
(defun pkm2--db-delete-link-between-nodes (id)
  (--> (format "delete from nodes_link where id = %d" id)
       (sqlite-execute pkm2-database-connection it)))


(defun pkm2--db-insert-link-between-nodeA-and-nodeB (type from-node-id to-node-id timestamp &optional  context-node-id)
  (let* ((table-name "nodes_link")
         (value-names '("type" "node_a" "node_b" "created_at"))
         (values `(,type ,from-node-id ,to-node-id ,timestamp)))
    (when context-node-id
      (push "context" value-names)
      (push context-node-id values))
    (--> (pkm2--db-compile-insert-statement table-name value-names values)
         (sqlite-execute pkm2-database-connection it)
         (car it)
         (car it)
         (make-pkm2-db-nodes-link :id it :type type :node_a from-node-id :node_b to-node-id :context context-node-id :created_at timestamp))))

(defun pkm2--db-insert-link-between-parent-and-child (type parent-id child-id timestamp &optional  context-node-id)
  (pkm2--db-insert-link-between-nodeA-and-nodeB type parent-id child-id timestamp context-node-id))


(defun pkm2--db-insert-node (content timestamp)
  (let* ((table-name "node")
         (value-names '("created_at"))
         (values `(,timestamp)))
    (when content
      (push "content" value-names)
      (push content values))
    (--> (pkm2--db-compile-insert-statement table-name value-names values)
         (sqlite-execute pkm2-database-connection it)
         (car it)
         (car it)
         (make-pkm2-db-node :id it :content content :created_at timestamp)
         (progn (message "Node inserted:%S" it)
                it))))

(defun pkm2--db-update-node (node-id new-content timestamp)
  (let* ((table-name "node")
         (value-names '("modified_at" "content"))
         (values `(,timestamp ,new-content))
         (where-clause (format "id = %d" node-id))
         (returning-column-names '("id" "content" "created_at" "modified_at")))
    (--> (pkm2--db-compile-update-statement table-name value-names values where-clause returning-column-names)
         (sqlite-execute pkm2-database-connection it)
         (car it)
         (make-pkm2-db-node :id (nth 0 it) :content (nth 1 it) :created_at (nth 2 it) :modified_at (nth 3 it)))))

(defun pkm2--db-insert-kvd (key value timestamp &optional type)
  (let* ((table-name (pkm2--db-get-kvd-data-table-for-type type))
         (value-names '("key" "value" "created_at"))
         (values `(,key ,value ,timestamp)))
    (--> (pkm2--db-compile-insert-statement table-name value-names values)
         (sqlite-execute pkm2-database-connection it)
         (car it)
         (car it)
         (make-pkm2-db-kvd :type type :id it :key key :value value :created_at timestamp))))

(defun pkm2--db-get-or-insert-kvd (key value &optional type)
  (let* ((data-table (pkm2--db-get-kvd-data-table-for-type type) )
         (db-info (car (sqlite-select pkm2-database-connection (format "SELECT id, key, value, created_at from %s WHERE key is '%s' AND value is %s" data-table key (pkm2--db-format-value-correctly value type))) )))
    (if db-info
        (make-pkm2-db-kvd :type type :id (nth 0 db-info) :key key :value value :created_at (nth 3 db-info))
      (pkm2--db-insert-kvd key value (pkm2-get-current-timestamp) type))))


(defun pkm2-node-has-key-value (node key value)
  "Check if NODE has KEY VALUE kvd.
VAlue can be a number, string, or list of numbers or strings."
  (--> (pkm2-node-kvds node)
       (-find (lambda (kvd)
                (and (equal (pkm2-db-kvd-key kvd) key)(equal (pkm2-db-kvd-value kvd) value))) it)
       (when it t)))


(defun pkm2-node-get-kvds-with-key(node key)
  "Get value of kvd  in NODE whose key = KEY"
  (--> (pkm2-node-kvds node)
       (-filter (lambda (kvd)
                  (equal (pkm2-db-kvd-key kvd) key)) it)))

; QUERY stuff

(defun pkm2--db-do-sql-query-and-get-all-rows (query &optional return-plist)
  "Run sql QUERY on database.
Returns output in two formats:
- either list of lists with first list being column names
- if RETURN-ALIST non-nil, list of alists, with keys of alist being column names"
  (if return-plist
      (let* ((table-with-columns-names (sqlite-select pkm2-database-connection query nil 'full))
             (columns (car table-with-columns-names))
             (data (cdr table-with-columns-names))
             (plist-data (-map (lambda (datum)
                                 (let (plist)
                                   (-each-indexed columns
                                     (lambda (index column)
                                       (setq plist (cons column (cons (nth index datum) plist )))
                                       ))
                                   plist))
                               data)))
        plist-data)
    (sqlite-select pkm2-database-connection query )))


(defun pkm2--db-query-get-all-nodes-ids (&optional limit)
  (--> (concat "SELECT id from node" (when limit (format " LIMIT %s" limit)) ";")
       (pkm2--db-do-sql-query-and-get-all-rows it)
       (-flatten it)))


(defun pkm2--db-query-get-kvd-link (link-id &optional data-type)
  (let* ((data-type (or data-type 'TEXT))
         (link-table (pkm2--db-get-kvd-link-table-for-type data-type))
         (query (format "SELECT * from %s WHERE id = %d" link-table link-id))
         (link-data (car (pkm2--db-do-sql-query-and-get-all-rows query t) )))
    (make-pkm2-db-kvd-link :id2 (plist-get link-data "id" #'equal)
                           :key_value_data (plist-get link-data "key_value_data" #'equal)
                           :node (plist-get link-data "node" #'equal)
                           :type data-type
                           :created_at (plist-get link-data "created_at" #'equal)
                           :is_archive (plist-get link-data "is_archive" #'equal)
                           :context (plist-get link-data "context" #'equal))))
(defun pkm2--db-query-get-kvds-for-node-with-id (id &optional allow-archive)
  ; TODO test
  (let* ((query (concat
                 "SELECT key_value_data.key as key, key_value_data.value as value, link.id as link_id, key_value_data.id as data_id, 'TEXT' as type, link.context as context_id "
                 "FROM data_or_properties_link as link "
                 "JOIN key_value_data ON key_value_data.id = link.key_value_data "
                 "WHERE "
                 (unless allow-archive
                   "link.is_archive is NULL AND ")
                 (format "link.node = '%d' " id)
                 "UNION ALL "

                 "SELECT key_value_data_integer.key, key_value_data_integer.value, link.id as link_id, key_value_data_integer.id as data_id, 'INTEGER' as type, link.context as context_id "
                 "FROM data_or_properties_link_integer as link "
                 "JOIN key_value_data_integer ON key_value_data_integer.id = link.key_value_data "
                 "WHERE "
                 (unless allow-archive
                   "link.is_archive is NULL AND ")
                 (format "link.node = '%d' " id)
                 "UNION ALL "

                 "SELECT key_value_data_real.key, key_value_data_real.value, link.id as link_id, key_value_data_real.id as data_id, 'REAL' as type, link.context as context_id "
                 "FROM data_or_properties_link_real as link "
                 "JOIN key_value_data_real ON key_value_data_real.id = link.key_value_data "
                 "WHERE "
                 (unless allow-archive
                   "link.is_archive is NULL AND ")
                 (format "link.node = '%d' " id)
                 "UNION ALL "

                 "SELECT key_value_data_blob.key, key_value_data_blob.value, link.id as link_id, key_value_data_blob.id as data_id, 'BLOB' as type, link.context as context_id "
                 "FROM data_or_properties_link_blob as link "
                 "JOIN key_value_data_blob ON key_value_data_blob.id = link.key_value_data "
                 "WHERE "
                 (unless allow-archive
                   "link.is_archive is NULL AND ")
                 (format "link.node = '%d' " id)
                 ";"))
         (return-plist t)
         (kvds-l (pkm2--db-do-sql-query-and-get-all-rows query return-plist))
         (kvds (-map (lambda (kvd-plist)
                       (make-pkm2-db-kvd :id (plist-get kvd-plist "data_id" #'equal)
                                         :type (intern (plist-get kvd-plist "type" #'equal))
                                         :link-id (plist-get kvd-plist "link_id" #'equal)
                                         :context-id (plist-get kvd-plist "context_id" #'equal)
                                         :key (plist-get kvd-plist "key" #'equal)
                                         :value (plist-get kvd-plist "value" #'equal)
                                         :created_at (plist-get kvd-plist "created_at" #'equal)))
                     kvds-l)))
    kvds))

(defun pkm2--db-query-get-archived-kvds-for-node-with-id (id)
  ; TODO test
  (let* ((query (concat
                 "SELECT key_value_data.key as key, key_value_data.value as value, link.id as link_id, key_value_data.id as data_id, 'TEXT' as type, link.context as context_id "
                 "FROM data_or_properties_link as link "
                 "JOIN key_value_data ON key_value_data.id = link.key_value_data "
                 "WHERE link.is_archive is 1 AND link.node = "
                 (format "'%d'" id)
                 "UNION ALL "

                 "SELECT key_value_data_integer.key, key_value_data_integer.value, link.id as link_id, key_value_data_integer.id as data_id, 'INTEGER' as type, link.context as context_id "
                 "FROM data_or_properties_link_integer as link "
                 "JOIN key_value_data_integer ON key_value_data_integer.id = link.key_value_data "
                 "WHERE link.is_archive is 1 AND link.node = "
                 (format "'%d'" id)
                 "UNION ALL "

                 "SELECT key_value_data_real.key, key_value_data_real.value, link.id as link_id, key_value_data_real.id as data_id, 'REAL' as type, link.context as context_id "
                 "FROM data_or_properties_link_real as link "
                 "JOIN key_value_data_real ON key_value_data_real.id = link.key_value_data "
                 "WHERE link.is_archive is 1 AND link.node = "
                 (format "'%d'" id)
                 "UNION ALL "

                 "SELECT key_value_data_blob.key, key_value_data_blob.value, link.id as link_id, key_value_data_blob.id as data_id, 'BLOB' as type, link.context as context_id "
                 "FROM data_or_properties_link_blob as link "
                 "JOIN key_value_data_blob ON key_value_data_blob.id = link.key_value_data "
                 "WHERE link.is_archive is 1 AND link.node = "
                 (format "'%d'" id)
                 ";"))
         (return-plist t)
         (kvds-l (pkm2--db-do-sql-query-and-get-all-rows query return-plist))
         (kvds (-map (lambda (kvd-plist)
                       (make-pkm2-db-kvd :id (plist-get kvd-plist "data_id" #'equal)
                                         :type (intern (plist-get kvd-plist "type" #'equal))
                                         :link-id (plist-get kvd-plist "link_id" #'equal)
                                         :context-id (plist-get kvd-plist "context_id" #'equal)
                                         :key (plist-get kvd-plist "key" #'equal)
                                         :value (plist-get kvd-plist "value" #'equal)
                                         :created_at (plist-get kvd-plist "created_at" #'equal)))
                     kvds-l)))
    kvds))

(defun pkm2--db-query-get-links-where-node-is-parent (id)
  (let* ((link-labels (plist-get pkm-links-type-to-label-eq-plist 'HIERARCHICAL))
         (query (concat "SELECT id, type, created_at, node_a, node_b, context FROM nodes_link "
                        (format "WHERE type in (%s) " (--> (-map #'pkm2--db-convert-object-to-string link-labels)
                                                           (string-join it ", ")))
                        (format "AND node_a = %d" id)))
         (return-plist t)
         (links-l (pkm2--db-do-sql-query-and-get-all-rows query return-plist))
         (links (-map (lambda (link-plist)
                        (make-pkm2-db-nodes-link
                         :id (plist-get link-plist "id" #'equal)
                         :type (plist-get link-plist "type" #'equal)
                         :node_a (plist-get link-plist "node_a" #'equal)
                         :node_b (plist-get link-plist "node_b" #'equal)
                         :context (plist-get link-plist "context" #'equal)
                         :created_at (plist-get link-plist "created_at" #'equal)))
                      links-l)))
    links))
(defun pkm2--db-query-get-links-where-node-is-child (id)
  (let* ((link-labels (plist-get pkm-links-type-to-label-eq-plist 'HIERARCHICAL))
         (query (concat "SELECT id, type, created_at, node_a, node_b, context FROM nodes_link "
                        (format "WHERE type in (%s) " (--> (-map #'pkm2--db-convert-object-to-string link-labels)
                                                           (string-join it ", ")))
                        (format "AND node_b = %d" id)))
         (return-plist t)
         (links-l (pkm2--db-do-sql-query-and-get-all-rows query return-plist))
         (links (-map (lambda (link-plist)
                        (make-pkm2-db-nodes-link
                         :id (plist-get link-plist "id" #'equal)
                         :type (plist-get link-plist "type" #'equal)
                         :node_a (plist-get link-plist "node_a" #'equal)
                         :node_b (plist-get link-plist "node_b" #'equal)
                         :context (plist-get link-plist "context" #'equal)
                         :created_at (plist-get link-plist "created_at" #'equal)))
                      links-l)))
    links))


(defun pkm2--db-query-get-node-with-id (id)
  (let* ((kvds (pkm2--db-query-get-kvds-for-node-with-id id))
         (query (format "SELECT * FROM node WHERE id = '%d';" id))
         (node-info (car (pkm2--db-do-sql-query-and-get-all-rows query t)))
         (db-node (make-pkm2-db-node :id (plist-get node-info "id" #'equal)
                                     :content (plist-get node-info "content" #'equal)
                                     :created_at (plist-get node-info "created_at" #'equal)
                                     :modified_at (plist-get node-info "modified_at" #'equal)))
         (parent-links (pkm2--db-query-get-links-where-node-is-child id))
         (children-links (pkm2--db-query-get-links-where-node-is-parent id))
         (pkm-node (make-pkm2-node :db-node db-node
                                   :kvds kvds
                                   :parent-links parent-links
                                   :children-links children-links)))
    (setf (pkm2-node-types pkm-node) (pkm-object-get-structures pkm-node))
    pkm-node))



(defun pkm2--db-query-get-nodes-with-links-to-kvds-with-key (key type)
  (let* ((data-table (pkm2--db-get-kvd-data-table-for-type type))
         (link-table (pkm2--db-get-kvd-link-table-for-type type))
         (query (format "SELECT link.node from %s as link WHERE link.key_value_data IN (SELECT id FROM %s WHERE key = '%s');" link-table data-table key)))
    (-flatten (sqlite-execute pkm2-database-connection query) )))

(defun pkm2--db-query-get-nodes-with-links-to-kvds-with-key-and-values (key values type)
  (let* ((data-table (pkm2--db-get-kvd-data-table-for-type type))
         (link-table (pkm2--db-get-kvd-link-table-for-type type))
         (query (format "SELECT link.node from %s as link WHERE link.key_value_data IN (SELECT id FROM %s WHERE key = '%s' AND value IN (%s));" link-table data-table key  (--> (-map #'pkm2--db-convert-object-to-string values)
                                                                                                                                                                               (string-join it ", ")))))
    (-flatten (sqlite-execute pkm2-database-connection query) )))

(defun pkm2--db-compile-query-to-get-nodes-in-following-subquery-and--key-and-value (subquery key values type)
  (let* ((data-table (pkm2--db-get-kvd-data-table-for-type type))
         (link-table (pkm2--db-get-kvd-link-table-for-type type))
         (query (concat (format "SELECT DISTINCT node from %s WHERE key_value_data IN (SELECT id FROM %s WHERE key = '%s' AND value IN (%s)) "
                                link-table
                                data-table key
                                (--> (-map #'pkm2--db-convert-object-to-string values)
                                     (string-join it ", ")))
                        (when subquery (format "AND node IN (%s)" subquery)))))
    query))




(defun pkm2--db-query-get-all-keys-to-types-alist ()
  "returns ((keyname type))"
  (let* ((query  "SELECT DISTINCT key, 'TEXT' as type from key_value_data UNION
                  SELECT DISTINCT key,'INTEGER' as type from key_value_data_integer UNION
                  SELECT DISTINCT key, 'REAL' as type from key_value_data_real UNION
                  SELECT DISTINCT key, 'BLOB' as type from key_value_data_blob;")
         (return-plist nil)
         (data (pkm2--db-do-sql-query-and-get-all-rows query return-plist))
         output)
    (-each data (lambda (datum)
                  (setq output
                        (plist-put output
                                   (nth 0 datum) ; key
                                   (-concat (plist-get output (nth 0 datum) #'equal)
                                            (list (nth 1 datum))) ;value type
                                   #'equal
                                   ))))
    (cl--plist-to-alist output)))

(defun pkm2--db-query-get-all-values-with-key (key &optional type)
  (let* ((data-table (pkm2--db-get-kvd-data-table-for-type (or type 'TEXT)))
         (query (concat "SELECT DISTINCT value from " (format "%s " data-table) (format " WHERE key = '%s'" key) ";"))
         (return-plist nil))
    (-flatten (pkm2--db-do-sql-query-and-get-all-rows query return-plist))))

;;; pkm-behavior

(defvar pkm-structure-defined-behavior-plist ())

(defun pkm2-register-behavior (behavior)
  (setq pkm-structure-defined-behavior-plist (plist-put pkm-structure-defined-behavior-plist (plist-get behavior :name) behavior)))


(defun pkm2--get-behavior-assets (behavior-name link-to)
  (--> (plist-get pkm-structure-defined-behavior-plist behavior-name)
       (plist-get it :assets)
       (-map (lambda (asset)
               (plist-put asset :link-to link-to))
             it)))

;;; pkm-object

(defvar pkm-structure-undefined-schemas-plist ())
(defvar pkm-structure-defined-schemas-plist ())




(defun pkm-register-structure (name structure-plist)
  (--> (plist-put structure-plist :name name) (plist-put it :pkm-type name) (plist-put pkm-structure-undefined-schemas-plist name it) (setq pkm-structure-undefined-schemas-plist it))
  )



(defvar pkm--object-known-keys (list :assets :links :parent :parents))

(defun pkm--object-psuedo-map-by-type-and-name (input-list)
  "INPUT-LIST should be formatted (type (plist)) plist might have :name key"
  (-map (lambda (item)
          (let* ((type (plist-get item :pkm-type))
                 (name (plist-get item :name)))
            (list (list type name) item)))
        input-list))

(defun pkm--object-nondestructively-combine (from to)
  "Things from FROM will overwrite things in TO"
  (let* (output
         (from-mapped (pkm--object-psuedo-map-by-type-and-name from))
         (to-mapped (pkm--object-psuedo-map-by-type-and-name to)))
    (-each from-mapped (lambda (item-mapped)
                         (unless (assoc-default (car item-mapped) to-mapped)
                           (push (cadr item-mapped) output))))
    (-each to-mapped (lambda (item-mapped)
                       (push (cadr item-mapped) output)))
    output))


(defun pkm--object-define (structure-name)
  "Used to create internal representation of pkm-object."
  (let* (fully-defined-schema
         (schema (plist-get pkm-structure-undefined-schemas-plist structure-name))
         (behaviors (plist-get schema :behaviors))
         (behavior-assets  (-map (lambda (behavior)
                                   (pkm2--get-behavior-assets (plist-get behavior :name) (plist-get behavior :link-to)))
                                 behaviors))
         (parent-name (plist-get schema :parent))
         (parent-schema (when parent-name (pkm--object-define parent-name)))
         (parent-keys (doom-plist-keys parent-schema))
         (self-keys (doom-plist-keys schema))
         (parent-assets (-map #'-copy (plist-get parent-schema :assets) )
                        )
         (self-assets (plist-get schema :assets))
         (combined-assets (-reduce-from #'pkm--object-nondestructively-combine (pkm--object-nondestructively-combine parent-assets self-assets) behavior-assets))
         (asset-modifications (plist-get schema :asset-modifications))
         (combined-assets (-map (lambda (asset)
                                  (if-let*
                                      ((a-mod (-find (lambda (mod)
                                                       (when (equal (plist-get mod :name) (plist-get asset :name))
                                                         t)) asset-modifications))
                                       (plist-key (plist-get a-mod :plist-key))
                                       (plist-value (plist-get a-mod :plist-value)))
                                      (plist-put asset  plist-key plist-value)
                                    asset))
                                combined-assets))

         (parent-links (-map #'-copy (plist-get parent-schema :links)))
         (self-links (plist-get schema :links))
         (links-in-both (pkm--object-nondestructively-combine self-links parent-links))
         (unhandled-keys (-filter (lambda (key)
                                    (not (member key pkm--object-known-keys)))
                                  (-concat parent-keys self-keys)))
         (parents (-concat (when parent-name `(,parent-name) ) (plist-get parent-schema :parents))) )
    (when combined-assets
      (setq fully-defined-schema (plist-put fully-defined-schema :assets combined-assets)))
    (when links-in-both (setq fully-defined-schema (plist-put fully-defined-schema :links links-in-both)))
    (-each unhandled-keys (lambda (key)
                            (setq fully-defined-schema
                                  (if (member key self-keys)
                                      (plist-put fully-defined-schema key (plist-get schema key))
                                    (plist-put fully-defined-schema key (plist-get parent-schema key))))))
    (setq fully-defined-schema (plist-put fully-defined-schema :parents parents))
    (when unhandled-keys
      (display-warning 'pkm-object "Unhandled keys, For now child is overwritting its parent, might want to do something else: %S" unhandled-keys))
    fully-defined-schema))

(defun pkm--object-validate-structure-schema (structure-schema)
  (let* ((primary-nodes (--> (plist-get structure-schema :assets)
                             (-filter (lambda (asset) (when (eq (plist-get asset :pkm-type) 'node) (plist-get asset :primary-node)))
                                      it)))
         (has-one-primary-node (= 1 (length primary-nodes))))
    (if (not has-one-primary-node) t (display-warning 'pkm-object "Object definition need to have one and only one primary node.\nPrimary nodes: %S" primary-nodes))))

(defun pkm--object-does-specifier-match (specifier asset-schema)
  (cond
   ((stringp specifier)                 ; node-specifier is a name
    (equal (plist-get asset-schema :name) specifier))
   ((eq specifier 'primary)
    (plist-get asset-schema :primary-node))
   ((eq specifier 'parent)
    (plist-get asset-schema :parent-node))
   ((eq specifier 'child)
    (message "in child asset, returning: %S, got: %S" (plist-get asset-schema :child-node) asset-schema)
    (plist-get asset-schema :child-node))
   (t (progn (display-warning 'pkm-object "node-specifier is ambigious: %S" specifier)
             (error "node-specifier was ambigious, be better!")))))





(defun pkm-object-get-structures (pkm-node)
  (--> (doom-plist-keys pkm-structure-defined-schemas-plist)
       (-filter (lambda (structure-name)
                  (pkm-objectp (plist-get pkm-structure-defined-schemas-plist structure-name) pkm-node)) it)))




(defvar pkm-structure-fully-specified-kvds-plist ())
(defvar pkm-structure-required-kvds-plist ())
(defvar pkm-structure-unique-required-plist ())
(defvar pkm-kvd-schemas-plist ())
(defvar pkm-kvd-key-to-structure-plist ())
(defvar pkm-data-type-to-kvd-key-plist ())

(defun pkm-object-register-structure-with-kvd (kvd structure-name)
  (let* ((key (plist-get kvd :key))
         (current-registrations  (plist-get pkm-kvd-key-to-structure-plist key #'equal)))
    (unless (member structure-name current-registrations)
      (setq  pkm-kvd-key-to-structure-plist (plist-put  pkm-kvd-key-to-structure-plist key (-concat `(,structure-name) current-registrations) #'equal)))))

(defun pkm-object-register-kvd-key-with-data-type (kvd)
  (let* ((key (plist-get kvd :key))
         (data-type (plist-get kvd :data-type))
         (current-registrations  (plist-get pkm-data-type-to-kvd-key-plist data-type #'equal)))
    (unless (member key current-registrations)
      (setq  pkm-data-type-to-kvd-key-plist (plist-put  pkm-data-type-to-kvd-key-plist data-type (-concat `(,key) current-registrations) #'equal)))))


(defun pkm-object-kvd-get-kvd-spec-for-key-in-structure (key structure-name)
  (--> (plist-get pkm-structure-defined-schemas-plist structure-name)
       (plist-get it :assets)
       (-find (lambda (asset-spec)
                  (when (and (equal (plist-get asset-spec :pkm-type) 'kvd)
                             (equal (plist-get asset-spec :key) key))
                    t))
                it)))

(defun pkm--object-find-unique-identifiers (structure-name structure-plist required-kvds-plist)
  (list (pkm--object-find-unique-identifiers-unique-key structure-name structure-plist required-kvds-plist)
        (pkm--object-find-unique-identifiers-unique-kvds structure-name structure-plist required-kvds-plist)))

(defun pkm--object-find-unique-identifiers-unique-key (structure-name  structure-plist required-kvds-plist)
  (let* ((self-fully-defined-kvds (plist-get required-kvds-plist structure-name))
         (keys (-distinct (-map (lambda (kvd) (plist-get kvd :key)) self-fully-defined-kvds)))
         (structure-names (doom-plist-keys required-kvds-plist))
         (unique-keys (-filter
                       (lambda (key)
                         (not (-any? (lambda (s-name)
                                       (unless (or (eq s-name structure-name)
                                                   (member structure-name (--> (plist-get structure-plist s-name) (plist-get it :parents))))
                                           (--> (plist-get required-kvds-plist s-name)
                                                (-map (lambda (kvd)
                                                        (plist-get kvd :key)) it)
                                                (member key it)) ))
                                     structure-names) ))
                       keys)))
    unique-keys))

(defun pkm--object-find-unique-identifiers-unique-kvds (structure-name structure-plist required-kvds-plist)
  (let* ((self-fully-defined-kvds (plist-get required-kvds-plist structure-name))
         (structure-names (doom-plist-keys required-kvds-plist))
         (unique-kvds (-filter
                       (lambda (s-kvd)
                         (not (-any? (lambda (temp-name)
                                       (if (or (eq temp-name structure-name) (member structure-name (--> (plist-get structure-plist temp-name) (plist-get it :parents))))
                                           nil
                                         (--> (plist-get required-kvds-plist temp-name)
                                              (-any? (lambda (kvd)
                                                       (if (equal (plist-get s-kvd :key) (plist-get kvd :key))
                                                           (or (equal (plist-get s-kvd :value) (plist-get kvd :value))
                                                               (and (and (plist-get s-kvd :choices) (plist-get kvd :choices))
                                                                    (or  (-intersection (plist-get s-kvd :choices) (plist-get kvd :choices))
                                                                         (-intersection (plist-get kvd :choices) (plist-get s-kvd :choices))) ))
                                                         nil))
                                                     it))))
                                     structure-names)))
                       self-fully-defined-kvds)))
    unique-kvds))



(defun pkm--object-get-required-kvds (object-schema)
  (let* ((assets (plist-get object-schema :assets))
         (primary-asset (-find (lambda (asset)
                                 (plist-get asset :primary-node))
                               assets))
         (required-kvds (-filter (lambda (possibly-kvd)
                                   (and (eq 'kvd (plist-get possibly-kvd :pkm-type))
                                        (not (plist-get possibly-kvd :optional))
                                        (not (plist-get possibly-kvd :managed))
                                        (-any
                                         (lambda (specifier)
                                           (pkm--object-does-specifier-match specifier primary-asset))
                                         (plist-get possibly-kvd :link-to))))
                                 assets)))
    required-kvds))

(defun pkm--behavior-get-required-kvds (object-schema)
  (let* ((assets (plist-get object-schema :assets))
         (required-kvds (-filter (lambda (possibly-kvd)
                                   (and (eq 'kvd (plist-get possibly-kvd :pkm-type))
                                        (not (plist-get possibly-kvd :optional))
                                        (not (plist-get possibly-kvd :managed))
                                        ))
                                 assets)))
    required-kvds))


(defun pkm--object-get-required-fully-specified-kvds (object-schema)
  (let* ((required-kvds (pkm--object-get-required-kvds object-schema))
         (fully-specified (pkm--object-get-required-fully-specified-kvds2 required-kvds) ))
    fully-specified))
(defun pkm--object-get-required-fully-specified-kvds2 (kvds)
"TODO replace v1 of this function with this."
  (-filter (lambda (kvd-schema)
             (or (stringp (plist-get kvd-schema :value))
                 (when (plist-get kvd-schema :choices)
                   (listp (plist-get kvd-schema :choices)))))
                                   kvds))
(defun pkm--object-get-time-related-kvd-keys ()
  (plist-get pkm-data-type-to-kvd-key-plist 'DATETIME))

(defun pkm-objectp (object-schema pkm-node)
  "Check if pkm-NODE is the primary pkm-node of OBJECT-SCHEMA"

  ; (message "schema: %S" object-schema)
  (let* ((name (plist-get object-schema :name))
         (fully-specified (plist-get pkm-structure-fully-specified-kvds-plist name))
         (has-kvd (-non-nil (-map (lambda (kvd-schema)
                                    (cond ((plist-get kvd-schema :value)
                                           (pkm2-node-has-key-value pkm-node (plist-get kvd-schema :key) (plist-get kvd-schema :value)))
                                          ((plist-get kvd-schema :choices)
                                           (-any (lambda (choice)
                                                   (let* ((actual-choice (if (listp choice) (cdr choice) choice))
                                                          (key (plist-get kvd-schema :key)))
                                                     ; (message "in choices: %S, key: %s, choice: %s, %S" name key actual-choice (pkm2-node-has-key-value pkm-node key actual-choice))
                                                     (pkm2-node-has-key-value pkm-node key actual-choice)))
                                                 (plist-get kvd-schema :choices)))))
                                  fully-specified) )))
    ; (message "fs: %S, hk: %S\nkvd:%S" fully-specified has-kvd (pkm2-node-kvds pkm-node))
    (eq (length fully-specified ) (length has-kvd))))


(defun pkm--object-cache-structure-info ()
  (let* ((structure-names  (doom-plist-keys pkm-structure-undefined-schemas-plist))
         (behavior-structures (-filter (lambda (s-name)
                                         (--> (plist-get pkm-structure-undefined-schemas-plist s-name)
                                              (plist-get it :is-behavior)))
                                       structure-names)))
    (-each structure-names
      (lambda (structure-name)
        (setq pkm-structure-defined-schemas-plist
              (plist-put pkm-structure-defined-schemas-plist structure-name (pkm--object-define structure-name)))))
    (-each  structure-names
      (lambda (structure-name)
        (--> (plist-get pkm-structure-defined-schemas-plist structure-name)
             (pkm--object-get-required-kvds it)
             (setq pkm-structure-required-kvds-plist (plist-put pkm-structure-required-kvds-plist structure-name it)))))
    (-each  structure-names
      (lambda (structure-name)
        (--> (plist-get pkm-structure-defined-schemas-plist structure-name)
             (pkm--object-get-required-fully-specified-kvds it)
             (setq pkm-structure-fully-specified-kvds-plist (plist-put pkm-structure-fully-specified-kvds-plist structure-name it)))))
    (-each  structure-names
      (lambda (structure-name)
        (--> (pkm--object-find-unique-identifiers structure-name pkm-structure-defined-schemas-plist pkm-structure-required-kvds-plist)
             (setq pkm-structure-unique-required-plist (plist-put pkm-structure-unique-required-plist structure-name it)))))
    ; For behaviour based structures, only compare with other behivor based structures
    (let* ((structure-to-behavior-schema-plist  (mm-alist-to-plist (-map (lambda (s-name)
                                                                           (cons s-name
                                                                                 (--> (plist-get pkm-structure-undefined-schemas-plist s-name)
                                                                                      (plist-get it :is-behavior)
                                                                                      (plist-get pkm-structure-defined-behavior-plist it) )))
                                                                         behavior-structures) ))
           (required-kvds-plist (mm-alist-to-plist (-map (lambda (s-name)
                                                           (cons s-name
                                                                 (--> (plist-get structure-to-behavior-schema-plist s-name)
                                                                      (pkm--behavior-get-required-kvds
                                                                       it))))
                                                         behavior-structures))))
      (-each  behavior-structures
        (lambda (structure-name)
          (--> (pkm--object-find-unique-identifiers structure-name structure-to-behavior-schema-plist  required-kvds-plist)
               (setq pkm-structure-unique-required-plist (plist-put pkm-structure-unique-required-plist structure-name it))))))))


;;; pkm-object-capture

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

(defvar pkm--object-capture-alist `((node ,#'pkm--object-capture-node)
                                    (kvd ,#'pkm--object-capture-kvd)))

(defvar pkm--object-add-to-db-alist `((node ,#'pkm--object-add-node-to-db)
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



;;; capture

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
    (list :asset-schema asset-spec :asset-capture-info (list :content content :db-id db-id :from from :to to) :asset-name asset-name)))

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
    (list :asset-schema asset-spec :asset-capture-info (list :key key :value value :from from :to to) :asset-name asset-name)))


(defun pkm2--object-capture-object-verify (structure-name-or-schema &optional external-buffer-name skip-verify values sub-asset-name)
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
                                                                        asset-name))))
                                        (list :asset-schema asset-spec :asset-capture-info nil :asset-name asset-name))))
                                  asset-specs)))
         (output (list :pkm-type structure-name :schema structure-schema :capture-info captured-assets :asset-name sub-asset-name)))
    (if (and skip-verify (not sub-asset-name))
        (pkm--object-capture-finializer structure-schema captured-assets)
      (unless (or sub-asset-name external-buffer-name)
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
  (unless within-transaction
    (sqlite-transaction pkm2-database-connection) )
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
                                        ; TODO NEXT handle none-node and none-kvd assets
             (uncommited-nodes (assoc-default 'node assets-by-group))
             (commited-nodes-with-db-ids (-map #'pkm--object-add-node-to-db uncommited-nodes))
             (commited-nodes-and-structures-with-db-ids (-concat commited-nodes-with-db-ids committed-pkm-structures-with-db-ids))
             (uncommited-kvds (assoc-default 'kvd assets-by-group))
             (commited-kvds-with-db-ids  (-map #'pkm--object-add-kvd-to-db uncommited-kvds))
                                        ; for each node specifier, get node-db-id and create link between kvd and node-db-id
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
          (sqlite-commit pkm2-database-connection)
                                        ; (sqlite-rollback pkm2-database-connection)
          ))
    (error (message "Received Error %s" (error-message-string err))
           (sqlite-rollback pkm2-database-connection))))


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


;;; pkm-links

(defvar pkm-links-types '(HIERARCHICAL SEQUENCAL FLAT))
(defvar pkm-links-label-to-type-equal-plist ())

(defvar pkm-links-type-to-label-eq-plist ())


(defun pkm2--register-link-label (link-type label)
  (unless (member link-type pkm-links-types) (error "%S is not a accepted link type. Needs to be one of following %S" link-type pkm-links-types))
  (setq pkm-links-label-to-type-equal-plist (plist-put pkm-links-label-to-type-equal-plist label link-type #'equal))
  (setq pkm-links-type-to-label-eq-plist
        (plist-put pkm-links-type-to-label-eq-plist link-type
                   (-distinct (-concat (list label) (plist-get pkm-links-type-to-label-eq-plist link-type))))))

(defun pkm2--link-get-link-parent-id (link)
  (pkm2-db-nodes-link-node_a link))
(defun pkm2--link-get-link-child-id (link)
  (pkm2-db-nodes-link-node_b link))

(pkm2--register-link-label 'HIERARCHICAL "sub")
(pkm2--register-link-label 'HIERARCHICAL "related")
(pkm2--register-link-label 'HIERARCHICAL "clock")
(pkm2--register-link-label 'HIERARCHICAL "instance")
(pkm2--register-link-label 'HIERARCHICAL "type-of")


;;; compile query

(defun pkm2--compile-get-all-nodes (&optional limit node-subquery)
  (concat "SELECT id from node "
          (when node-subquery (format "WHERE id IN (%s) "  node-subquery))
          (when limit (format " LIMIT %s" limit)) ))

(defun pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key (key type &optional node-subquery)
  (let* ((data-table (pkm2--db-get-kvd-data-table-for-type type))
         (link-table (pkm2--db-get-kvd-link-table-for-type type))
         (query (concat (format "SELECT link.node from %s as link WHERE link.is_archive is NULL AND link.key_value_data IN (SELECT id FROM %s WHERE key = '%s') "
                                link-table
                                data-table
                                key)
                        (when node-subquery (format "AND node in (%s)" node-subquery)))))
    query))

(defun pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-values (key values type &optional node-subquery)
  (let* ((data-table (pkm2--db-get-kvd-data-table-for-type type))
         (link-table (pkm2--db-get-kvd-link-table-for-type type))
         (query (concat (format "SELECT link.node from %s as link WHERE link.is_archive is NULL AND link.key_value_data IN (SELECT id FROM %s WHERE key = '%s' AND value IN (%s))"
                                link-table data-table key
                                (--> (-map #'pkm2--db-convert-object-to-string values)
                                     (string-join it ", ")))
                        (when node-subquery (format "AND node in (%s)" node-subquery)))))
    query))


(defun pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-value-in-range (key after before type &optional node-subquery)
  (if (or (eq type 'DATETIME) (eq type 'INTEGER) (eq type 'REAL))
      (let* ((data-table (pkm2--db-get-kvd-data-table-for-type type))
             (link-table (pkm2--db-get-kvd-link-table-for-type type))
             (kvd-subquery (concat (format "SELECT id FROM %s WHERE key = '%s' " data-table key)
                                   (when after (format "AND value > %d " after))
                                   (when before (format "AND value < %d " before))))
             (query (concat (format "SELECT link.node from %s as link WHERE link.is_archive is NULL AND link.key_value_data IN (%s) "
                                    link-table
                                    kvd-subquery)
                            (when node-subquery (format "AND node in (%s)" node-subquery)))))
        query)))


(defun pkm2--object-db-compile-query-to-get-nodes-with-link-to-kvd (kvd &optional node-subquery)
  (let* ((key (plist-get kvd :key))
         (values (cond ((list (plist-get kvd :value)) )
                       ((plist-get kvd :choices) )))
         (type (plist-get kvd :data-type)))
    (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-values key values type node-subquery)))

(defun pkm2--object-db-query-get-nodes-with-links-to-kvds (kvds &optional node-subquery)
  (-reduce-from (lambda (init-subquery kvd)
                  (pkm2--object-db-compile-query-to-get-nodes-with-link-to-kvd kvd init-subquery))
   node-subquery
   kvds))


(defun pkm2--db-compile-query-get-nodes-of-structure-type (structure-name &optional node-subquery)
  (let* ((unique-required (plist-get pkm-structure-unique-required-plist structure-name))
         (u-r-keys (--> (car unique-required) (-filter (lambda (kvd) (member (plist-get kvd :key) it))
                                                       (cadr unique-required))))
         (first-key-kvd-spec (car u-r-keys))
         (first-key (plist-get first-key-kvd-spec :key))
         (first-key-type (plist-get first-key-kvd-spec :data-type))
         (fully-specified-kvds (pkm--object-get-required-fully-specified-kvds2 (cadr unique-required)))
         (easiest-queriable-kvds
          (list (cond ((-find (lambda (kvd)
                                (--> (plist-get kvd :value) (or (stringp it) (numberp it)))) fully-specified-kvds) )
                      ((-find (lambda (kvd)
                                (plist-get kvd :choices)) fully-specified-kvds))))))
    (cond (u-r-keys (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key first-key first-key-type node-subquery))
          (easiest-queriable-kvds  (pkm2--object-db-query-get-nodes-with-links-to-kvds easiest-queriable-kvds node-subquery))
          (t (error "Unable to get nodes for structure-type %S" structure-name)))))

(defun pkm2--db-compile-get-nodes-between (after before &optional node-subquery)
  ; created_at and modified_at between after and before
  ; Node linked to any kvd with datetime type between after and before
  ; If any of the nodes above are dependent types, get its parent node
  (let* ((keys (pkm--object-get-time-related-kvd-keys))
         (query-nodes-with-keys (-reduce-from (lambda (init-subquery key) (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-value-in-range key after before init-subquery))
                                              nil
                                              keys))
         ; TODO query creation can def be improved below
         (query (concat "SELECT id FROM node WHERE "
                        "("
                        (when after (format "(created_at > %d OR modified_at > %d) " after after))
                        (when before (format "OR (created_at < %d OR modified_at < %d) " before before))
                        (when query-nodes-with-keys (format "OR id IN (%s)" query-nodes-with-keys))
                        ") "
                        (when node-subquery (format "AND id IN (%s)" node-subquery) ))))
    (message "in between: query: %s" query)
    query))


(defun pkm2--db-compile-get-nodes-with-num-children (num &optional node-subquery)
  (let* ((parent "node_b")
         (query (concat (format "SELECT %s FROM nodes_link " parent)
                        (when node-subquery (format "WHERE %s IN (%s) " parent node-subquery))
                        (format "GROUP BY %s " parent)
                        (when num (format "HAVING COUNT(*) > %d " num)))))
    query))

(defun pkm2--db-compile-get-nodes-with-children-with (children-subquery &optional node-subquery)
  (let* ((parent "node_b")
         (child "node_a")
         (query (concat (format "SELECT %s FROM nodes_link " parent)
                        (format "WHERE %s IN (%s) " child children-subquery)
                        (when node-subquery (format "WHERE %s IN (%s) " parent node-subquery)))))
    query))


(defun pkm2--db-compile-get-nodes-with-parents-with (parent-subquery &optional node-subquery)
  (let* ((parent "node_b")
         (child "node_a")
         (query (concat (format "SELECT %s FROM nodes_link " child)
                        (format "WHERE %s IN (%s) " parent parent-subquery)
                        (when node-subquery (format "WHERE %s IN (%s) " child node-subquery)))))
    query))


(defun pkm2--db-compile-get-nodes-with-num-parents (num &optional node-subquery)
  (let* ((child "node_a")
         (query (concat (format "SELECT %s FROM nodes_link " child)
                        (when node-subquery (format "WHERE %s IN (%s) " child node-subquery))
                        (format "GROUP BY %s " child)
                        (when num (format "HAVING COUNT(*) > %d " num)))))
    query))



(defun pkm2--db-compile-query-get-node-with-text (text &optional node-subquery)
  (concat "SELECT node FROM search_node "
          (format "WHERE search_node MATCH '%s' " text)
          (when node-subquery (format "AND node in (%s)" node-subquery))))

(defun pkm2--db-query-get-sub-nodes (levels link-labels node-subquery &optional get-parent-id)
  ; TODO TEST
  (let* ((link-labels (or link-labels (plist-get pkm-links-type-to-label-eq-plist 'HIERARCHICAL) ))
         (link-labels-string (--> (-map #'pkm2--db-convert-object-to-string link-labels)
                                              (string-join it ", ")))
         (query (concat
                 (format "WITH RECURSIVE subs_table(node_id, parent_id, level) AS (%s) "
                         (concat (format "SELECT node_b, node_a, 1 FROM nodes_link WHERE type in (%s) AND node_a IN (%s) "
                                         link-labels-string
                                         node-subquery)
                                 "UNION "
                                 "SELECT node_b, node_id, subs_table.level + 1 FROM nodes_link JOIN subs_table ON node_id = node_a "
                                 (format "WHERE type in (%s) " link-labels-string)
                                 (when  (numberp levels)
                                   (format "AND level < %d " levels))))
                 (if get-parent-id
                     "SELECT node_id, parent_id FROM subs_table"
                   "SELECT node_id FROM subs_table"))))
    query))

(defun pkm2--db-query-get-parent-nodes (levels link-labels node-subquery &optional get-child-id)
  ; TODO modify to only return query
  (let* ((link-labels (or link-labels (plist-get pkm-links-type-to-label-eq-plist 'HIERARCHICAL) ))
         (link-labels-string (--> (-map #'pkm2--db-convert-object-to-string link-labels)
                                  (string-join it ", ")))
         (query (concat
                 (format "WITH RECURSIVE subs_table(node_id, child_id, level) AS (%s) "
                         (concat (format "SELECT node_a, node_b, 1 FROM nodes_link WHERE type in (%s) AND node_b IN (%s) "
                                         link-labels-string
                                         node-subquery)
                                 "UNION "
                                 "SELECT node_a, node_id, subs_table.level + 1 FROM nodes_link JOIN subs_table ON node_id = node_b "
                                 (format "WHERE type in (%s) " link-labels-string)
                                 (when (numberp levels)
                                   (format "AND level < %d " levels))))
                 (if get-child-id
                     "SELECT node_id, child_id FROM subs_table"
                   "SELECT node_id FROM subs_table"))))
    query))



(defun pkm2--compile-full-db-query (query-plist)
  (message "q-plist: %S" query-plist)
  (let* ((query query-plist)
         (output
          (-reduce-from
           (lambda (current-output single-query-spec)
             (cond ((equal (car single-query-spec) :or)
                    (if current-output (format "%s UNION %s" current-output (pkm2--compile-db-query (cdr single-query-spec)))
                      (pkm2--compile-db-query (cdr single-query-spec))))
                   ((equal (car single-query-spec) :and)
                    (format "%s INTERSECT %s" current-output (pkm2--compile-db-query (cdr single-query-spec))))
                   ((equal (car single-query-spec) :not)
                    (format "%s EXCEPT %s" current-output (pkm2--compile-db-query (cdr single-query-spec))))
                   ((equal (car single-query-spec) :convert-and)
                    (pkm2--compile-db-query (cdr single-query-spec) current-output))
                   ((equal (car single-query-spec) :convert-or)
                    (format "SELECT id from node WHERE id IN (%s) OR  id IN (%s)" current-output (pkm2--compile-db-query (cdr single-query-spec) current-output)))
                   ((equal (car single-query-spec) :compound-or)
                    (format "SELECT id from node WHERE id IN (%s) OR  id IN (%s)" current-output (pkm2--compile-full-db-query (cdr single-query-spec))))
                   ((equal (car single-query-spec) :compound-and)
                    (format "SELECT id from node WHERE id IN (%s) AND  id IN (%s)" current-output (pkm2--compile-full-db-query (cdr single-query-spec))))
                   (t (error "Got weird single-query-spec: %S" single-query-spec))))
           nil
           query)))
    output))

(defun pkm2--compile-db-query-kvd (type-values nodes-subquery)
  (cond ((plist-get type-values :kvd) ; TODO Test
         (pkm2--object-db-compile-query-to-get-nodes-with-link-to-kvd (plist-get type-values :kvd) nodes-subquery))
        ((and (plist-get type-values :value) (plist-get type-values :key) (plist-get type-values :data-type)) ;TODO Test
         (message "I is here")
         (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-values
          (plist-get type-values :key)
          (list (plist-get type-values :value))
          (plist-get type-values :data-type)
          nodes-subquery))
        ((and (plist-get type-values :choices) (plist-get type-values :key) (plist-get type-values :data-type) ) ;TODO Test
         (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-values
          (plist-get type-values :key)
          (plist-get type-values :choices)
          (plist-get type-values :data-type)
          nodes-subquery))
        ((and (or (plist-get type-values :after) (plist-get type-values :before) ) (plist-get type-values :key) (plist-get type-values :data-type) ) ; TODO Test
         (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-value-in-range
          (plist-get type-values :key)
          (plist-get type-values :after)
          (plist-get type-values :before)
          (plist-get type-values :data-type)
          nodes-subquery))
        ((and (plist-get type-values :key) (plist-get type-values :data-type)) ; TODO Test
         (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key
          (plist-get type-values :key)
          (plist-get type-values :data-type)
          nodes-subquery))
        (t (error "Unable to get nodes for nodes-get:\n%S" type-values))))

(defun pkm2--compile-db-query-all (type-values nodes-subquery)
  (pkm2--compile-get-all-nodes nodes-subquery))

(defun pkm2--compile-db-query-structure-type (type-values nodes-subquery)
  (pkm2--db-compile-query-get-nodes-of-structure-type (plist-get type-values :structure-name)))

(defun pkm2--compile-db-query-between (type-values nodes-subquery)
  (pkm2--db-compile-get-nodes-between
   (plist-get type-values :after)
   (plist-get type-values :before)
   nodes-subquery))

(defun pkm2--compile-db-query-children-num (type-values nodes-subquery)
  (pkm2--db-compile-get-nodes-with-num-children (plist-get type-values :children-num)))

(defun pkm2--compile-db-query-parents-num (type-values nodes-subquery)
  (pkm2--db-compile-get-nodes-with-num-parents (plist-get type-values :parent-num)))

(defun pkm2--compile-db-query-with-children (type-values nodes-subquery)
  (--> (plist-get type-values :children-gets)(pkm2--compile-get-nodes it)(pkm2--db-compile-get-nodes-with-children-with it nodes-subquery)))

(defun pkm2--compile-db-query-with-parent (type-values nodes-subquery)
  (--> (plist-get type-values :parent-gets)(pkm2--compile-get-nodes it)(pkm2--db-compile-get-nodes-with-parents-with it nodes-subquery)))

(defun pkm2--compile-db-query-text (type-values nodes-subquery)
  (pkm2--db-compile-query-get-node-with-text (plist-get type-values :text) nodes-subquery))

(defun pkm2--compile-db-query-convert-to-parent (type-values nodes-subquery)
  (when (not nodes-subquery)
    (error "convert-to-parents needs to supply a sub-query"))
  (pkm2--db-query-get-parent-nodes  (plist-get type-values :levels)
                                    (plist-get type-values :link-labels)
                                    nodes-subquery) )

(defun pkm2--compile-db-query-convert-to-children (type-values nodes-subquery)
  (when (not nodes-subquery)
    (error "convert-to-parents needs to supply a sub-query"))
  (pkm2--db-query-get-sub-nodes  (plist-get type-values :levels)
                                 (plist-get type-values :link-labels)
                                 nodes-subquery))

(defun pkm2--compile-db-query-db-id (type-values nodes-subquery)
  (format "SELECT id from node where id = %d" (plist-get type-values :db-id)))

(defun pkm2--compile-db-query-db-node-ids (type-values nodes-subquery)
  (format "SELECT id from node where id IN (%s)"  (--> (plist-get type-values :db-node-ids)
                                                       (-map #'number-to-string it)
                                                       (string-join it ", "))))

(defun pkm2--compile-db-query (single-query-spec &optional nodes-subquery)
  (if-let* ((type-strategies (plist-get pkm2--query-spec-options-plist (car single-query-spec)))
            (db-query-func (plist-get type-strategies :get-db-query))
            (db-query (funcall db-query-func (cadr single-query-spec) nodes-subquery)))
      db-query
      (error (format "Spec wrong: %S" single-query-spec))))

(defun pkm2--create-query (&optional  print-output initial-queries initial-action)
  (let* ((action-options '(:or :and :not :convert-or :convert-and))
         (prompts '(("Filter nodes down?" . :and)
                    ("Add nodes to current selection?" . :or)
                    ("Remove nodes" . :not)
                    ("Convert nodes to thier parents or children" . :convert-and)
                    ("Also get children or parents of current nodes." . :convert-or)
                    ("I'm done" .  "DONE")))
         (action (or initial-action :or))
         (options (doom-plist-keys pkm2--query-spec-options-plist)
                  ; '("all" "structure-type" "between" "kvd" "between" "children-num" "parent-num" "with-children" "with-parent" "text" "db-node")
                  )
         (convert-options '("convert-to-parents" "convert-to-children" "convert-dependent-to-parent"))
         (query-spec (-copy initial-queries)))
    (while  (member action action-options)
      (message "action: %S" action)
      (when print-output (funcall print-output query-spec) )
      (setq query-spec (-as-> (cond ((equal action :or)
                                     (--> (completing-read "How would you like to select nodes to add to current selection?" options)
                                          (intern it)
                                          (list it (pkm--convert-into-get-spec it))))
                                    ((equal action :and)
                                     (--> (completing-read "How would you filter current nodes?" options)
                                          (intern it)
                                          (list it (pkm--convert-into-get-spec it))))
                                    ((equal action :not)
                                     (--> (completing-read "How would you remove nodes from current selection?" options)
                                          (intern it)
                                          (list it (pkm--convert-into-get-spec it))))
                                    ((equal action :convert-and)
                                     (--> (completing-read "How would you like to convert current selection?" convert-options)
                                          (intern it)
                                          (list it (pkm--convert-into-get-spec it))))
                                    ((equal action :convert-or)
                                     (--> (completing-read "How would you like to convert current selection?" convert-options)
                                          (intern it)
                                          (list it (pkm--convert-into-get-spec it))))
                                    ((not (equal action "DONE"))
                                     (error "Something weird happened")))
                              it2
                              (pkm2--create-query-2 action (nth 0 it2) (nth 1 it2))
                              (list it2)
                              (-concat query-spec it2)
                              ))
      (when print-output (funcall print-output query-spec) )
      (setq action (--> (completing-read "What would you like to do next?" prompts)
                        (assoc-default it prompts))))
    query-spec))

(defvar pkm2--query-spec-options-plist ())
(defun pkm2--register-query-spec-option (spec-option-name inputs read-info-function convert-to-db-func)
  (setq pkm2--query-spec-options-plist (plist-put pkm2--query-spec-options-plist spec-option-name (list :inputs inputs  :read-info read-info-function :get-db-query convert-to-db-func))))

(defun pkm2--create-query-2 (action query-type query-type-inputs)
  `(,action ,query-type ,query-type-inputs))

(defun pkm--convert-into-get-spec (key)
  (--> (plist-get pkm2--query-spec-options-plist key)
       (plist-get it :read-info)
       (funcall it)))

(pkm2--register-query-spec-option 'structure-type '(:structure-name)  #'pkm--convert-into-get-spec-structure-type #'pkm2--compile-db-query-structure-type)
(pkm2--register-query-spec-option 'time-between '(:after :before)  #'pkm--convert-into-get-spec-between #'pkm2--compile-db-query-between)
(pkm2--register-query-spec-option 'kvd '(:key :value :choices :after :before) #'pkm--convert-into-get-spec-kvd #'pkm2--compile-db-query-kvd)
(pkm2--register-query-spec-option 'convert-to-parents '(:levels) #'pkm--convert-into-get-spec-covert-parent #'pkm2--compile-db-query-convert-to-parent)
(pkm2--register-query-spec-option 'convert-to-children '(:levels) #'pkm--convert-into-get-spec-covert-children #'pkm2--compile-db-query-convert-to-children)
(pkm2--register-query-spec-option 'text '(:text) #'pkm--convert-into-get-spec-text #'pkm2--compile-db-query-text)
(pkm2--register-query-spec-option 'all nil #'pkm--convert-into-get-spec-empty  #'pkm2--compile-db-query-all)
(pkm2--register-query-spec-option 'db-node '(:db-id) #'pkm--convert-into-get-spec-db-id #'pkm2--compile-db-query-db-id)
(pkm2--register-query-spec-option 'db-nodes '(:db-node-ids) #'pkm--convert-into-get-spec-db-node-ids #'pkm2--compile-db-query-db-node-ids)


(defun pkm--convert-into-get-spec-empty ())


(defun pkm--convert-into-get-spec-structure-type ()
  (list :structure-name (intern (completing-read "What structure-type?" (doom-plist-keys pkm-structure-undefined-schemas-plist)) )))


(defun pkm--convert-into-get-spec-between ()
  (let* ((what-to-get (completing-read "What would you like to set?" '("after" "before" "both")))
                (after (when (or (equal "after" what-to-get) (equal "both" what-to-get))
                         (--> (read-string "Time after:\n" (format-time-string "%FT%T%z" (current-time) ))
                              (date-to-time it)
                              (time-convert it 'integer))
                         ))
                (before (when (or (equal "before" what-to-get) (equal "both" what-to-get))
                          (--> (read-string "Time before:\n" (format-time-string "%FT%T%z" (current-time) ))
                               (date-to-time it)
                               (time-convert it 'integer))
                          )))
    (list :after after :before before)))


(defun pkm--convert-into-get-spec-kvd (&optional what-to-get key keys)
  (let* ((what-to-get (or what-to-get (completing-read "How would you like to filter by kvd?" '("key" "key and value" "key and choices" "key and number range")) ))
         (key (or key (completing-read "What key?" (or keys
                                                       (pkm2--db-query-get-all-keys-to-types-alist)) nil 'confirm) ))
         (type (--> (assoc-default key (pkm2--db-query-get-all-keys-to-types-alist ))
                    (if (length> it 1)
                        (completing-read "What type:" it)
                      (car it))
                    (intern it)) )
         (value (when (equal what-to-get "key and value")
                  (completing-read "What value?" (pkm2--db-query-get-all-values-with-key key type) )))
         (choices (when (equal what-to-get "key and choices")
                    (completing-read-multiple "What choices?" (pkm2--db-query-get-all-values-with-key key type))))
         (what-to-get-for-range
          (when (equal what-to-get "key and number range")
            (completing-read "What would you like to set?" '("after" "before" "both"))))
         (after (when (and what-to-get-for-range (or (equal "after" what-to-get-for-range) (equal "both" what-to-get-for-range)) )
                  (read-number "number greater than:\n" )))
         (before (when (and what-to-get-for-range (or (equal "before" what-to-get-for-range) (equal "both" what-to-get-for-range)) )
                   (read-number "number less than\n" )))
         (output (-concat (list :key key :data-type type)
                         (when value (list :value value))
                         (when choices (list :choices choices))
                         (when after (list :after after))
                         (when before (list :before before)))))
    output))

(defun pkm--convert-into-get-spec-text ()
  (list :text (read-string "What text would you like pkm to include?")))
(defun pkm--convert-into-get-spec-db-id ()
  (list :db-id (read-number "What is the id of node you would like?")))
(defun pkm--convert-into-get-spec-db-node-ids ()
  (list :db-node-ids
        (pkm2-nodes-search-multiple)))
(defun pkm--convert-into-get-spec-covert-parent ()
  (list :levels (intern (completing-read "How many levels of parents would you like?" `("1" "2" "3" "4" "5" "6" "7" "ALL")) ) ))
(defun pkm--convert-into-get-spec-covert-children ()
  (list :levels (read (completing-read "How many levels of children would you like?" `("1" "2" "3" "4" "5" "6" "7" "ALL")) ) ))

;;; db cleaner
(defun pkm2--db-find-kvds-with-no-links ()
  (let* ((query (concat "SELECT kvd.id FROM key_value_data_integer kvd "
                        "LEFT JOIN data_or_properties_link_integer link "
                        "ON kvd.id = link.key_value_data "
                        "WHERE link.node IS NULL;"))
         (output (sqlite-select pkm2-database-connection query)))
    output))
(defun pkm2--db-delete-kvds-with-no-links ()
  (-each (doom-plist-keys pkm2--key_value_data-type-to-table-plist)
    (lambda (type)
      (let* ((data-table (pkm2--db-get-kvd-data-table-for-type type))
             (select-query (concat (format "SELECT kvd.id FROM %s kvd " (pkm2--db-get-kvd-data-table-for-type type) )
                                   (format "LEFT JOIN %s link " (pkm2--db-get-kvd-link-table-for-type type))
                                   "ON kvd.id = link.key_value_data "
                                   "WHERE link.node IS NULL"))
             (delete-query (format "DELETE FROM %s WHERE id IN (%s)" data-table select-query)))
        (sqlite-execute pkm2-database-connection delete-query)))))

;;; log change

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

(provide 'pkm-new-core)
