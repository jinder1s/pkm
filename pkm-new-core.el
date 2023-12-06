;;; pkm-new-core.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-generic)
(require 'cl-macs)
(require 'pkm2-utils)
(defgroup pkm2 nil
  "Personal knowledge management package."
  :group 'outlines)

(require 'dash)

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
;; (advice-add #'sqlite-select :before #'pkm2--core-trace-sqlite-execute)

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

;; QUERY stuff

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
  ;; TODO test
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
  ;; TODO test
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




(provide 'pkm-new-core)
