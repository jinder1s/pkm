;;; pkm-data-change.el -*- lexical-binding: t; -*-

(require 'dash)
(require 'pkm-new-core)
(defun change-key-value-to-integer (db)
  (let* ((query1 "INSERT INTO data_or_properties_link_integer (node, key_value_data, created_at) SELECT node, 61, created_at FROM data_or_properties_link WHERE key_value_data = 18;")
         (query "DELETE FROM key_value_data WHERE key = 'todo-priority';"))
    ;; (sqlite-execute db query1)

    (sqlite-execute db query)))

(defun change-link-from-a-to-b (db kvd-a kvd-b &optional type)
  (let* ((query (format "UPDATE %s SET key_value_data = %d WHERE key_value_data = %d" (pkm2--db-get-kvd-link-table-for-type (or type 'TEXT)) kvd-b kvd-a )))
    (sqlite-execute db query)))


(defun change-link-from-a-to-b-verbose (a-key a-value b-key b-value type)
  (let* ((a-id (--> (pkm2--db-get-or-insert-kvd a-key a-value type) (pkm2-db-kvd-id it)))
         (b-id (--> (pkm2--db-get-or-insert-kvd b-key b-value type) (pkm2-db-kvd-id it))))
    (change-link-from-a-to-b pkm2-database-connection a-id b-id type)))


(defun change-todo-key ()
  (change-link-from-a-to-b-verbose "status" "TODO" "task-status" "TODO" 'TEXT)
  (change-link-from-a-to-b-verbose "status" "DONE" "task-status" "DONE" 'TEXT)
  (change-link-from-a-to-b-verbose "status" "DOING" "task-status" "DOING" 'TEXT)
  (change-link-from-a-to-b-verbose "status" "KILL" "task-status" "KILL" 'TEXT)
  (change-link-from-a-to-b-verbose "todo-status" "TODO" "task-status" "TODO" 'TEXT)
  (change-link-from-a-to-b-verbose "todo-priority" 3 "task-priority" 3 'INTEGER)
  (change-link-from-a-to-b-verbose "priority" 1 "task-priority" 1 'INTEGER))

(defun change-tag-to-node-type ()
  (change-link-from-a-to-b-verbose "tag" "project" "node-type" "project" 'TEXT)
  (change-link-from-a-to-b-verbose "tag" "log" "node-type" "log" 'TEXT)
  (change-link-from-a-to-b-verbose "tag" "clock" "node-type" "clock" 'TEXT)
  (change-link-from-a-to-b-verbose "tag" "DEPENDENT" "node-type" "DEPENDENT" 'TEXT)
  (change-link-from-a-to-b-verbose "tag" "note" "node-type" "note" 'TEXT)
  (change-link-from-a-to-b-verbose "tag" "work" "node-type" "work" 'TEXT)

  )

(defun swap-link-noda-a-and-node-b ()

  (sqlite-execute pkm2-database-connection "UPDATE nodes_link SET node_a = node_b, node_b = node_a; ")
  )

(defun update-link-type-log-to-sub ()

  (sqlite-execute pkm2-database-connection "UPDATE nodes_link  SET type = 'sub' WHERE type = 'log';")
  )


(defun move-data-over ()
  (sqlite-execute pkm2-database-connection "ATTACH database '/USERS/msingh15/.doom.d/test_pkm.sqlite3' as old_db")

  ;; (sqlite-execute pkm2-database-connection "INSERT INTO node (id, content, created_at, modified_at)  SELECT ROWID, text, created_at, modified_at FROM old_db.node;")
  ;; (sqlite-execute pkm2-database-connection "INSERT INTO nodes_link (id, type, created_at, node_a, node_b)  SELECT ROWID, type, created_at, node_a, node_b FROM old_db.nodes_link;")

  ;; (sqlite-execute pkm2-database-connection "INSERT INTO key_value_data (id, created_at, key, value)  SELECT ROWID, created_at, key, value FROM old_db.key_value_data;")
  ;; (sqlite-execute pkm2-database-connection "INSERT INTO key_value_data_integer (id, created_at, key, value)  SELECT ROWID, created_at, key, value FROM old_db.key_value_data_integer;")
  ;; (sqlite-execute pkm2-database-connection "INSERT INTO key_value_data_real (id, created_at, key, value)  SELECT ROWID, created_at, key, value FROM old_db.key_value_data_real;")
  ;; (sqlite-execute pkm2-database-connection "INSERT INTO key_value_data_blob (id, created_at, key, value)  SELECT ROWID, created_at, key, value FROM old_db.key_value_data_blob;")



  ;; (sqlite-execute pkm2-database-connection "INSERT INTO data_or_properties_link (id, created_at, node, key_value_data)  SELECT ROWID, created_at, node, key_value_data FROM old_db.data_or_properties_link;")
  ;; (sqlite-execute pkm2-database-connection "INSERT INTO data_or_properties_link_integer (id, created_at, node, key_value_data)  SELECT ROWID, created_at, node, key_value_data FROM old_db.data_or_properties_link_integer;")
  ;; (sqlite-execute pkm2-database-connection "INSERT INTO data_or_properties_link_real (id, created_at, node, key_value_data)  SELECT ROWID, created_at, node, key_value_data FROM old_db.data_or_properties_link_real;")
  ;; (sqlite-execute pkm2-database-connection "INSERT INTO data_or_properties_link_blob (id, created_at, node, key_value_data)  SELECT ROWID, created_at, node, key_value_data FROM old_db.data_or_properties_link_blob;")
  (sqlite-execute pkm2-database-connection "DETACH database 'old_db';")
  )

(defun add-column-to-table (database table column-name column-type)
  (--> (format "ALTER TABLE %s ADD %s %s;" table column-name column-type)
       (sqlite-execute database it)))

(defun add-archive-table ()
  (add-column-to-table pkm2-database-connection "nodes_link" "is_archive" "INTEGER")
  (add-column-to-table pkm2-database-connection "data_or_properties_link" "is_archive" "INTEGER")
  (add-column-to-table pkm2-database-connection "data_or_properties_link_integer" "is_archive" "INTEGER")
  (add-column-to-table pkm2-database-connection "data_or_properties_link_real" "is_archive" "INTEGER")
  (add-column-to-table pkm2-database-connection "data_or_properties_link_blob" "is_archive" "INTEGER"))

(provide 'pkm-data-change)
