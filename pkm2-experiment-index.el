;;; pkm/pkm2-experiment-index.el -*- lexical-binding: t; -*-

(defun pkm2--index-node-children (node-id)
  (let* ((query-for-children (concat
                              "WITH RECURSIVE subs_table(top, parent, node_id, level) AS "
                              (format "(%s)" (concat
                                              (format "SELECT node_a, node_a, node_b, 1 FROM nodes_link WHERE type in ('log', 'type-of', 'clock', 'sub') AND node_a = %d " node-id )
                                              "UNION "
                                              "SELECT subs_table.top, node_a, node_b, subs_table.level + 1 FROM nodes_link JOIN subs_table ON node_id = node_a WHERE type in ('log', 'type-of', 'clock', 'sub')"
                                              ))
                              "SELECT top, parent, node_id, level FROM subs_table"
                              ))
         (view-statement (format "CREATE VIEW  test_project_%d (top, parent, node_id, level) AS %s;" node-id query-for-children)))
    ;; (sqlite-execute  jinder_dbh view-statement)
    (sqlite-select jinder_dbh query-for-children)
    ;; view-statement
    ))

;;


(defun pkm2--index-kvd (database_handle)
  )

(provide 'pkm2-experiment-index)
