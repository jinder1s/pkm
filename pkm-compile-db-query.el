;;; pkm-compile-db-query.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jinder
;;
;; Author: Jinder <jinder1s@gmail.com>
;; Maintainer: Jinder <jinder1s@gmail.com>
;; Created: December 30, 2023
;; Modified: December 30, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/manjindersingh/pkm-compile-db-query
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

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

(defun pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-2 (key type &optional node-table)
  (let* ((data-table (pkm2--db-get-kvd-data-table-for-type type))
         (link-table (pkm2--db-get-kvd-link-table-for-type type))
         (query (concat (format "SELECT %1$s.id FROM %1$s JOIN %2$s ON %2$s.node = %1$s.id JOIN %3$s ON %2$s.key_value_data = %3$s.id WHERE %2$s.is_archive is NULL AND %3$s.key = '%4$s'"
                                node-table
                                link-table
                                data-table
                                key))))
    query))
(defun test-compare-kvds-with-key ()
  (equal (length (-distinct (-flatten (sqlite-select
                                       pkm2-database-connection
                                       (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-2
                                        "task-status" 'TEXT "node")) ) ) )

         (length (-distinct (-flatten (sqlite-select
                                       pkm2-database-connection
                                       (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key
                                        "task-status" 'TEXT "node")) ) ) ) ) )

(defun pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-values (key values type &optional node-subquery)
  (let* ((data-table (pkm2--db-get-kvd-data-table-for-type type))
         (link-table (pkm2--db-get-kvd-link-table-for-type type))
         (query (concat (format "SELECT link.node from %s as link WHERE link.is_archive is NULL AND link.key_value_data IN (SELECT id FROM %s WHERE key = '%s' AND value IN (%s))"
                                link-table data-table key
                                (--> (-map #'pkm2--db-convert-object-to-string values)
                                     (string-join it ", ")))
                        (when node-subquery (format "AND node in (%s)" node-subquery)))))
    query))

(defun pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-values-2 (key values type &optional node-table)
  (let* ((data-table (pkm2--db-get-kvd-data-table-for-type type))
         (link-table (pkm2--db-get-kvd-link-table-for-type type))
         (query (concat (format "SELECT %1$s.id FROM %1$s JOIN %2$s ON %2$s.node = %1$s.id JOIN %3$s ON %2$s.key_value_data = %3$s.id WHERE %2$s.is_archive is NULL AND %3$s.key = '%4$s' AND %3$s.value IN (%5$s)"
                                node-table
                                link-table
                                data-table
                                key
                                (--> (-map #'pkm2--db-convert-object-to-string values)
                                     (string-join it ", "))))))
    query))

(defun test-compare-kvds-with-key-and-values ()
  (equal (length (-distinct (-flatten (sqlite-select
                                       pkm2-database-connection
                                       (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-values
                                        "task-status" '("TODO" "DOING") 'TEXT "node")))))

         (length (-distinct (-flatten (sqlite-select
                                       pkm2-database-connection
                                       (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-values-2
                                        "task-status" '("TODO" "DOING") 'TEXT "node")))))))


(defun pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-value-in-range (key after before type &optional node-subquery)
  (if (or (eq type 'DATETIME) (eq type 'INTEGER) (eq type 'REAL))
      (let* ((data-table (pkm2--db-get-kvd-data-table-for-type type))
             (link-table (pkm2--db-get-kvd-link-table-for-type type))
             (kvd-subquery (concat (format "SELECT id FROM %s WHERE key = '%s' " data-table key)
                                   (when after (format "AND value >= %d " after))
                                   (when before (format "AND value <= %d " before))))
             (query (concat (format "SELECT link.node from %s as link WHERE link.is_archive is NULL AND link.key_value_data IN (%s) "
                                    link-table
                                    kvd-subquery)
                            (when node-subquery (format "AND node in (%s)" node-subquery)))))
        query)))

(defun pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-value-in-range-2 (key after before type &optional node-table)
  (if (or (eq type 'DATETIME) (eq type 'INTEGER) (eq type 'REAL))
      (let* ((data-table (pkm2--db-get-kvd-data-table-for-type type))
             (link-table (pkm2--db-get-kvd-link-table-for-type type))
             (query (concat (format "SELECT %1$s.id FROM %1$s JOIN %2$s ON %2$s.node = %1$s.id JOIN %3$s ON %2$s.key_value_data = %3$s.id WHERE %2$s.is_archive is NULL AND %3$s.key = '%4$s' "
                                    node-table
                                    link-table
                                    data-table
                                    key)
                            (format "AND %s.value >= (%s) " data-table after)
                            (format "AND %s.value <= (%s) " data-table before))))
        query)))

(defun test-compare-kvds-with-key-and-value-in-range ()
  (equal (length (-distinct (-flatten (sqlite-select
                                       pkm2-database-connection
                                       (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-value-in-range
                                        "task-priority" 2 4  'INTEGER "node")))))

         (length (-distinct (-flatten (sqlite-select
                                       pkm2-database-connection
                                       (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-value-in-range-2
                                        "task-priority" 2 4  'INTEGER "node")))))))


(defun pkm2--object-db-compile-query-to-get-nodes-with-link-to-kvd (kvd &optional node-subquery)
  (let* ((key (plist-get kvd :key))
         (values (cond ((list (plist-get kvd :value)) )
                       ((plist-get kvd :choices) )))
         (type (plist-get kvd :data-type)))
    (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-values key values type node-subquery)))

(defun pkm2--object-db-compile-query-to-get-nodes-with-link-to-kvd-2 (kvd &optional node-table)
  (let* ((key (plist-get kvd :key))
         (values (cond ((list (plist-get kvd :value)) )
                       ((plist-get kvd :choices) )))
         (type (plist-get kvd :data-type)))
    (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-values-2 key values type node-table)))

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

(defun pkm2--db-compile-query-get-nodes-of-structure-type-2 (structure-name &optional node-table)
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
    (cond (u-r-keys (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-2 first-key first-key-type node-table))
          (easiest-queriable-kvds (pkm2--compile-full-db-query-common (-map-indexed (lambda (index kvd)
                                                                                      (if (equal index 0)
                                                                                          `(:or kvd (:kvd ,kvd))
                                                                                        `(:and kvd (:kvd ,kvd))))
                                                                                    easiest-queriable-kvds) ))
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

(defun pkm2--db-compile-get-nodes-between-2 (after before &optional node-table)
                                        ; created_at and modified_at between after and before
                                        ; Node linked to any kvd with datetime type between after and before
                                        ; If any of the nodes above are dependent types, get its parent node
  (let* ((keys (pkm--object-get-time-related-kvd-keys))
         (pkm-kvd-queries (-map (lambda (key) `(:or kvd (:key ,key))) keys))
         (node-pkm-queries `((:or created-at (:after ,after :before ,before))
                             (:or modified-at (:after ,after :before ,before))))
         (pkm-queries (-concat node-pkm-queries pkm-kvd-queries))
         (query (pkm2--compile-full-db-query-common pkm-queries node-table)))
    query))


(defun pkm2--db-compile-get-nodes-created-at-2 (after before &optional node-table)
  (let* ((query (concat (format "SELECT node.id FROM %1$s JOIN node ON node.id = %1$s.id  " node-table)
                        "WHERE ("
                        (when after (format "node.created_at > %d "  after))
                        (when before (format "OR node.created_at < %d"  before))
                        ") ")))
    query))

(defun pkm2--db-compile-get-nodes-modified-at-2 (after before &optional node-table)
  (let* ((query (concat (format "SELECT node.id FROM %1$s JOIN node ON node.id = %1$s.id  " node-table)
                        "WHERE ("
                        (when after (format " node.modified_at > %d " after))
                        (when before (format "OR  node.modified_at < %d " before))
                        ") ")))
    query))






(defun pkm2--db-compile-query-get-node-with-text (text &optional node-subquery)
  (concat "SELECT node FROM search_node "
          (format "WHERE search_node MATCH '%s' " text)
          (when node-subquery (format "AND node in (%s)" node-subquery))))

(defun pkm2--db-compile-query-get-node-with-text-2 (text &optional node-table)
  (concat
   (format "SELECT %1$s.id from %1$s JOIN search_node on search_node.node = %1$s.id " node-table)
   (format "WHERE search_node MATCH '%s' " text)))

(defun test-compare-search-with-text ()
  (equal (length (-distinct (-flatten (sqlite-select
                                       pkm2-database-connection
                                       (pkm2--db-compile-query-get-node-with-text-2
                                         "personal" "node")) ) ) )

         (length (-distinct (-flatten (sqlite-select
                                       pkm2-database-connection
                                       (pkm2--db-compile-query-get-node-with-text
                                        "personal" "node")) ) ) ) ) )

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

(defun pkm2--db-query-get-sub-nodes-2 (levels link-labels node-table &optional get-parent-id)
  ; TODO TEST
  (let* ((link-labels (or link-labels (plist-get pkm-links-type-to-label-eq-plist 'HIERARCHICAL) ))
         (link-labels-string (--> (-map #'pkm2--db-convert-object-to-string link-labels)
                                              (string-join it ", ")))
         (query (concat
                 (format "WITH RECURSIVE subs_table(node_id, parent_id, level) AS (%s) "
                         (concat (format "SELECT nodes_link.node_b, nodes_link.node_a, 1 FROM %s JOIN nodes_link ON %1$s.id = nodes_link.node_a WHERE nodes_link.type in (%s) "
                                         node-table
                                         link-labels-string)
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

(defun pkm2--db-query-get-parent-nodes-2 (levels link-labels node-table &optional get-child-id)
                                        ; TODO modify to only return query
  (let* ((link-labels (or link-labels (plist-get pkm-links-type-to-label-eq-plist 'HIERARCHICAL) ))
         (link-labels-string (--> (-map #'pkm2--db-convert-object-to-string link-labels)
                                  (string-join it ", ")))
         (query (concat
                 (format "WITH RECURSIVE subs_table(node_id, child_id, level) AS (%s) "
                         (concat (format "SELECT nodes_link.node_a, nodes_link.node_b, 1 FROM %1$s JOIN nodes_link ON %1$s.id = nodes_link.node_b WHERE nodes_link.type in (%s) "
                                         node-table
                                         link-labels-string)
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
(defun pkm2--compile-full-db-query-common (query-plist &optional node-table)
  (let* ((query query-plist)
         (output
          (-reduce-from
           (lambda (current-output single-query-spec)
             (cond ((equal (car single-query-spec) :or)
                    (if current-output
                        (cons (+ (car current-output) 1) (format "%s, v%d(id) as (SELECT id from v%d UNION %s) "
                                                                 (cdr current-output)
                                                                 (+ (car current-output) 1)
                                                                 (car current-output)
                                                                 (pkm2--compile-db-query (cdr single-query-spec))) )
                      (cons 1 (format "WITH v1(id) as (%s) " (pkm2--compile-db-query (cdr single-query-spec))))))
                   ((equal (car single-query-spec) :and)
                    (cons  (+ (car current-output) 1)
                           (format "%s, v%d(id) as (%s) "
                                   (cdr current-output)
                                   (+ (car current-output) 1)
                                   (pkm2--compile-db-query (cdr single-query-spec) (format "v%d" (car current-output)))) ))
                   ((equal (car single-query-spec) :not)
                    (cons (+ (car current-output) 1)
                          (format "%s, v%d(id) as (SELECT id from v%d WHERE id not in (%s))"
                                  (cdr current-output)
                                  (+ (car current-output) 1)
                                  (car current-output)
                                  (pkm2--compile-db-query (cdr single-query-spec) (format "v%d" (car current-output)))) ))
                   ((equal (car single-query-spec) :convert-and)
                    (cons (+ (car current-output) 1)
                          (format "%s, v%d(id) as (%s)"
                                  (cdr current-output)
                                  (+ (car current-output) 1)
                                  (pkm2--compile-db-query (cdr single-query-spec) (format "v%d" (car current-output))) ) ))
                   ((equal (car single-query-spec) :convert-or)
                    (cons (+ (car current-output) 1) (format "%s, v%d(id) as (SELECT id from v%d UNION %s) "
                                                             (cdr current-output)
                                                             (+ (car current-output) 1)
                                                             (car current-output)
                                                             (pkm2--compile-db-query
                                                              (cdr single-query-spec)
                                                              (format "v%d" (car current-output))))))
                   ((equal (car single-query-spec) :compound-or)
                    (cons (+ (car current-output) 1) (format "%s, v%d(id) as (SELECT id from v%d UNION (%s)) "
                                                             (cdr current-output)
                                                             (+ (car current-output) 1)
                                                             (car current-output)
                                                             (pkm2--compile-full-db-query-common (cdr single-query-spec)))))
                   ((equal (car single-query-spec) :compound-and)
                    (cons (+ (car current-output) 1) (format "%s, v%d(id) as (%s) "
                                                             (cdr current-output)
                                                             (+ (car current-output) 1)
                                                             (pkm2--compile-full-db-query-common (cdr single-query-spec) (format "v%d" (car current-output))))))
                   (t (error "Got weird single-query-spec: %S" (car single-query-spec)))))
           nil
           query)))
    output))
(defun pkm2--compile-db-query-2 (single-query-spec &optional nodes-table-name)
  (if-let* ((type-strategies (plist-get pkm2--query-spec-options-plist (car single-query-spec)))
            (db-query-func (plist-get type-strategies :get-db-query-2))
            (db-query (funcall db-query-func (cadr single-query-spec) nodes-table-name)))
      db-query
    (error (format "Spec wrong: %S" single-query-spec))))

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
        ((and (or (plist-get type-values :after) (plist-get type-values :before)) (plist-get type-values :key) (plist-get type-values :data-type) ) ; TODO Test
         (message "Doing the right thing: %S, %S" (plist-get type-values :after) (plist-get type-values :before))
         (let* ((after (plist-get type-values :after))
                (after (if (and (plistp after) (length> after 1) )
                           (truncate (ts-unix (apply #'ts-adjust (-concat after (list (ts-now))))))
                         after))
                (before (plist-get type-values :before))
                (before  (if (and (plistp before) (length> before 1) )
                             (truncate (ts-unix (apply #'ts-adjust (-concat before (list (ts-now)))) ) )
                           before)))
           (message "after: %S, before: %S" after before)
           (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-value-in-range
            (plist-get type-values :key)
            after
            before
            (plist-get type-values :data-type)
            nodes-subquery)))
        ((and (plist-get type-values :key) (plist-get type-values :data-type)) ; TODO Test
         (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key
          (plist-get type-values :key)
          (plist-get type-values :data-type)
          nodes-subquery))
        (t (error "Unable to get nodes for nodes-get:\n%S" type-values))))

(defun pkm2--compile-db-query-kvd-2 (type-values nodes-table)
  (cond ((plist-get type-values :kvd) ; TODO Test
         (pkm2--object-db-compile-query-to-get-nodes-with-link-to-kvd-2 (plist-get type-values :kvd) nodes-table))
        ((and (plist-get type-values :value) (plist-get type-values :key) (plist-get type-values :data-type)) ;TODO Test
         (message "I is here")
         (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-values-2
          (plist-get type-values :key)
          (list (plist-get type-values :value))
          (plist-get type-values :data-type)
          nodes-table))
        ((and (plist-get type-values :choices) (plist-get type-values :key) (plist-get type-values :data-type) ) ;TODO Test
         (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-values-2
          (plist-get type-values :key)
          (plist-get type-values :choices)
          (plist-get type-values :data-type)
          nodes-table))
        ((and (or (plist-get type-values :after) (plist-get type-values :before)) (plist-get type-values :key) (plist-get type-values :data-type) ) ; TODO Test
         (message "Doing the right thing: %S, %S" (plist-get type-values :after) (plist-get type-values :before))
         (let* ((after (plist-get type-values :after))
                (after (if (and (plistp after) (length> after 1) )
                           (truncate (ts-unix (apply #'ts-adjust (-concat after (list (ts-now))))))
                         after))
                (before (plist-get type-values :before))
                (before  (if (and (plistp before) (length> before 1) )
                             (truncate (ts-unix (apply #'ts-adjust (-concat before (list (ts-now)))) ) )
                           before)))
           (message "after: %S, before: %S" after before)
           (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-value-in-range-2
            (plist-get type-values :key)
            after
            before
            (plist-get type-values :data-type)
            nodes-table)))
        ((and (plist-get type-values :key) (plist-get type-values :data-type)) ; TODO Test
         (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-2
          (plist-get type-values :key)
          (plist-get type-values :data-type)
          nodes-table))
        (t (error "Unable to get nodes for nodes-get:\n%S" type-values))))

(defun pkm2--compile-db-query-all (type-values nodes-subquery)
  (pkm2--compile-get-all-nodes nodes-subquery))

(defun pkm2--compile-db-query-structure-type (type-values nodes-subquery)
  (pkm2--db-compile-query-get-nodes-of-structure-type (plist-get type-values :structure-name)))

(defun pkm2--compile-db-query-structure-type-2 (type-values nodes-table)
  (pkm2--db-compile-query-get-nodes-of-structure-type (plist-get type-values :structure-name) nodes-table))

(defun pkm2--compile-db-query-between (type-values nodes-subquery)
  (let* ((after (plist-get type-values :after))
         (after (if (and (plistp after) (length> after 1) )
                    (truncate (ts-unix (apply #'ts-adjust (-concat after (list (ts-now))))))
                  after))
         (before (plist-get type-values :before))
         (before  (if (and (plistp before) (length> before 1) )
                      (truncate (ts-unix (apply #'ts-adjust (-concat before (list (ts-now)))) ) )
                    before)))
    (pkm2--db-compile-get-nodes-between
     after
     before
     nodes-subquery) ))

(defun pkm2--compile-db-query-between-2 (type-values nodes-table)
  (let* ((after (plist-get type-values :after))
         (after (if (and (plistp after) (length> after 1) )
                    (truncate (ts-unix (apply #'ts-adjust (-concat after (list (ts-now))))))
                  after))
         (before (plist-get type-values :before))
         (before  (if (and (plistp before) (length> before 1) )
                      (truncate (ts-unix (apply #'ts-adjust (-concat before (list (ts-now)))) ) )
                    before)))
    (pkm2--db-compile-get-nodes-between-2
     after
     before
     nodes-table)))


(defun pkm2--compile-db-query-text (type-values nodes-subquery)
  (pkm2--db-compile-query-get-node-with-text (plist-get type-values :text) nodes-subquery))

(defun pkm2--compile-db-query-text-2 (type-values nodes-table)
  (pkm2--db-compile-query-get-node-with-text-2 (plist-get type-values :text) nodes-table))

(defun pkm2--compile-db-query-convert-to-parent (type-values nodes-subquery)
  (when (not nodes-subquery)
    (error "convert-to-parents needs to supply a sub-query"))
  (pkm2--db-query-get-parent-nodes  (plist-get type-values :levels)
                                    (plist-get type-values :link-labels)
                                    nodes-subquery) )

(defun pkm2--compile-db-query-convert-to-parent-2 (type-values nodes-table)
  (when (not nodes-table)
    (error "convert-to-parents needs to supply a sub-query"))
  (pkm2--db-query-get-parent-nodes-2
   (plist-get type-values :levels)
   (plist-get type-values :link-labels)
   nodes-table) )

(defun pkm2--compile-db-query-convert-to-children (type-values nodes-subquery)
  (when (not nodes-subquery)
    (error "convert-to-parents needs to supply a sub-query"))
  (pkm2--db-query-get-sub-nodes
   (plist-get type-values :levels)
   (plist-get type-values :link-labels)
   nodes-subquery))

(defun pkm2--compile-db-query-convert-to-children-2 (type-values nodes-table)
  (when (not nodes-table)
    (error "convert-to-parents needs to supply a sub-query"))
  (pkm2--db-query-get-sub-nodes-2
   (plist-get type-values :levels)
   (plist-get type-values :link-labels)
   nodes-table))

(defun pkm2--compile-db-query-db-id (type-values nodes-subquery)
  (format "SELECT id from node where id = %d" (plist-get type-values :db-id)))

(defun pkm2--compile-db-query-db-node-ids (type-values nodes-subquery)
  (format "SELECT id from node where id IN (%s)"  (--> (plist-get type-values :db-node-ids)
                                                       (-map #'number-to-string it)
                                                       (string-join it ", "))))

(defun pkm2--compile-db-query-db-id-2 (type-values nodes-subquery)
  (format "SELECT id from node where id = %d" (plist-get type-values :db-id)))

(defun pkm2--compile-db-query-db-node-ids-2 (type-values nodes-subquery)
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
                    ("Done and name query" . "DONE-NAME")
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
                              (-concat query-spec it2)))
      (when print-output (funcall print-output query-spec) )
      (setq action (--> (completing-read "What would you like to do next?" prompts)
                        (assoc-default it prompts))))
    (if (equal action "DONE-NAME")
        (--> (read-string (format "Name for: %S" query-spec))
             (-concat pkm2-browse-saved-named-queries (list (cons it (format "%S" query-spec )) ))
             (setq pkm2-browse-saved-named-queries it))
      (--> (-concat pkm2-browse-saved-queries (list (format "%S" query-spec) ))
           (setq pkm2-browse-saved-queries it)))
    (persist-save 'pkm2-browse-saved-queries)
    (persist-save 'pkm2-browse-saved-named-queries)
    query-spec))


(defun pkm2--create-query-filter (&optional action)
  (let* ((action-options '(:or :and :not :convert-or :convert-and :name :combine-or :combine-and))
         (action (or action (intern (completing-read "What action would you like to do?" action-options) ) ))
         (options (doom-plist-keys pkm2--query-spec-options-plist))
         (convert-options '("convert-to-parents" "convert-to-children" "convert-dependent-to-parent"))
         (query-spec (-as-> (cond ((equal action :or)
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
                                  ((equal action :name)
                                   (list nil (list :name (read-string "Name for current selection: "))))
                                  ((not (equal action "DONE"))
                                   (error "Something weird happened")))
                            it2
                            (pkm2--create-query-2 action (nth 0 it2) (nth 1 it2)))))

    query-spec))

(defvar pkm2--query-spec-options-plist ())
(defun pkm2--register-query-spec-option (spec-option-name inputs read-info-function convert-to-db-func &optional convert-to-db-func-2)
  (setq pkm2--query-spec-options-plist (plist-put pkm2--query-spec-options-plist spec-option-name (list :inputs inputs  :read-info read-info-function :get-db-query convert-to-db-func))))

(defun pkm2--create-query-2 (action query-type query-type-inputs)
  `(,action ,query-type ,query-type-inputs))

(defun pkm--convert-into-get-spec (key)
  (--> (plist-get pkm2--query-spec-options-plist key)
       (plist-get it :read-info)
       (funcall it)))



(pkm2--register-query-spec-option 'structure-type '(:structure-name)  #'pkm--convert-into-get-spec-structure-type #'pkm2--compile-db-query-structure-type #'pkm2--compile-db-query-structure-type-2)
(pkm2--register-query-spec-option 'time-between '(:after :before)  #'pkm--convert-into-get-spec-between #'pkm2--compile-db-query-between #'pkm2--compile-db-query-between-2)
(pkm2--register-query-spec-option 'created-at '(:after :before)  #'pkm--convert-into-get-spec-between #'pkm2--compile-db-query-between #'pkm2--db-compile-get-nodes-created-at-2)
(pkm2--register-query-spec-option 'modified-at '(:after :before)  #'pkm--convert-into-get-spec-between #'pkm2--compile-db-query-between #'pkm2--db-compile-get-nodes-modified-at-2)
(pkm2--register-query-spec-option 'kvd '(:key :value :choices :after :before :kvd) #'pkm--convert-into-get-spec-kvd #'pkm2--compile-db-query-kvd #'pkm2--compile-db-query-kvd-2)
(pkm2--register-query-spec-option 'convert-to-parents '(:levels) #'pkm--convert-into-get-spec-covert-parent #'pkm2--compile-db-query-convert-to-parent #'pkm2--compile-db-query-convert-to-parent-2)
(pkm2--register-query-spec-option 'convert-to-children '(:levels) #'pkm--convert-into-get-spec-covert-children #'pkm2--compile-db-query-convert-to-children #'pkm2--compile-db-query-convert-to-children-2)
(pkm2--register-query-spec-option 'text '(:text) #'pkm--convert-into-get-spec-text #'pkm2--compile-db-query-text #'pkm2--compile-db-query-text-2)
(pkm2--register-query-spec-option 'all nil #'pkm--convert-into-get-spec-empty  #'pkm2--compile-db-query-all #'pkm2--compile-db-query-all-2)
(pkm2--register-query-spec-option 'db-node '(:db-id) #'pkm--convert-into-get-spec-db-id #'pkm2--compile-db-query-db-id #'pkm2--compile-db-query-db-id-2)
(pkm2--register-query-spec-option 'db-nodes '(:db-node-ids) #'pkm--convert-into-get-spec-db-node-ids #'pkm2--compile-db-query-db-node-ids #'pkm2--compile-db-query-db-node-ids-2)


(defun pkm--convert-into-get-spec-empty ())


(defun pkm--convert-into-get-spec-structure-type ()
  (list :structure-name (intern (completing-read "What structure-type?" (doom-plist-keys pkm-structure-undefined-schemas-plist)) )))


(defun pkm--convert-into-get-spec-between ()
  (let* ((what-to-get (completing-read "What would you like to set?" '("after" "before" "both")))
         (relative-time (y-or-n-p "Default is absolute time. Would you like to switch to relative time now?"))

         (after (when (or (equal "after" what-to-get) (equal "both" what-to-get))
                  (if relative-time
                      (let* ((relative-by (completing-read-multiple "How would you like to specify after?"
                                                                    '("hour" "day" "minute" "second" "week" "month")))
                             (units-amounts (-map (lambda (time-unit)
                                                    (cons (intern time-unit ) (read-number (format "Relative by %s?" time-unit)) )) relative-by))
                             output)

                        (-each units-amounts (lambda (unit-amount) (setq output (-concat output (list (car unit-amount) (cdr unit-amount))))))
                        output)
                      (--> (read-string "Time after:\n" (format-time-string "%FT%T%z" (current-time) ))
                           (date-to-time it)
                           (time-convert it 'integer)) )
                  ))
         (before (when (or (equal "before" what-to-get) (equal "both" what-to-get))
                   (if relative-time
                       (let* ((relative-by (completing-read-multiple "How would you like to specify before?"
                                                                     '("hour" "day" "minute" "second" "week" "month")))
                              (units-amounts (-map (lambda (time-unit)
                                                     (cons (intern time-unit ) (read-number (format "Relative by %s?" time-unit)) )) relative-by))
                              output)

                         (-each units-amounts (lambda (unit-amount) (setq output (-concat output (list (car unit-amount) (cdr unit-amount))))))
                         output)
                     (--> (read-string "Time before:\n" (format-time-string "%FT%T%z" (current-time) ))
                          (date-to-time it)
                          (time-convert it 'integer)) )
                   )))
    (list :after after :before before)))


(defun pkm--convert-into-get-spec-kvd (&optional what-to-get key keys)
  (let* ((what-to-get (or what-to-get (completing-read "How would you like to filter by kvd?" '("key" "key and value" "key and choices" "key and number range" "key and time range"))))
         (key (or key (completing-read "What key?" (or keys
                                                       (pkm2--db-query-get-all-keys-to-types-alist)) nil 'confirm) ))
         (type (assoc-default key (pkm2--db-query-get-all-keys-to-types-alist )))
         (value (when (equal what-to-get "key and value")
                  (completing-read "What value?" (pkm2--db-query-get-all-values-with-key key type) )))
         (choices (when (equal what-to-get "key and choices")
                    (completing-read-multiple "What choices?" (pkm2--db-query-get-all-values-with-key key type))))
         (what-to-get-for-range
          (when (equal what-to-get "key and number range")
            (completing-read "What would you like to set?" '("after" "before" "both"))))
         (what-to-get-for-time-range
          (when (equal what-to-get "key and time range")
            (completing-read "What would you like to set?" '("after" "before" "both" "after-relative" "both-relative" "before-relative"))))
         (after (when (and what-to-get-for-range (or (equal "after" what-to-get-for-range) (equal "both" what-to-get-for-range)) )
                  (read-number "number greater than:\n" )))
         (before (when (and what-to-get-for-range (or (equal "before" what-to-get-for-range) (equal "both" what-to-get-for-range)) )
                   (read-number "number less than\n" )))
         (after (if (member what-to-get-for-time-range (list "after" "both" "after-relative" "both-relative"))
                    (if (member what-to-get-for-time-range (list "after-relative" "both-relative"))
                        (let* ((relative-by (completing-read-multiple "How would you like to specify after?"
                                                                      '("hour" "day" "minute" "second" "week" "month")))
                               (units-amounts (-map (lambda (time-unit)
                                                      (cons (intern time-unit ) (read-number (format "Relative by %s?" time-unit)) )) relative-by))
                               output)
                          (-each units-amounts (lambda (unit-amount) (setq output (-concat output (list (car unit-amount) (cdr unit-amount))))))
                          output)
                      (--> (read-string "Time after:\n" (format-time-string "%FT%T%z" (current-time) ))
                           (date-to-time it)
                           (time-convert it 'integer)) )
                  after
                  ))
         (before (if (member what-to-get-for-time-range (list "before" "both" "before-relative" "both-relative"))
                     (if (member what-to-get-for-time-range (list "before-relative" "both-relative"))
                         (let* ((relative-by (completing-read-multiple "How would you like to specify before?"
                                                                       '("hour" "day" "minute" "second" "week" "month")))
                                (units-amounts (-map (lambda (time-unit)
                                                       (cons (intern time-unit ) (read-number (format "Relative by %s?" time-unit)) )) relative-by))
                                output)

                           (-each units-amounts (lambda (unit-amount) (setq output (-concat output (list (car unit-amount) (cdr unit-amount))))))
                           output)
                       (--> (read-string "Time before:\n" (format-time-string "%FT%T%z" (current-time) ))
                            (date-to-time it)
                            (time-convert it 'integer)))
                   before))
         (output (-concat (list :key key :data-type type)
                          (when value (list :value value))
                          (when choices (list :choices choices))
                          (when after (list :after after))
                          (when before (list :before before)))))
    (when (and (or after before) (not (or (equal type 'INTEGER) (equal type 'REAL))))
      (error "kvd type is not a number"))
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





(provide 'pkm-compile-db-query)
;;; pkm-compile-db-query.el ends here
