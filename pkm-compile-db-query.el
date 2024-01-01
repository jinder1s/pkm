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

(defun pkm2--compile-db-query-kvd-2 (type-values nodes-table)
  (message "nodes-table: %S" nodes-table)
  (cond ((plist-get type-values :kvd) ; TODO Test
         (pkm2--object-db-compile-query-to-get-nodes-with-link-to-kvd-2 (plist-get type-values :kvd) nodes-table))
        ((and (plist-get type-values :value) (plist-get type-values :key) (plist-get type-values :data-type)) ;TODO Test
         (message "I is here")
         (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-values-2
          (plist-get type-values :key)
          (list (plist-get type-values :value))
          (plist-get type-values :data-type)
          nodes-table))
        ((and (plist-get type-values :choices) (plist-get type-values :key) (plist-get type-values :data-type)) ;TODO Test
         (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-values-2
          (plist-get type-values :key)
          (plist-get type-values :choices)
          (plist-get type-values :data-type)
          nodes-table))
        ((and (or (plist-get type-values :after) (plist-get type-values :before)) (plist-get type-values :key) (plist-get type-values :data-type)) ; TODO Test
         (message "Doing the right thing: %S, %S" (plist-get type-values :after) (plist-get type-values :before))
         (let* ((after (plist-get type-values :after))
                (after (if (and (plistp after) (length> after 1))
                           (truncate (ts-unix (apply #'ts-adjust (-concat after (list (ts-now))))))
                         after))
                (before (plist-get type-values :before))
                (before  (if (and (plistp before) (length> before 1))
                             (truncate (ts-unix (apply #'ts-adjust (-concat before (list (ts-now))))))
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
                                        "task-status" 'TEXT "node")))))

         (length (-distinct (-flatten (sqlite-select
                                       pkm2-database-connection
                                       (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key
                                        "task-status" 'TEXT "node")))))))



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




(defun pkm2--object-db-compile-query-to-get-nodes-with-link-to-kvd-2 (kvd &optional node-table)
  (let* ((key (plist-get kvd :key))
         (values (cond ((list (plist-get kvd :value)))
                       ((plist-get kvd :choices))))
         (type (plist-get kvd :data-type)))
    (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-and-values-2 key values type node-table)))

(defun pkm2--object-db-query-get-nodes-with-links-to-kvds (kvds &optional node-subquery)
  (-reduce-from (lambda (init-subquery kvd)
                  (pkm2--object-db-compile-query-to-get-nodes-with-link-to-kvd kvd init-subquery))
                node-subquery
                kvds))



(defun pkm2--db-compile-query-get-nodes-of-structure-type-2 (type-values &optional nodes-table)
  (let* ((nodes-table (or nodes-table "node"))
         (structure-name (plist-get type-values :structure-name))
         (unique-required (plist-get pkm-structure-unique-required-plist structure-name))
         (u-r-keys (--> (car unique-required) (-filter (lambda (kvd) (member (plist-get kvd :key) it))
                                                       (cadr unique-required))))
         (first-key-kvd-spec (car u-r-keys))
         (first-key (plist-get first-key-kvd-spec :key))
         (first-key-type (plist-get first-key-kvd-spec :data-type))
         (fully-specified-kvds (pkm--object-get-required-fully-specified-kvds2 (cadr unique-required)))
         (easiest-queriable-kvds
          (list (cond ((-find (lambda (kvd)
                                (--> (plist-get kvd :value) (or (stringp it) (numberp it)))) fully-specified-kvds))
                      ((-find (lambda (kvd)
                                (plist-get kvd :choices)) fully-specified-kvds))))))
    (message "%S, %S, %S" unique-required u-r-keys easiest-queriable-kvds)
    (cond (u-r-keys (pkm2--db-compile-query-get-nodes-with-links-to-kvds-with-key-2 first-key first-key-type nodes-table))
          (easiest-queriable-kvds (pkm2--compile-full-db-query-common (-map-indexed (lambda (index kvd)
                                                                                      (if (equal index 0)
                                                                                          `(:or kvd (:kvd ,kvd))
                                                                                        `(:and kvd (:kvd ,kvd))))
                                                                                    easiest-queriable-kvds)
                                                                      nodes-table))
          (t (error "Unable to get nodes for structure-type %S" structure-name)))))



(defun pkm2--db-compile-get-nodes-between-2 (type-values &optional node-table)
                                        ; created_at and modified_at between after and before
                                        ; Node linked to any kvd with datetime type between after and before
                                        ; If any of the nodes above are dependent types, get its parent node
  (let* ((after (plist-get type-values :after))
         (after (if (and (plistp after) (length> after 1))
                    (truncate (ts-unix (apply #'ts-adjust (-concat after (list (ts-now))))))
                  after))
         (before (plist-get type-values :before))
         (before  (if (and (plistp before) (length> before 1))
                      (truncate (ts-unix (apply #'ts-adjust (-concat before (list (ts-now))))))
                    before))
         (keys (progn (pkm--object-get-time-related-kvd-keys) nil ))
         (pkm-kvd-queries (-map (lambda (key) `(:or kvd (:key ,key :data-type DATETIME))) keys))
         (node-pkm-queries `((:or created-at (:after ,after :before ,before))
                             (:or modified-at (:after ,after :before ,before))))
         (pkm-queries (-concat node-pkm-queries pkm-kvd-queries))
         (query (pkm2--compile-full-db-query-common pkm-queries node-table)))
    query))


(defun pkm2--db-compile-get-nodes-created-at-2 (type-values &optional node-table)
  (let* ((after (plist-get type-values :after))
         (after (if (and (plistp after) (length> after 1))
                    (truncate (ts-unix (apply #'ts-adjust (-concat after (list (ts-now))))))
                  after))
         (before (plist-get type-values :before))
         (before  (if (and (plistp before) (length> before 1))
                      (truncate (ts-unix (apply #'ts-adjust (-concat before (list (ts-now))))))
                    before))
         (query (concat (format "SELECT node.id FROM %1$s JOIN node ON node.id = %1$s.id  " node-table)
                        "WHERE ("
                        (when after (format "node.created_at > %d "  after))
                        (when before (format "OR node.created_at < %d"  before))
                        ") ")))
    query))

(defun pkm2--db-compile-get-nodes-modified-at-2 (type-values &optional node-table)
  (let* ((after (plist-get type-values :after))
         (after (if (and (plistp after) (length> after 1))
                    (truncate (ts-unix (apply #'ts-adjust (-concat after (list (ts-now))))))
                  after))
         (before (plist-get type-values :before))
         (before  (if (and (plistp before) (length> before 1))
                      (truncate (ts-unix (apply #'ts-adjust (-concat before (list (ts-now))))))
                    before))
         (query (concat (format "SELECT node.id FROM %1$s JOIN node ON node.id = %1$s.id  " node-table)
                        "WHERE ("
                        (when after (format " node.modified_at > %d " after))
                        (when before (format "OR  node.modified_at < %d " before))
                        ") ")))
    query))

(defun pkm2--db-compile-query-get-node-with-text-2 (type-values &optional nodes-table)
  (concat
   (format "SELECT %1$s.id from %1$s JOIN search_node on search_node.node = %1$s.id " nodes-table)
   (format "WHERE search_node MATCH '%s' " (plist-get type-values :text))))

(defun test-compare-search-with-text ()
  (equal (length (-distinct (-flatten (sqlite-select
                                       pkm2-database-connection
                                       (pkm2--db-compile-query-get-node-with-text-2
                                         "personal" "node")))))

         (length (-distinct (-flatten (sqlite-select
                                       pkm2-database-connection
                                       (pkm2--db-compile-query-get-node-with-text
                                        "personal" "node")))))))


(defun pkm2--db-query-get-sub-nodes-2 (type-values nodes-table &optional get-parent-id)
  ; TODO TEST
  (let* ((levels (plist-get type-values :levels))
         (link-labels (plist-get type-values :link-labels))
         (link-labels (or link-labels (plist-get pkm-links-type-to-label-eq-plist 'HIERARCHICAL)))
         (link-labels-string (--> (-map #'pkm2--db-convert-object-to-string link-labels)
                                  (string-join it ", ")))
         (query (concat
                 (format "WITH RECURSIVE subs_table(node_id, parent_id, level) AS (%s) "
                         (concat (format "SELECT nodes_link.node_b, nodes_link.node_a, 1 FROM %s JOIN nodes_link ON %1$s.id = nodes_link.node_a WHERE nodes_link.type in (%s) "
                                         nodes-table
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



(defun pkm2--db-query-get-parent-nodes-2 (type-values nodes-table &optional get-child-id)
  (when (not nodes-table)
    (error "convert-to-parents needs to supply a sub-query"))
                                        ; TODO modify to only return query
  (let* ((levels (plist-get type-values :levels))
         (link-labels (plist-get type-values :link-labels))
         (link-labels (or link-labels (plist-get pkm-links-type-to-label-eq-plist 'HIERARCHICAL)))
         (link-labels-string (--> (-map #'pkm2--db-convert-object-to-string link-labels)
                                  (string-join it ", ")))
         (query (concat
                 (format "WITH RECURSIVE subs_table(node_id, child_id, level) AS (%s) "
                         (concat (format "SELECT nodes_link.node_a, nodes_link.node_b, 1 FROM %1$s JOIN nodes_link ON %1$s.id = nodes_link.node_b WHERE nodes_link.type in (%s) "
                                         nodes-table
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
(defun pkm2--compile-db-query-2 (single-query-spec &optional nodes-table)
  (message "q-1: %S" nodes-table)
  (if-let* ((type-strategies (plist-get pkm2--query-spec-options-plist (car single-query-spec)))
            (db-query-func (plist-get type-strategies :get-db-query-2))
            (db-query (funcall db-query-func (cadr single-query-spec) nodes-table)))
      db-query
    (error (format "Spec wrong: %S" single-query-spec))))



(defun pkm2--compile-full-db-query-common (query-plist &optional nodes-table)
  (message "nodes table: %S" nodes-table)
  (let* ((query query-plist)
         (reduced-output
          (-reduce-from
           (lambda (current-output single-query-spec)
             (if current-output
                 (let* ((current-db-query (cdr current-output))
                        (last-index (car current-output))
                        (next-index (+ last-index 1))
                        (current-query-spec (cdr single-query-spec))
                        (current-expression-name (format "%sv%d" (or nodes-table "") next-index))
                        (last-expression-name (format "%sv%d" (or nodes-table "") last-index))
                        (base-expression-name (format "%sv" (or nodes-table "") ) ))
                   (cond ((equal (car single-query-spec) :or)
                          (let* ((second-index (+ next-index 1))
                                 (second-expression-name (format "%s%d" base-expression-name second-index)))
                            (cons second-index
                                  (format "%1$s, %2$s(id) as ( %s), %s(id) as (SELECT id from %2$s UNION SELECT id from %5$s) "
                                          current-db-query
                                          current-expression-name
                                          (pkm2--compile-db-query-2 current-query-spec "node")
                                          second-expression-name
                                          last-expression-name)) ))
                         ((equal (car single-query-spec) :and)
                          (cons  next-index
                                 (format "%s, %s(id) as (%s) "
                                         current-db-query
                                         current-expression-name
                                         (pkm2--compile-db-query-2 current-query-spec last-expression-name))))
                         ((equal (car single-query-spec) :not)
                          (cons next-index
                                (format "%s, %s(id) as (SELECT id from %s WHERE id not in (%s))"
                                        current-db-query
                                        current-expression-name
                                        last-expression-name
                                        (pkm2--compile-db-query-2 current-query-spec last-expression-name))))
                         ((equal (car single-query-spec) :convert-and)
                          (cons next-index
                                (format "%s, %s(id) as (%s)"
                                        current-db-query
                                        current-expression-name
                                        (pkm2--compile-db-query-2 current-query-spec last-expression-name))))
                         ((equal (car single-query-spec) :convert-or)
                          (let* ((second-index (+ next-index 1))
                                 (second-expression-name (format "%s%d" base-expression-name second-index)))
                            (cons second-index
                                  (format "%1$s, %2$s(id) as ( %s), %s(id) as (SELECT id from %2$s UNION SELECT id from %5$s) "
                                          current-db-query
                                          current-expression-name
                                          (pkm2--compile-db-query-2 current-query-spec last-expression-name)
                                          second-expression-name
                                          last-expression-name))))
                         ((equal (car single-query-spec) :compound-or)
                          (let* ((second-index (+ next-index 1))
                                 (second-expression-name (format "%s%d" base-expression-name second-index)))
                            (cons second-index
                                  (format "%1$s, %2$s(id) as ( %s), %3$s(id) as (SELECT id from %2$s UNION SELECT id from %5$s) "
                                          current-db-query
                                          current-expression-name
                                          (pkm2--compile-full-db-query-common current-query-spec)
                                          second-expression-name
                                          last-expression-name))))
                         ((equal (car single-query-spec) :compound-and)
                          (cons next-index (format "%s, %s(id) as (%s) "
                                                   current-db-query
                                                   current-expression-name
                                                   (pkm2--compile-full-db-query-common current-query-spec last-expression-name))))
                         (t (error "Got weird single-query-spec: %S" (car single-query-spec)))))
               (cons 1 (format "WITH %sv1(id) as (%s) " (or nodes-table "") (pkm2--compile-db-query-2 (cdr single-query-spec) nodes-table)))))
           nil
           query))
         (output (format "%s SELECT * FROM %sv%d"
                         (cdr reduced-output)
                         (or nodes-table "")
                         (car reduced-output))))
    output))

(defun test-compile-full-db-query-common-1 ()
  (let* ((pkm-query '((:or structure-type (:structure-name log-n))
                      (:and time-between (:after (day -20) :before nil))))
         (query1 (pkm2--compile-full-db-query-common pkm-query))
         (query2 (pkm2--compile-full-db-query pkm-query))
         (query1-output (sqlite-select pkm2-database-connection query1))
         (query2-output (sqlite-select pkm2-database-connection query2)))
    (equal (length (-distinct query1-output ))
           (length (-distinct query2-output)))

    ;; query
    ))

(defun test-compile-full-db-query-common-2 ()
  (let* ((pkm-query '((:or structure-type (:structure-name log-n))
                      (:or structure-type (:structure-name project-s))
                      (:convert-or convert-to-children (:levels 1))
                      ))
         (query1 (pkm2--compile-full-db-query-common pkm-query))
         (query2 (pkm2--compile-full-db-query pkm-query))
         (query1-output (sqlite-select pkm2-database-connection query1))
         (query2-output (sqlite-select pkm2-database-connection query2))
         )
    (equal (length (-distinct query1-output ))
           (length (-distinct query2-output)))

    ;; query1
    ))
(defun test-rename ()
  (--> "WITH blah as (SELECT 1), beep as blah SELECT * from beep"
       (sqlite-select pkm2-database-connection it))
  )




(defun pkm2--compile-db-query-convert-to-children-2 (type-values nodes-table)
  (when (not nodes-table)
    (error "convert-to-parents needs to supply a sub-query"))
  (pkm2--db-query-get-sub-nodes-2
   (plist-get type-values :levels)
   (plist-get type-values :link-labels)
   nodes-table))

(defun pkm2--compile-db-query-db-id-2 (type-values nodes-subquery)
  (format "SELECT id from node where id = %d" (plist-get type-values :db-id)))

(defun pkm2--compile-db-query-db-node-ids-2 (type-values nodes-subquery)
  (format "SELECT id from node where id IN (%s)"  (--> (plist-get type-values :db-node-ids)
                                                       (-map #'number-to-string it)
                                                       (string-join it ", "))))
(pkm2--register-query-spec-option 'structure-type '(:structure-name)  #'pkm--convert-into-get-spec-structure-type #'pkm2--compile-db-query-structure-type #'pkm2--db-compile-query-get-nodes-of-structure-type-2)
(pkm2--register-query-spec-option 'time-between '(:after :before)  #'pkm--convert-into-get-spec-between #'pkm2--compile-db-query-between #'pkm2--db-compile-get-nodes-between-2)
(pkm2--register-query-spec-option 'created-at '(:after :before)  #'pkm--convert-into-get-spec-between #'pkm2--compile-db-query-between #'pkm2--db-compile-get-nodes-created-at-2)
(pkm2--register-query-spec-option 'modified-at '(:after :before)  #'pkm--convert-into-get-spec-between #'pkm2--compile-db-query-between #'pkm2--db-compile-get-nodes-modified-at-2)
(pkm2--register-query-spec-option 'kvd '(:key :value :choices :after :before :kvd) #'pkm--convert-into-get-spec-kvd #'pkm2--compile-db-query-kvd #'pkm2--compile-db-query-kvd-2)
(pkm2--register-query-spec-option 'convert-to-parents '(:levels) #'pkm--convert-into-get-spec-covert-parent #'pkm2--compile-db-query-convert-to-parent-2 #'pkm2--db-query-get-parent-nodes-2)
(pkm2--register-query-spec-option 'convert-to-children '(:levels) #'pkm--convert-into-get-spec-covert-children #'pkm2--compile-db-query-convert-to-children #'pkm2--db-query-get-sub-nodes-2)
(pkm2--register-query-spec-option 'text '(:text) #'pkm--convert-into-get-spec-text #'pkm2--compile-db-query-text #'pkm2--db-compile-query-get-node-with-text-2)
(pkm2--register-query-spec-option 'all nil #'pkm--convert-into-get-spec-empty  #'pkm2--compile-db-query-all #'pkm2--compile-db-query-all-2)
(pkm2--register-query-spec-option 'db-node '(:db-id) #'pkm--convert-into-get-spec-db-id #'pkm2--compile-db-query-db-id #'pkm2--compile-db-query-db-id-2)
(pkm2--register-query-spec-option 'db-nodes '(:db-node-ids) #'pkm--convert-into-get-spec-db-node-ids #'pkm2--compile-db-query-db-node-ids #'pkm2--compile-db-query-db-node-ids-2)
(provide 'pkm-compile-db-query)
;;; pkm-compile-db-query.el ends here
