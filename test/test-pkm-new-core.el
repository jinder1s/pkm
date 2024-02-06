;;; test-core.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jinder
;;
;; Author: Jinder <jinder1s@gmail.com>
;; Maintainer: Jinder <jinder1s@gmail.com>
;; Created: January 03, 2024
;; Modified: January 03, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/msingh15/test-core
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(when (require 'undercover nil t)
  (undercover "pkm-new-core.el"
              (:report-file "/tmp/local-report.txt")
              (:report-format 'text)
              (:send-report nil)))

(require 'pkm-new-core)

(describe "A suite"
  (it "contains a spec with an expectation"
    (expect t :to-be t)))

(describe "Database core tests"
  :var (database-file)
  (before-each
    (setq database-file (make-temp-file "pkm-test" nil ".sqlite3"))
    (setq pkm2-database-connection (sqlite-open  database-file) )
    (pkm2-setup-database pkm2-database-connection))
  (it "Database creation: make sure rerunning setup database does not throw error"
    (expect (pkm2-setup-database pkm2-database-connection) :not :to-throw))
  (it "Inserting node"
    (let* ((content "Hello, This is my first node.")
           (timestamp (pkm2-get-current-timestamp))
           (sql-query "SELECT id, content, created_at, modified_at FROM node;")
           database-nodes
           node)
      (pkm2--db-insert-node content timestamp)
      (setq database-nodes (sqlite-select pkm2-database-connection sql-query))
      (expect (length database-nodes ) :to-equal 1)
      (expect (nth 1 (car database-nodes)) :to-equal content)
      (expect (nth 2 (car database-nodes)) :to-equal timestamp) ; created_at
      (expect (nth 3 (car database-nodes)) :to-equal nil) ; modified_at should be nil
      (setq node (pkm2--db-query-get-node-with-id 1))
      (expect (--> (pkm2-node-db-node node) (pkm2-db-node-content it)) :to-equal content)
      (expect (--> (pkm2-node-db-node node) (pkm2-db-node-created_at it)) :to-equal timestamp)
      (expect (pkm2-node-children-links node) :to-be nil)
      (expect (pkm2-node-parent-links node) :to-be nil)
      (expect (pkm2-node-previous-sequencial-links node) :to-be nil)
      (expect (pkm2-node-next-sequencial-links node) :to-be nil)
      (expect (pkm2-node-flat-links node) :to-be nil)
      (expect (pkm2-node-kvds node) :to-be nil)))

  (it "Inserting text kvd"
    (let* ((key "test-key")
           (value "test-value")
           (type 'TEXT)
           (timestamp (pkm2-get-current-timestamp))
           (data-table (pkm2--db-get-kvd-data-table-for-type type))
           (sql-query (format "SELECT id, key, value, created_at FROM %s;" data-table))
           database-nodes
           kvd)
      (pkm2--db-insert-kvd key value timestamp type)
      (setq database-nodes (sqlite-select pkm2-database-connection sql-query))
      (expect (length database-nodes ) :to-equal 1)
      (expect (nth 1 (car database-nodes)) :to-equal key)
      (expect (nth 2 (car database-nodes)) :to-equal value)
      (expect (nth 3 (car database-nodes)) :to-equal timestamp) ; created_at
      (setq kvd (pkm2--db-get-or-insert-kvd key value type))
      (expect (pkm2-db-kvd-key kvd) :to-equal key)
      (expect (pkm2-db-kvd-value kvd) :to-equal value)
      (expect (pkm2-db-kvd-created_at kvd) :to-equal timestamp)))

(it "Inserting text kvd without specifying type. It should default to TEXT"
    (let* ((key "test-key")
           (value "test-value")
           (type 'TEXT)
           (timestamp (pkm2-get-current-timestamp))
           (data-table (pkm2--db-get-kvd-data-table-for-type type))
           (sql-query (format "SELECT id, key, value, created_at FROM %s;" data-table))
           database-kvds
           kvd)
      (pkm2--db-insert-kvd key value timestamp)
      (setq database-kvds (sqlite-select pkm2-database-connection sql-query))
      (expect (length database-kvds ) :to-equal 1)
      (expect (nth 1 (car database-kvds)) :to-equal key)
      (expect (nth 2 (car database-kvds)) :to-equal value)
      (expect (nth 3 (car database-kvds)) :to-equal timestamp) ; created_at
      (setq kvd (pkm2--db-get-or-insert-kvd key value))
      (expect (pkm2-db-kvd-key kvd) :to-equal key)
      (expect (pkm2-db-kvd-value kvd) :to-equal value)
      (expect (pkm2-db-kvd-created_at kvd) :to-equal timestamp))))


(describe "Object definition tests"
  (it "Define base node"
    (let* ((node-asset '(:pkm-type node :name "base-node" :primary-node t))
           (schema  (list :assets
                          (list
                           node-asset)))
           (object-def (progn
                         (pkm-register-structure 'base-n schema)
                         (pkm--object-define 'base-n))))
      (expect object-def :not :to-be nil)
      (expect (plist-get object-def :assets) :not :to-be nil)
      (expect (length (plist-get object-def :assets) ) :to-be 1)
      (expect (car (plist-get object-def :assets)) :to-be node-asset)))
  (it "Define sub-object"
    (let* ((node-asset '(:pkm-type node :name "base-node" :primary-node t))
           (schema  (list :assets
                          (list
                           node-asset)))
           (object-def (progn
                         (pkm-register-structure 'base-n schema)
                         (pkm--object-define 'base-n))))
      (expect object-def :not :to-be nil)
      (expect (plist-get object-def :assets) :not :to-be nil)
      (expect (length (plist-get object-def :assets) ) :to-be 1)
      (expect (car (plist-get object-def :assets)) :to-be node-asset))))

(describe "Object creation tests"
  :var (database-file)
  (before-each
    (setq database-file (make-temp-file "pkm-test" nil ".sqlite3"))
    (setq pkm2-database-connection (sqlite-open  database-file) )
    (pkm2-setup-database pkm2-database-connection))
  (it "Creating a basic node with kvd(node-type, first)"
    (let* ((node-content "First node content")
           (kvd-key "node-type")
           (kvd-value "first")
           (structure-schema (list :name 'basic
                                   :assets (-non-nil
                                            (list
                                             `(:pkm-type node :name "first-node" :primary-node t)
                                             `(:pkm-type kvd :name "is-first-note" :key ,kvd-key :value ,kvd-value :link-to (primary) :data-type TEXT)))))

           (values `(("first-node" . ,node-content)))
           (sql-query "SELECT id, content, created_at, modified_at FROM node;")
           (type 'TEXT)
           (data-table (pkm2--db-get-kvd-data-table-for-type type))
           (kvd-sql-query (format "SELECT id, key, value, created_at FROM %s;" data-table))

           database-nodes
           database-kvds
           kvd
           node)
      (expect
       (pkm2--object-capture-object-verify
        structure-schema
        nil
        t
        values) :not :to-throw)
      (setq database-nodes (sqlite-select pkm2-database-connection sql-query))
      (expect (length database-nodes ) :to-equal 1)
      (expect (nth 1 (car database-nodes)) :to-equal node-content)
      (expect (nth 3 (car database-nodes)) :to-equal nil) ; modified_at should be nil
      (setq node (pkm2--db-query-get-node-with-id 1))
      (expect (--> (pkm2-node-db-node node) (pkm2-db-node-content it)) :to-equal node-content)
      (expect (pkm2-node-children-links node) :to-be nil)
      (expect (pkm2-node-parent-links node) :to-be nil)
      (expect (pkm2-node-previous-sequencial-links node) :to-be nil)
      (expect (pkm2-node-next-sequencial-links node) :to-be nil)
      (expect (pkm2-node-flat-links node) :to-be nil)
      (expect (length (pkm2-node-kvds node)) :to-be  1)


      (setq database-kvds (sqlite-select pkm2-database-connection kvd-sql-query))
      (expect (length database-kvds ) :to-equal 1)
      (expect (nth 1 (car database-kvds)) :to-equal kvd-key)
      (expect (nth 2 (car database-kvds)) :to-equal kvd-value)
      (setq kvd (pkm2--db-get-or-insert-kvd kvd-key kvd-value))
      (expect (pkm2-db-kvd-key kvd) :to-equal kvd-key)
      (expect (pkm2-db-kvd-value kvd) :to-equal kvd-value)
      )))


(describe "Object creation tests with kvd-group"
  :var (database-file)
  (before-each
    (setq database-file (make-temp-file "pkm-test" nil ".sqlite3"))
    (setq pkm2-database-connection (sqlite-open  database-file) )
    (pkm2-setup-database pkm2-database-connection))
  (it "Creating a basic node with kvd(node-type, first) and kvd-group(time-interval)"
    (let* ((node-content "First node content")
           (kvd-key "node-type")
           (kvd-value "first")
           (structure-schema (list :name 'basic
                                   :assets (-non-nil
                                            (list
                                             `(:pkm-type node :name "first-node" :primary-node t)
                                             `(:pkm-type kvd :name "is-first-note" :key ,kvd-key :value ,kvd-value :link-to (primary) :data-type TEXT)
                                             `(:pkm-type kvd-group
                                               :name time-interval
                                               :link-to (primary)
                                               :assets
                                               ((:pkm-type kvd :name "time-start" :key "time-start" :value ,#'pkm2-get-user-selected-timestamp :data-type DATETIME)
                                                (:pkm-type kvd :name "time-end" :key "time-end" :value ,#'pkm2-get-user-selected-timestamp :data-type DATETIME :managed t)
                                                (:pkm-type kvd :name "interval-type" :key "interval-type" :value "time-interval" :data-type TEXT)))))))

           (values `(("first-node" . ,node-content)
                     (time-interval . (("time-start" . ,(pkm2-get-current-timestamp) )
                                       ("time-end" . ,(pkm2-get-current-timestamp) )))))
           (sql-query "SELECT id, content, created_at, modified_at FROM node;")
           (text-kvd-sql-query (format "SELECT id, key, value, created_at FROM %s;" (pkm2--db-get-kvd-data-table-for-type 'TEXT)))
           (datetime-kvd-sql-query (format "SELECT id, key, value, created_at FROM %s;" (pkm2--db-get-kvd-data-table-for-type 'DATETIME)))
           (datetime-kvd-links-sql-query (format "SELECT * FROM %s;" (pkm2--db-get-kvd-link-table-for-type 'DATETIME)))
           (text-kvd-links-sql-query (format "SELECT * FROM %s;" (pkm2--db-get-kvd-link-table-for-type 'TEXT)))

           database-nodes
           database-kvds
           kvd
           node)
      (expect
       (pkm2--object-capture-object-verify
        structure-schema
        nil
        t
        values) :not :to-throw)
      (setq database-nodes (sqlite-select pkm2-database-connection sql-query))
      (expect (length database-nodes ) :to-equal 1)
      (expect (nth 1 (car database-nodes)) :to-equal node-content)
      (expect (nth 3 (car database-nodes)) :to-equal nil) ; modified_at should be nil
      (setq node (pkm2--db-query-get-node-with-id 1))
      (expect (--> (pkm2-node-db-node node) (pkm2-db-node-content it)) :to-equal node-content)
      (expect (pkm2-node-children-links node) :to-be nil)
      (expect (pkm2-node-parent-links node) :to-be nil)
      (expect (pkm2-node-previous-sequencial-links node) :to-be nil)
      (expect (pkm2-node-next-sequencial-links node) :to-be nil)
      (expect (pkm2-node-flat-links node) :to-be nil)
      (expect (length (pkm2-node-kvds node)) :to-be  4)


      (setq database-kvds (sqlite-select pkm2-database-connection text-kvd-sql-query))
      (message "Database kvds: %s" database-kvds)
      (expect (length database-kvds ) :to-equal 2)
      (expect (nth 1 (car database-kvds)) :to-equal kvd-key)
      (expect (nth 2 (car database-kvds)) :to-equal kvd-value)
      (setq kvd (pkm2--db-get-or-insert-kvd kvd-key kvd-value))
      (expect (pkm2-db-kvd-key kvd) :to-equal kvd-key)
      (expect (pkm2-db-kvd-value kvd) :to-equal kvd-value)

      (setq datetime-database-kvds (sqlite-select pkm2-database-connection datetime-kvd-sql-query))
      (message "datetime Database kvds: %s" datetime-database-kvds)
      (expect (length datetime-database-kvds ) :to-equal 2)

      (setq datetime-database-kvd-links (sqlite-select pkm2-database-connection datetime-kvd-links-sql-query))
      (message "datetime Database kvd links: %s %s" datetime-database-kvd-links datetime-kvd-links-sql-query)

      (setq text-database-kvd-links (sqlite-select pkm2-database-connection text-kvd-links-sql-query))
      (message "text Database kvd links: %s %s" text-database-kvd-links text-kvd-links-sql-query)
      )))



;;; test-core.el ends here
