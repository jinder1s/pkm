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
      (expect (oref node :content) :to-equal content)
      (expect (oref node :created_at) :to-equal timestamp)
      (expect (oref node :children-links) :to-be nil)
      (expect (oref node :parent-links) :to-be nil)
      (expect (oref node :previous-sequencial-links) :to-be nil)
      (expect (oref node :next-sequencial-links) :to-be nil)
      (expect (oref node :flat-links) :to-be nil)
      (expect (oref node :kvds) :to-be nil)))

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
      (expect (oref kvd :key) :to-equal key)
      (expect (oref kvd :value) :to-equal value)
      (expect (oref kvd :created_at) :to-equal timestamp)))

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
      (expect (oref kvd :key) :to-equal key)
      (expect (oref kvd :value) :to-equal value)
      (expect (oref kvd :created_at) :to-equal timestamp))))
;;; test-core.el ends here
