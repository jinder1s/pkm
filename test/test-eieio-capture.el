;;; test-eieio-capture.el  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jinder
;;
;; Author: Jinder <jinder1s@gmail.com>
;; Maintainer: Jinder <jinder1s@gmail.com>
;; Created: February 07, 2024
;; Modified: February 07, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/msingh15/test-eieio-capture
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(when (require 'undercover nil t)
  (undercover "pkm-new-core.el"
              (:report-file "/tmp/local-report.txt")
              (:report-format 'text)
              (:send-report nil)))
(require 'pkm-new-core)

(describe "Eieio object definition tests"
  (it "Define base node"
    (let* ((node-asset (node-schema :name "base-node" :primary-node t))
           (schema (object-schema
                    :name "base-n"
                    :assets (list node-asset)))
           (object-def (schema-compile schema)))
      (expect object-def :not :to-be nil)
      (expect (oref object-def :assets) :not :to-be nil)
      (expect (length (oref object-def :assets)) :to-be 1)
      (expect (car (oref object-def :assets)) :not :to-be node-asset)
      (expect (car (oref object-def :assets)) :to-equal node-asset)))


  (it "Define node with parent"
    (let* ((node-asset (node-schema :name "base-node" :primary-node t))
           (base-schema (object-schema
                         :name "base-n"
                         :assets (list node-asset)))
           (pkm-structure-2-undefined-schemas-plist (list base-schema))
           (new-schema (object-schema
                        :parent-schema-name "base-n"))


           (object-def (schema-compile new-schema)))
      (expect object-def :not :to-be nil)
      (expect (oref object-def :assets) :not :to-be nil)
      (expect (length (oref object-def :assets)) :to-be 1)
      (expect (car (oref object-def :assets)) :not :to-be node-asset)
      (expect (car (oref object-def :assets)) :to-equal node-asset)))
  (it "Define base node with modifications"
    (let* ((node-asset (node-schema :name "base-node" :content "Hello" :primary-node t))
           (mod (list :name "base-node" :plist-key :content :plist-value "World"))
           (schema (object-schema
                    :name "base-n"
                    :assets (list node-asset)
                    :asset-modifications (list mod)))
           (object-def (schema-compile schema))
           (object-assets (oref object-def :assets)))
      (expect object-def :not :to-be nil)
      (expect object-assets :not :to-be nil)
      (expect (length object-assets) :to-be 1)
      (expect (car object-assets) :not :to-be node-asset)
      (expect (car object-assets) :not :to-equal node-asset)
      (expect (oref (car (oref object-def :assets)) :content) :to-equal "World")))

  (it "Define base node with kvd with behavior"
    (let* ((node-asset (node-schema :name "base-node" :content "Hello" :primary-node t))
           (kvd-key "node-type")
           (kvd-value "first")
           (kvd-asset (kvd-schema :name "is-first-note"
                                  :key kvd-key
                                  :value kvd-value
                                  :data-type 'TEXT))
           (schema (object-schema
                    :name "base-n"
                    :assets (list node-asset)
                    :behaviors '((:name "test-behavior" :link-to (primary)))))
           (pkm-structure-2-defined-behavior-plist (list
                                            (behavior-schema :name "test-behavior"
                                                             :assets (list kvd-asset))))
           (object-def (schema-compile schema))
           (object-assets (oref object-def :assets)))
      (expect object-def :not :to-be nil)
      (expect  object-assets :not :to-be nil)
      (expect (length  object-assets) :to-be 2)
      (expect (car  object-assets) :not :to-be node-asset)
      (expect (cadr  object-assets)  :to-equal node-asset)
      (expect (car  object-assets) :not :to-equal kvd-asset)
      (expect (oref (car  object-assets) :key) :to-equal kvd-key)
      (expect (oref (car  object-assets) :value) :to-equal kvd-value))))

(describe "EIEIO Object creation tests"
  :var (database-file)
  (before-each
    (setq database-file (make-temp-file "pkm-test" nil ".sqlite3"))
    (setq pkm2-database-connection (sqlite-open database-file))
    (pkm2-setup-database pkm2-database-connection))
  (it "Creating a basic node with kvd(node-type, first)"
    (let* ((node-content "First node content")
           (kvd-key "node-type")
           (kvd-value "first")
           (s-schema (object-schema :name 'basic
                                    :assets (list
                                             (node-schema :name "first-node" :primary-node t)
                                             (kvd-schema :name "is-first-note"
                                                         :key kvd-key
                                                         :value kvd-value
                                                         :link-to '(primary)
                                                         :data-type 'TEXT))))
           (compiled-schema (schema-compile s-schema))
           (values `(("first-node" . ,node-content)))
           (captured-structure (pkm-capture-test compiled-schema values))
           (committed-structure (pkm-commit-test captured-structure))
           (sql-query "SELECT id, content, created_at, modified_at FROM node;")
           (type 'TEXT)
           (data-table (pkm2--db-get-kvd-data-table-for-type type))
           (link-table (pkm2--db-get-kvd-link-table-for-type type))
           (kvd-sql-query (format "SELECT id, key, value, created_at FROM %s;" data-table))
           (link-sql-query (format "SELECT id, node, key_value_data, created_at FROM %s;" link-table))
           (database-nodes (sqlite-select pkm2-database-connection sql-query))
           (database-kvds (sqlite-select pkm2-database-connection kvd-sql-query))
           (database-kvd-links (sqlite-select pkm2-database-connection link-sql-query))
           kvd
           node)
      (expect (length database-nodes) :to-equal 1)
      (expect (length database-kvds) :to-equal 1)
      (expect (length database-kvd-links) :to-equal 1)
      (expect (nth 1 (car database-nodes)) :to-equal node-content)
      (expect (nth 3 (car database-nodes)) :to-equal nil) ; modified_at should be nil
      (setq node (pkm2--db-query-get-node-with-id 1))
      (expect (oref node :content) :to-equal node-content)
      (expect (oref node :children-links) :to-be nil)
      (expect (oref node :parent-links) :to-be nil)
      (expect (oref node :previous-sequencial-links) :to-be nil)
      (expect (oref node :next-sequencial-links) :to-be nil)
      (expect (oref node :flat-links) :to-be nil)
      (expect (length (oref node :kvds)) :to-be 1)
      (expect (nth 1 (car database-kvds)) :to-equal kvd-key)
      (expect (nth 2 (car database-kvds)) :to-equal kvd-value)
      (setq kvd (pkm2--db-get-or-insert-kvd kvd-key kvd-value))
      (expect (oref kvd :key) :to-equal kvd-key)
      (expect (oref kvd :value) :to-equal kvd-value)))
(it "Creating a basic node with kvd group"
    (let* ((node-content "First node content")
           (kvd-key "node-type")
           (kvd-value "first")
           (kvd-key2 "node-type2")
           (kvd-value2 "second")
           (group (kvd-group-schema :kvds (list "is-first-note" "is-first-note-2") :name "test-group"))
           (s-schema (object-schema :name 'basic
                                    :assets (list
                                             (node-schema :name "first-node" :primary-node t)
                                             (kvd-schema :name "is-first-note"
                                                         :key kvd-key
                                                         :value kvd-value
                                                         :link-to '(primary)
                                                         :data-type 'TEXT)
                                             (kvd-schema :name "is-first-note-2"
                                                         :key kvd-key2
                                                         :value kvd-value2
                                                         :link-to '(primary)
                                                         :data-type 'TEXT))
                                    :groups (list group)))
           (compiled-schema (schema-compile s-schema))
           (values `(("first-node" . ,node-content)))
           (captured-structure (pkm-capture-test compiled-schema values))
           (committed-structure (pkm-commit-test captured-structure))
           (sql-query "SELECT id, content, created_at, modified_at FROM node;")
           (type 'TEXT)
           (data-table (pkm2--db-get-kvd-data-table-for-type type))
           (link-table (pkm2--db-get-kvd-link-table-for-type type))
           (kvd-sql-query (format "SELECT id, key, value, created_at FROM %s;" data-table))
           (link-sql-query (format "SELECT id, node, key_value_data, created_at, groups FROM %s;" link-table))
           (database-nodes (sqlite-select pkm2-database-connection sql-query))
           (database-kvds (sqlite-select pkm2-database-connection kvd-sql-query))
           (database-kvd-links (sqlite-select pkm2-database-connection link-sql-query))
           kvd
           node)

      (expect (oref compiled-schema :groups) :to-equal (list group))
      (expect (length database-nodes) :to-equal 1)
      (expect (length database-kvds) :to-equal 2)
      (expect (length database-kvd-links) :to-equal 2)
      (expect (nth 1 (car database-nodes)) :to-equal node-content)
      (expect (nth 3 (car database-nodes)) :to-equal nil) ; modified_at should be nil
      (setq node (pkm2--db-query-get-node-with-id 1))
      (expect (oref node :content) :to-equal node-content)
      (expect (oref node :children-links) :to-be nil)
      (expect (oref node :parent-links) :to-be nil)
      (expect (oref node :previous-sequencial-links) :to-be nil)
      (expect (oref node :next-sequencial-links) :to-be nil)
      (expect (oref node :flat-links) :to-be nil)
      (expect (length (oref node :kvds)) :to-be 2)
      (expect (nth 1 (cadr database-kvds)) :to-equal kvd-key)
      (expect (nth 2 (cadr database-kvds)) :to-equal kvd-value)
      (expect (nth 1 (car database-kvds)) :to-equal kvd-key2)
      (expect (nth 2 (car database-kvds)) :to-equal kvd-value2)
      (expect (nth 4 (car database-kvd-links)) :to-equal (nth 4 (cadr database-kvd-links)))
      (setq kvd (pkm2--db-get-or-insert-kvd kvd-key kvd-value))
      (expect (oref kvd :key) :to-equal kvd-key)
      (expect (oref kvd :value) :to-equal kvd-value)
      (setq kvd2 (pkm2--db-get-or-insert-kvd kvd-key2 kvd-value2))
      (expect (oref kvd2 :key) :to-equal kvd-key2)
      (expect (oref kvd2 :value) :to-equal kvd-value2))))

(provide 'test-eieio-capture)
;;; test-eieio-capture.el ends here
