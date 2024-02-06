;;; test-sync.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jinder
;;
;; Author: Jinder <jinder1s@gmail.com>
;; Maintainer: Jinder <jinder1s@gmail.com>
;; Created: January 21, 2024
;; Modified: January 21, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/msingh15/test-sync
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

(describe "Sync Tests"
  :var (database-file
        events
        logged-events
        )
  (before-each
    (setq database-file (make-temp-file "pkm-test" nil ".sqlite3"))
    (setq pkm2-database-connection (sqlite-open database-file))
    (pkm2-setup-database pkm2-database-connection)
    (setq events (list :main ())))

  (it "Creating sync event for insert node"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))
           (content "Hello, This is my first node.")
           (timestamp (pkm2-get-current-timestamp))
           (sql-query "SELECT id, content, created_at, modified_at FROM node;")
           database-nodes
           node)
      (pkm2--db-insert-node content timestamp)
      (setq database-nodes (sqlite-select pkm2-database-connection sql-query))
      (expect (length database-nodes) :to-equal 1)
      (expect (nth 1 (car database-nodes)) :to-equal content)
      (expect (nth 2 (car database-nodes)) :to-equal timestamp) ; created_at
      (expect (nth 3 (car database-nodes)) :to-equal nil) ; modified_at should be nil
      (setq node (pkm2--db-query-get-node-with-id 1))
      (expect (--> (pkm2-node-db-node node) (pkm2-db-node-content it)) :to-equal content)
      (expect (--> (pkm2-node-db-node node) (pkm2-db-node-created_at it)) :to-equal timestamp)
      (expect (length (plist-get events :main)) :to-be 1)))
  (it "Creating sync event for inserting text kvd"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))
           (key "test-key")
           (value "test-value")
           (type 'TEXT)
           (timestamp (pkm2-get-current-timestamp))
           (data-table (pkm2--db-get-kvd-data-table-for-type type))
           (sql-query (format "SELECT id, key, value, created_at FROM %s;" data-table))
           (inserted-kvd (pkm2--db-insert-kvd key value timestamp type))
           (kvds (sqlite-select pkm2-database-connection sql-query))
           kvd)

      (expect (length kvds) :to-equal 1)
      (expect (nth 1 (car kvds)) :to-equal key)
      (expect (nth 2 (car kvds)) :to-equal value)
      (expect (nth 3 (car kvds)) :to-equal timestamp) ; created_at
      (setq kvd (pkm2--db-get-or-insert-kvd key value type))
      (expect (pkm2-db-kvd-id inserted-kvd) :to-equal (pkm2-db-kvd-id kvd))
      (expect (pkm2-db-kvd-key kvd) :to-equal key)
      (expect (pkm2-db-kvd-value kvd) :to-equal value)
      (expect (pkm2-db-kvd-created_at kvd) :to-equal timestamp)
      (expect (length (plist-get events :main)) :to-be 1)))

  (it "Test inserting kvd link"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))
           (type 'TEXT)
           (inserted-kvd (pkm2--db-insert-kvd "kvd-key" "kvd-value" (pkm2-get-current-timestamp) type))
           (inserted-node (pkm2--db-insert-node "node-content" (pkm2-get-current-timestamp)))
           (inserted-link (pkm2--db-insert-link-between-node-and-kvd
                           (pkm2-db-node-id inserted-node)
                           (pkm2-db-kvd-id inserted-kvd)
                           (pkm2-get-current-timestamp)
                           type))
           (pkm-node (pkm2--db-query-get-node-with-id 1))
           (kvds (pkm2-node-kvds pkm-node)))
      (expect (length kvds) :to-equal 1)
      (expect (pkm2-db-kvd-id inserted-kvd) :to-equal (pkm2-db-kvd-id (car kvds)))
      (expect (pkm2-db-kvd-link-id2 inserted-link) :to-equal (pkm2-db-kvd-link-id (car kvds)))
      (expect (length (plist-get events :main)) :to-be 3)
      (expect (-map (lambda (event) (plist-get event :action)) (plist-get events :main))
              :to-equal '(insert insert insert))
      (expect (-map (lambda (event) (plist-get event :what)) (plist-get events :main))
              :to-equal '(kvd-link node kvd))))

  (it "Test inserting node link"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))
           (inserted-node (pkm2--db-insert-node "node-content1" (pkm2-get-current-timestamp)))
           (inserted-node2 (pkm2--db-insert-node "node-content2" (pkm2-get-current-timestamp)))
           (inserted-link (pkm2--db-insert-link-between-nodeA-and-nodeB
                           "test-link-label"
                           (pkm2-db-node-id inserted-node)
                           (pkm2-db-node-id inserted-node2)
                           (pkm2-get-current-timestamp)))
           (pkm-node (pkm2--db-query-get-node-with-id 1))
           (pkm-node-2 (pkm2--db-query-get-node-with-id 2))
           (nodes-links-query "SELECT id, type, node_a, node_b, created_at FROM nodes_link;")
           (nodes-links (sqlite-select pkm2-database-connection nodes-links-query)))
      (expect (length nodes-links) :to-equal 1)
      (expect (pkm2-db-nodes-link-id inserted-link) :to-equal (caar nodes-links))
      (expect  (nth 2 (car nodes-links)) :to-equal (--> (pkm2-node-db-node pkm-node)
                                                        (pkm2-db-node-id it)))
      (expect  (nth 3 (car nodes-links)) :to-equal (--> (pkm2-node-db-node pkm-node-2)
                                                        (pkm2-db-node-id it)))



      (expect (-map (lambda (event) (plist-get event :action)) (plist-get events :main))
              :to-equal '(insert insert insert))
      (expect (-map (lambda (event) (plist-get event :what)) (plist-get events :main))
              :to-equal '(nodes-link node node))))

  (it "Creating sync event for insert, update, and deleting node"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))
           (content "Hello, This is my first node.")
           (timestamp (pkm2-get-current-timestamp))
           (timestamp2 (pkm2-get-current-timestamp))
           (sql-query "SELECT id, content, created_at, modified_at FROM node;")
           (inserted-node (pkm2--db-insert-node content timestamp))
           database-nodes
           node)
      (setq database-nodes (sqlite-select pkm2-database-connection sql-query))
      (expect (length database-nodes) :to-equal 1)
      (expect (nth 1 (car database-nodes)) :to-equal content)
      (expect (nth 2 (car database-nodes)) :to-equal timestamp) ; created_at
      (expect (nth 3 (car database-nodes)) :to-equal nil) ; modified_at should be nil
      (setq node (pkm2--db-query-get-node-with-id 1))
      (expect (--> (pkm2-node-db-node node) (pkm2-db-node-content it)) :to-equal content)
      (expect (--> (pkm2-node-db-node node) (pkm2-db-node-created_at it)) :to-equal timestamp)
      (expect (length (plist-get events :main)) :to-be 1)
      (pkm2--db-update-node
       (pkm2-db-node-id inserted-node)
       "Updated content"
       timestamp2)
      (expect (length (plist-get events :main)) :to-be 2)
      (pkm2--db-delete-node (pkm2-db-node-id inserted-node))
      (expect (length (plist-get events :main)) :to-be 3)
      (expect (-map (lambda (event) (plist-get event :action)) (plist-get events :main)) :to-equal '(delete update insert))
      (setq database-nodes (sqlite-select pkm2-database-connection sql-query))
      (expect (length database-nodes) :to-equal 0)))

  (it "Test deleting kvd-link"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))
           (type 'TEXT)
           (inserted-kvd (pkm2--db-insert-kvd "kvd-key" "kvd-value" (pkm2-get-current-timestamp) type))
           (inserted-node (pkm2--db-insert-node "node-content" (pkm2-get-current-timestamp)))
           (inserted-link (pkm2--db-insert-link-between-node-and-kvd
                           (pkm2-db-node-id inserted-node)
                           (pkm2-db-kvd-id inserted-kvd)
                           (pkm2-get-current-timestamp)
                           type))
           (pkm-node (pkm2--db-query-get-node-with-id 1))
           (kvds (pkm2-node-kvds pkm-node)))
      (expect (length kvds) :to-equal 1)
      (expect (pkm2-db-kvd-id inserted-kvd) :to-equal (pkm2-db-kvd-id (car kvds)))
      (expect (pkm2-db-kvd-link-id2 inserted-link) :to-equal (pkm2-db-kvd-link-id (car kvds)))
      (pkm2--db-delete-link-between-node-and-kvd (pkm2-db-kvd-link-id2 inserted-link)
                                                 type)
      (expect (length (plist-get events :main)) :to-be 4)
      (expect (-map (lambda (event) (plist-get event :action)) (plist-get events :main))
              :to-equal '(delete insert insert insert))
      (expect (-map (lambda (event) (plist-get event :what)) (plist-get events :main))
              :to-equal '(kvd-link kvd-link node kvd))))
  (it "Test deleting nodes-link"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))
           (inserted-node (pkm2--db-insert-node "node-content1" (pkm2-get-current-timestamp)))
           (inserted-node2 (pkm2--db-insert-node "node-content2" (pkm2-get-current-timestamp)))
           (inserted-link (pkm2--db-insert-link-between-nodeA-and-nodeB
                           "test-link-label"
                           (pkm2-db-node-id inserted-node)
                           (pkm2-db-node-id inserted-node2)
                           (pkm2-get-current-timestamp)))
           (pkm-node (pkm2--db-query-get-node-with-id 1))
           (pkm-node-2 (pkm2--db-query-get-node-with-id 2))
           (nodes-links-query "SELECT id, type, node_a, node_b, created_at FROM nodes_link;")
           (nodes-links (sqlite-select pkm2-database-connection nodes-links-query)))
      (expect (length nodes-links) :to-equal 1)
      (expect (pkm2-db-nodes-link-id inserted-link) :to-equal (caar nodes-links))
      (expect  (nth 2 (car nodes-links)) :to-equal (--> (pkm2-node-db-node pkm-node)
                                                        (pkm2-db-node-id it)))
      (expect  (nth 3 (car nodes-links)) :to-equal (--> (pkm2-node-db-node pkm-node-2)
                                                        (pkm2-db-node-id it)))


      (pkm2--db-delete-link-between-nodes (pkm2-db-nodes-link-id inserted-link))

      (expect (-map (lambda (event) (plist-get event :action)) (plist-get events :main))
              :to-equal '(delete insert insert insert))
      (expect (-map (lambda (event) (plist-get event :what)) (plist-get events :main))
              :to-equal '(nodes-link nodes-link node node))))
  (it "Test deleting kvd fails")
  (it "Test archiving kvd-link"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))
           (type 'TEXT)
           (inserted-kvd (pkm2--db-insert-kvd "kvd-key" "kvd-value" (pkm2-get-current-timestamp) type))
           (inserted-node (pkm2--db-insert-node "node-content" (pkm2-get-current-timestamp)))
           (inserted-link (pkm2--db-insert-link-between-node-and-kvd
                           (pkm2-db-node-id inserted-node)
                           (pkm2-db-kvd-id inserted-kvd)
                           (pkm2-get-current-timestamp)
                           type))
           (pkm-node (pkm2--db-query-get-node-with-id 1))
           (kvds (pkm2-node-kvds pkm-node)))
      (expect (length kvds) :to-equal 1)
      (expect (pkm2-db-kvd-id inserted-kvd) :to-equal (pkm2-db-kvd-id (car kvds)))
      (expect (pkm2-db-kvd-link-id2 inserted-link) :to-equal (pkm2-db-kvd-link-id (car kvds)))
      (pkm2-db-archive-link-between-node-and-kvd (pkm2-db-kvd-link-id2 inserted-link)
                                                 type
                                                 (pkm2-get-current-timestamp))
      (expect (length (plist-get events :main)) :to-be 4)
      (expect (-map (lambda (event) (plist-get event :action)) (plist-get events :main))
              :to-equal '(archive insert insert insert))
      (expect (-map (lambda (event) (plist-get event :what)) (plist-get events :main))
              :to-equal '(kvd-link kvd-link node kvd))))
  (it "Test archiving nodes-link fails")
  (it "Test archiving kvd fails")
  (it "Test archiving nodes fails")
  (it "Test updating kvd fails")


  (it "Test applying insert node event"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))
           (content "Hello, This is my remote node.")
           (timestamp (pkm2-get-current-timestamp))
           (shadow-id "21d6b80d-a2f7-4899-9c0e-a4f3d8c02d78")
           (event `(:action insert
                    :what node
                    :data
                    (:content ,content :new-node-id 1 :shadow-id ,shadow-id :timestamp ,timestamp)))
           (sql-query "SELECT id, content, created_at, modified_at FROM node;"))
      (pkm-sync--apply-remote-events (list event) nil)
      (setq database-nodes (sqlite-select pkm2-database-connection sql-query))
      (expect (length database-nodes) :to-equal 1)
      (expect (nth 1 (car database-nodes)) :to-equal content)
      (expect (nth 2 (car database-nodes)) :to-equal timestamp) ; created_at
      (expect (nth 3 (car database-nodes)) :to-equal nil) ; modified_at should be nil
      (setq node (pkm2--db-query-get-node-with-id 1))
      (expect (--> (pkm2-node-db-node node) (pkm2-db-node-content it)) :to-equal content)
      (expect (--> (pkm2-node-db-node node) (pkm2-db-node-created_at it)) :to-equal timestamp)
      (expect (length (plist-get events :main)) :to-be 1))
    )
  (it "Test applying insert kvd event"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))
           (key "test-key")
           (value "test-value")
           (type 'TEXT)
           (timestamp (pkm2-get-current-timestamp))
           (shadow-id "21d6b80d-a2f7-4899-9c0e-a4f3d8c02d78")
           (event `(:action insert
                    :what kvd
                    :data
                    (:key ,key :value ,value :new-kvd-id 1 :shadow-id ,shadow-id :timestamp ,timestamp :type ,type)))
           (sql-query (format "SELECT id, key, value, created_at FROM %s;" (pkm2--db-get-kvd-data-table-for-type type)))
           kvds
           )
      (pkm-sync--apply-remote-events (list event) nil)
      (setq kvds (sqlite-select pkm2-database-connection sql-query))
      (expect (length kvds) :to-equal 1)
      (expect (nth 1 (car kvds)) :to-equal key)
      (expect (nth 2 (car kvds)) :to-equal value)
      (expect (nth 3 (car kvds)) :to-equal timestamp) ; created_at
      (setq kvd (pkm2--db-get-or-insert-kvd key value type))
      (expect (pkm2-db-kvd-key kvd) :to-equal key)
      (expect (pkm2-db-kvd-value kvd) :to-equal value)
      (expect (pkm2-db-kvd-created_at kvd) :to-equal timestamp)
      (expect (length (plist-get events :main)) :to-be 1)))

  (it "Test applying insert kvd-link event"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))
           (kvd-event
            `(:action insert :what kvd :data (:key "kvd-key" :value "kvd-value" :new-kvd-id 1 :shadow-id "2b0e948a-e0d3-4fd9-b60e-876bbe8576e7" :timestamp 1705964736 :type TEXT)))
           (node-event
            `(:action insert :what node :data (:content "node-content" :new-node-id 1 :shadow-id "15415fe5-2530-47f4-81bc-f9c55c30af5f" :timestamp 1705964736)))
           (kvd-link-event `(:action insert
                             :what kvd-link
                             :data
                             (:node-id 1 :new-link-id 1 :shadow-id "da8579e1-dafc-4853-91d3-0dc23f7483a9" :kvd-id 1 :timestamp 1705964736 :type TEXT :context-node-id nil :is-archive nil)))

           (pkm-node (progn
                       (pkm-sync--apply-remote-events (list node-event kvd-event kvd-link-event) nil)
                       (pkm2--db-query-get-node-with-id 1) ))
           (kvds (pkm2-node-kvds pkm-node)))
      (expect (length kvds) :to-equal 1)
      (expect (length (plist-get events :main)) :to-be 3)
      (expect (-map (lambda (event) (plist-get event :action)) (plist-get events :main))
              :to-equal '(insert insert insert))
      (expect (-map (lambda (event) (plist-get event :what)) (plist-get events :main))
              :to-equal '(kvd-link kvd node))))
  (it "Test applying insert nodes-link event"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))

           (insert-node1-event `(:action insert :what node
                                 :data
                                 (:content "node-content1" :new-node-id 1 :shadow-id "927b1829-0438-45b6-a8d8-8498c2bd42cf" :timestamp 1705965211)))
           (insert-node2-event `(:action insert :what node
                                 :data
                                 (:content "node-content2" :new-node-id 2 :shadow-id "b87b5b3e-0357-4c76-92f1-5f63bf76debb" :timestamp 1705965211)))
           (insert-nodes-link-event `(:action insert :what nodes-link
                                      :data
                                      (:type "test-link-label" :new-link-id 1 :shadow-id "1a5c2579-8087-4378-b735-6f614339beee" :node-a-id 1 :node-b-id 2 :timestamp 1705965211 :context-node-id nil)))
           (pkm-node (progn
                       (pkm-sync--apply-remote-events (list insert-node1-event insert-node2-event insert-nodes-link-event) nil)
                       (pkm2--db-query-get-node-with-id 1)))
           (pkm-node-2 (pkm2--db-query-get-node-with-id 2))
           (nodes-links-query "SELECT id, type, node_a, node_b, created_at FROM nodes_link;")
           (nodes-links (sqlite-select pkm2-database-connection nodes-links-query)))
      (expect (length nodes-links) :to-equal 1)
      (expect  (nth 2 (car nodes-links)) :to-equal (--> (pkm2-node-db-node pkm-node)
                                                        (pkm2-db-node-id it)))
      (expect  (nth 3 (car nodes-links)) :to-equal (--> (pkm2-node-db-node pkm-node-2)
                                                        (pkm2-db-node-id it)))
      (expect (-map (lambda (event) (plist-get event :action)) (plist-get events :main))
              :to-equal '(insert insert insert))
      (expect (-map (lambda (event) (plist-get event :what)) (plist-get events :main))
              :to-equal '(nodes-link node node))))
  (it "Test applying insert, update, delete node events"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))
           (initial-content "Hello, This is my first node.")
           (updated-content "Updated content")
           (insert-node-event `(:action insert :what node :data (:content ,initial-content :new-node-id 1 :shadow-id "798629fc-0a9f-4a33-8801-250c5a306f6f" :timestamp 1705965546)))
           (update-node-event `(:action update :what node :data (:node-id 1 :new-content ,updated-content :timestamp 1705965546)))
           (delete-node-event `(:action delete :what node :data (:id 1 :shadow-id "798629fc-0a9f-4a33-8801-250c5a306f6f")))
           (sql-query "SELECT id, content, created_at, modified_at FROM node;")
           database-nodes
           node)
      (pkm-sync--apply-remote-events (list insert-node-event  ) nil)
      (setq database-nodes (sqlite-select pkm2-database-connection sql-query))
      (expect (length database-nodes) :to-equal 1)
      (expect (nth 1 (car database-nodes)) :to-equal initial-content)
      (expect (nth 3 (car database-nodes)) :to-equal nil) ; modified_at should be nil
      (expect (length (plist-get events :main)) :to-be 1)


      (pkm-sync--apply-remote-events (list update-node-event  ) nil)
      (setq database-nodes (sqlite-select pkm2-database-connection sql-query))
      (expect (length database-nodes) :to-equal 1)
      (expect (nth 1 (car database-nodes)) :to-equal updated-content)

      (expect (length (plist-get events :main)) :to-be 2)
      (pkm-sync--apply-remote-events (list delete-node-event) nil)

      (setq database-nodes (sqlite-select pkm2-database-connection sql-query))
      (expect (length database-nodes) :to-equal 0)
      (expect (length (plist-get events :main)) :to-be 3)
      (expect (-map (lambda (event) (plist-get event :action)) (plist-get events :main)) :to-equal '(delete update insert))))

  (it "Test applying delete kvd event fails")
  (it "Test applying delete kvd-link  event"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))
           (kvd-event
            `(:action insert :what kvd :data (:key "kvd-key" :value "kvd-value" :new-kvd-id 1 :shadow-id "2b0e948a-e0d3-4fd9-b60e-876bbe8576e7" :timestamp 1705964736 :type TEXT)))
           (node-event
            `(:action insert :what node :data (:content "node-content" :new-node-id 1 :shadow-id "15415fe5-2530-47f4-81bc-f9c55c30af5f" :timestamp 1705964736)))
           (kvd-link-event `(:action insert
                             :what kvd-link
                             :data
                             (:node-id 1 :new-link-id 1 :shadow-id "da8579e1-dafc-4853-91d3-0dc23f7483a9" :kvd-id 1 :timestamp 1705964736 :type TEXT :context-node-id nil :is-archive nil)))
           (kvd-link-delete-event `(:action delete :what kvd-link :data (:id 1 :shadow-id "da8579e1-dafc-4853-91d3-0dc23f7483a9")))
           (pkm-node (progn
                       (pkm-sync--apply-remote-events (list node-event kvd-event kvd-link-event) nil)
                       (pkm2--db-query-get-node-with-id 1) ))
           (kvds (pkm2-node-kvds pkm-node)))
      (expect (length kvds) :to-equal 1)
      (expect (length (plist-get events :main)) :to-be 3)

      (pkm-sync--apply-remote-events (list kvd-link-delete-event) nil)
      (setq pkm-node (pkm2--db-query-get-node-with-id 1))
      (setq kvds (pkm2-node-kvds pkm-node))
      (expect (length kvds) :to-equal 0)

      (expect (length (plist-get events :main)) :to-be 4)
      (expect (-map (lambda (event) (plist-get event :action)) (plist-get events :main))
              :to-equal '(delete insert insert insert))
      (expect (-map (lambda (event) (plist-get event :what)) (plist-get events :main))
              :to-equal '(kvd-link kvd-link kvd node))))

  (it "Test applying archiving kvd-link  event"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))
           (kvd-event
            `(:action insert :what kvd :data (:key "kvd-key" :value "kvd-value" :new-kvd-id 1 :shadow-id "2b0e948a-e0d3-4fd9-b60e-876bbe8576e7" :timestamp 1705964736 :type TEXT)))
           (node-event
            `(:action insert :what node :data (:content "node-content" :new-node-id 1 :shadow-id "15415fe5-2530-47f4-81bc-f9c55c30af5f" :timestamp 1705964736)))
           (kvd-link-event `(:action insert
                             :what kvd-link
                             :data
                             (:node-id 1 :new-link-id 1 :shadow-id "da8579e1-dafc-4853-91d3-0dc23f7483a9" :kvd-id 1 :timestamp 1705964736 :type TEXT :context-node-id nil :is-archive nil)))
           (kvd-link-archive-event `(:action archive :what kvd-link :data (:id 1 :shadow-id "da8579e1-dafc-4853-91d3-0dc23f7483a9")))
           (pkm-node (progn
                       (pkm-sync--apply-remote-events (list node-event kvd-event kvd-link-event) nil)
                       (pkm2--db-query-get-node-with-id 1) ))
           (kvds (pkm2-node-kvds pkm-node)))
      (expect (length kvds) :to-equal 1)
      (expect (length (plist-get events :main)) :to-be 3)

      (pkm-sync--apply-remote-events (list kvd-link-archive-event) nil)
      (setq pkm-node (pkm2--db-query-get-node-with-id 1))
      (setq kvds (pkm2-node-kvds pkm-node))
      (expect (length kvds) :to-equal 0)

      (expect (length (plist-get events :main)) :to-be 4)
      (expect (-map (lambda (event) (plist-get event :action)) (plist-get events :main))
              :to-equal '(archive insert insert insert))
      (expect (-map (lambda (event) (plist-get event :what)) (plist-get events :main))
              :to-equal '(kvd-link kvd-link kvd node)))

    )
  (it "Test applying delete nodes-link  event"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))

           (insert-node1-event `(:action insert :what node
                                 :data
                                 (:content "node-content1" :new-node-id 1 :shadow-id "927b1829-0438-45b6-a8d8-8498c2bd42cf" :timestamp 1705965211)))
           (insert-node2-event `(:action insert :what node
                                 :data
                                 (:content "node-content2" :new-node-id 2 :shadow-id "b87b5b3e-0357-4c76-92f1-5f63bf76debb" :timestamp 1705965211)))
           (insert-nodes-link-event `(:action insert :what nodes-link
                                      :data
                                      (:type "test-link-label" :new-link-id 1 :shadow-id "1a5c2579-8087-4378-b735-6f614339beee" :node-a-id 1 :node-b-id 2 :timestamp 1705965211 :context-node-id nil)))
           (delete-nodes-link-event `(:action delete :what nodes-link :data (:id 1 :shadow-id "b1604685-3bf8-4ddb-b40e-991e46a17438")) )
           (pkm-node (progn
                       (pkm-sync--apply-remote-events (list insert-node1-event insert-node2-event insert-nodes-link-event) nil)
                       (pkm2--db-query-get-node-with-id 1)))
           (pkm-node-2 (pkm2--db-query-get-node-with-id 2))
           (nodes-links-query "SELECT id, type, node_a, node_b, created_at FROM nodes_link;")
           (nodes-links (sqlite-select pkm2-database-connection nodes-links-query)))
      (expect (length nodes-links) :to-equal 1)
      (expect  (nth 2 (car nodes-links)) :to-equal (--> (pkm2-node-db-node pkm-node)
                                                        (pkm2-db-node-id it)))
      (expect  (nth 3 (car nodes-links)) :to-equal (--> (pkm2-node-db-node pkm-node-2)
                                                        (pkm2-db-node-id it)))
      (pkm-sync--apply-remote-events (list delete-nodes-link-event) nil)
      (setq nodes-links (sqlite-select pkm2-database-connection nodes-links-query))
      (expect (length nodes-links) :to-equal 0)
      (expect (-map (lambda (event) (plist-get event :action)) (plist-get events :main))
              :to-equal '(delete insert insert insert))
      (expect (-map (lambda (event) (plist-get event :what)) (plist-get events :main))
              :to-equal '(nodes-link nodes-link node node))))
  (it "Sync main with 1 remote event"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))
           (content "Hello, This is my remote node.")
           (timestamp (pkm2-get-current-timestamp))
           (shadow-id "21d6b80d-a2f7-4899-9c0e-a4f3d8c02d78")
           (saved-connection pkm2-database-connection)
           (pkm-sync--get-remote-events-func (lambda ()
                                               `(
                                                 ("mobile" . ((:action insert :what node :data (:content ,content :new-node-id 1 :shadow-id ,shadow-id :timestamp ,timestamp)) ) )
                                                 ;; (:action update :what node :data (:node-id 1 :new-content Updated content :timestamp 1705866591))
                                                 )))
           (pkm-sync-log-events-applied-func (lambda (events)
                                               (setq logged-events events)))
           (pkm2-database-file-path database-file)
           (sql-query "SELECT id, content, created_at, modified_at FROM node;")
           database-nodes
           node)
      (setq pkm2-database-connection (pkm-sync-on-main))
      (expect saved-connection :not :to-equal pkm2-database-connection)
      (setq database-nodes (sqlite-select pkm2-database-connection sql-query))
      (message "logged-events %s" logged-events)
      (expect (length logged-events) :to-be 1)
      (expect (length (car logged-events )) :to-be 2)
      (expect (length database-nodes) :to-equal 1)
      (expect (nth 1 (car database-nodes)) :to-equal content)
      (expect (nth 2 (car database-nodes)) :to-equal timestamp) ; created_at
      (expect (nth 3 (car database-nodes)) :to-equal nil) ; modified_at should be nil
      (setq node (pkm2--db-query-get-node-with-id 1))
      (expect (--> (pkm2-node-db-node node) (pkm2-db-node-content it)) :to-equal content)
      (expect (--> (pkm2-node-db-node node) (pkm2-db-node-created_at it)) :to-equal timestamp)
      (expect (length (plist-get events :main)) :to-be 1)))
  (it "Sync remote with 1 main event"
    (let* ((pkm-sync-add-event-func (lambda (event)
                                      (setq events (plist-put events :main (cons event (plist-get events :main))))))
           (content "Hello, This is my main node.")
           (saved-connection pkm2-database-connection)
           (timestamp (pkm2-get-current-timestamp))
           (shadow-id "21d6b80d-a2f7-4899-9c0e-a4f3d8c02d78")
           (pkm-sync--get-main-events-func (lambda ()
                                             `((:action insert :what node :data (:content ,content :new-node-id 1 :shadow-id ,shadow-id :timestamp ,timestamp))
                                               ;; (:action update :what node :data (:node-id 1 :new-content Updated content :timestamp 1705866591))
                                               )))
           (pkm-sync-log-events-applied-func (lambda (events)
                                               (setq logged-events events)))
           (pkm2-database-file-path database-file)
           (sql-query "SELECT id, content, created_at, modified_at FROM node;")
           database-nodes
           node)
      (copy-file pkm2-database-file-path (concat pkm2-database-file-path ".bak") t)
      ;; TODO its unclear if this is actually working as expected.
      ;; I can't tell if the database files are actually be coopied and moved around correctly.
      (setq pkm2-database-connection (pkm-sync-on-remote))
      (expect saved-connection :not :to-equal pkm2-database-connection)
      (setq database-nodes (sqlite-select pkm2-database-connection sql-query))
      (message "logged-events %s" logged-events)
      (expect (length logged-events) :to-be 1)
      (expect (caar logged-events) :to-equal "main")
      (expect (length database-nodes) :to-equal 1)
      (expect (nth 1 (car database-nodes)) :to-equal content)
      (expect (nth 2 (car database-nodes)) :to-equal timestamp) ; created_at
      (expect (nth 3 (car database-nodes)) :to-equal nil) ; modified_at should be nil
      (setq node (pkm2--db-query-get-node-with-id 1))
      (expect (--> (pkm2-node-db-node node) (pkm2-db-node-content it)) :to-equal content)
      (expect (--> (pkm2-node-db-node node) (pkm2-db-node-created_at it)) :to-equal timestamp)
      (expect (length (plist-get events :main)) :to-be 0))))



(provide 'test-sync)
;;; test-sync.el ends here
