;;; test-structure-identification.el --- Test to make sure my method for identifying structure type make sense -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jinder
;;
;; Author: Jinder <jinder1s@gmail.com>
;; Maintainer: Jinder <jinder1s@gmail.com>
;; Created: May 11, 2024
;; Modified: May 11, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/msingh15/test-structure-identification
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Test to make sure my method for identifying structure type make sense
;;
;;; Code:

(when (require 'undercover nil t)
  (undercover "pkm-new-core.el"
              (:report-file "/tmp/local-report.txt")
              (:report-format 'text)
              (:send-report nil)))
(require 'pkm-new-core)
(require 'pkm-compile-db-query)
(require 'org)

(describe "Eieio object identification tests"
  :var (database-file)
  (before-each
    (setq database-file (make-temp-file "pkm-test" nil ".sqlite3"))
    (setq pkm2-database-connection nil)

    (setq pkm2-database-connection (sqlite-open database-file))
    (setq pkm-structure-2-undefined-schemas-plist ())
    (setq pkm-data-type-to-kvd-key-plist-eieio ())
    (setq pkm-kvd-key-to-structure-plist-eieio ())
    (setq pkm-structure-2-defined-schemas-plist ())

    (pkm2-setup-database pkm2-database-connection))
  (after-each
    (setq pkm2-database-connection nil))


  (it "Identify objects with different schemas"
    (let* ((node-asset (node-schema :name "base-node" :primary-node t))
           (schema (object-schema
                    :name "base-n"
                    :assets (list node-asset)))
           (first-schema (object-schema :name "first-n" :parent-schema-name "base-n"
                                        :assets
                                        (list
                                         (kvd-schema  :name "is-first" :key "node-type" :value "first-n" :link-to '("base-node") :data-type 'TEXT))))
           (second-schema (object-schema :name "second-n" :parent-schema-name "base-n"
                                         :assets
                                         (list
                                          (kvd-schema  :name "is-second" :key "node-type" :value "second-n" :link-to '("base-node") :data-type 'TEXT))))
           (first-n-node-content "first-n-node value")
           (second-n-node-content "second-n-node value")
           (first-n-values `(("base-node" . ,first-n-node-content)))
           (second-n-values `(("base-node" . ,second-n-node-content)))
           (query-first-n nil)
           (query-second-n nil)
           (first-n-nodes nil)
           (second-n-nodes nil))
      (pkm-register-structure-2 schema)
      (pkm-register-structure-2 first-schema)
      (pkm-register-structure-2 second-schema)
      (pkm--object-cache-structure-info-eieio)
      ;; 3 schemas + 3 names =6
      (expect (length pkm-structure-2-undefined-schemas-plist) :to-equal 6)
      (expect (length pkm-structure-2-defined-schemas-plist) :to-equal 6)
      (expect (length pkm-kvd-key-to-structure-plist-eieio) :to-equal 2)
      (expect (length pkm-kvd-key-to-structure-plist-eieio) :to-equal 2)
      (expect (length (plist-get pkm-structure-required-kvds-plist-eieio "first-n" #'equal)) :to-equal 1)
      (expect (length (plist-get pkm-structure-required-kvds-plist-eieio "second-n" #'equal)) :to-equal 1)
      (expect (length (plist-get pkm-structure-fully-specified-kvds-plist-eieio "first-n" #'equal)) :to-equal 1)
      (expect (length (plist-get pkm-structure-fully-specified-kvds-plist-eieio "second-n" #'equal)) :to-equal 1)

      (setq query-first-n (pkm2--db-compile-query-get-nodes-of-structure-type '(:structure-name "first-n")))
      (setq query-second-n (pkm2--db-compile-query-get-nodes-of-structure-type '(:structure-name "second-n")))
      (expect  query-first-n :not :to-be nil)
      (expect  query-second-n :not :to-be nil)
      ;; Make sure there are no nils in queries
      (expect (cl-search "nil" query-first-n) :to-be nil)
      (expect (cl-search "nil" query-second-n) :to-be nil)
      (--> (schema-compile first-schema)
           (pkm-capture-test it first-n-values)
           (pkm-commit-test it))
      (--> (schema-compile second-schema)
           (pkm-capture-test it second-n-values)
           (pkm-commit-test it))
      (setq first-n-nodes (-flatten (sqlite-select pkm2-database-connection query-first-n) ))
      (setq second-n-nodes (-flatten (sqlite-select pkm2-database-connection query-second-n) ))
      (expect (length first-n-nodes) :to-equal 1)
      (expect (length second-n-nodes) :to-equal 1)
      (expect (car first-n-nodes) :to-equal 1)
      (expect (car second-n-nodes) :to-equal 2))))

(provide 'test-structure-identification)
;;; test-structure-identification.el ends here
