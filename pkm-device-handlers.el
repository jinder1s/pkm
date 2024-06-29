;;; pkm-device-handlers.el Used to setup a device for sync -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jinder
;;
;; Author: Jinder <jinder1s@gmail.com>
;; Maintainer: Jinder <jinder1s@gmail.com>
;; Created: June 16, 2024
;; Modified: June 16, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/msingh15/pkm-device-handlers
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:
(defvar pkm-device-range-length 1000 "Range a device is given")

(defun pkm-device-get-min-available-for-table (database-connection table-name)
  (let* ((query (format "SELECT max(higher_limit) from device_sync WHERE table_name LIKE '%s';" table-name))
         (values (-flatten (sqlite-select database-connection query) ))
         (max_range (if values (car values) 0)))
    max_range))

(defun pkm-device-get-next-id (database-connection table-name)
  (let* ((query (format "SELECT id, next_id from device_sync WHERE table_name like '%s' ORDER BY next_id DESC LIMIT 1;" table-name))
         (values (sqlite-select database-connection query) ))
    (car values)
    ))

(defun pkm-device-set-next-id (database-connection current_id device_sync_row_id)
  ;; TODO Make sure next id will be smaller than limit
  (let* ((next_id (+ current_id 1))
         (table-sync-info (car (sqlite-select pkm2-database-connection (format "SELECT laptop_id, table_name, higher_limit from device_sync where id=%d" device_sync_row_id)) ))
         (higher-limit (nth 2 table-sync-info))
         (device-id (nth 0 table-sync-info))
         (table_name (nth 1 table-sync-info))
         (query-to-update-next_id (pkm2--db-compile-update-statement "device_sync"
                                                                     (list (format "next_id"))
                                                                     (list next_id)
                                                                     (format "id=%d" device_sync_row_id))))
    (if (< next_id higher-limit)
        (sqlite-execute database-connection query-to-update-next_id)
      (pkm-device-set-limits-for-table database-connection device-id table_name (pkm2-get-current-timestamp)))))

(defun setup-new-device (database-connection device-id)
  (let* ((ranges (-flatten-n 1 (-map (lambda (table-name)
                                       (--> (pkm-device-get-min-available-for-table database-connection table-name)
                                            (list (list (format "%s_lower_limit" table-name) it)
                                                  (list (format "%s_higher_limit" table-name) (+ it pkm-device-range-length))
                                                  (list (format "%s_next_id" table-name) it))))
                                     pkm-core-db-tables) ))
         (created-at (pkm2-get-current-timestamp))
         (columns (-concat ranges (list `("created_at" ,created-at)
                                        `("laptop_id" ,device-id))))
         (column-names (-map #'car columns))
         (column-values (-map #'cadr columns))
         (insert-statement (pkm2--db-compile-insert-statement "device_sync" column-names column-values)))
    (sqlite-execute database-connection insert-statement)))

(defun pkm-device-set-limits-for-table (database-connection device-id table-name created-at)
  (let* ((min-value (pkm-device-get-min-available-for-table database-connection table-name))
         (higher_limit (+ min-value pkm-device-range-length))
         (next-id min-value)
         (column-names (list "table_name" "laptop_id" "lower_limit" "higher_limit" "next_id" "created_at"))
         (column-values `(,table-name ,device-id ,min-value ,higher_limit ,next-id ,created-at))
         (insert-statement (pkm2--db-compile-insert-statement "device_sync" column-names column-values)))
    (sqlite-execute database-connection insert-statement)))
(defun setup-new-device2 (database-connection device-id)
  (--> (pkm2-get-current-timestamp)
       (-each pkm-core-db-tables (lambda (table-name)
                                   (pkm-device-set-limits-for-table database-connection device-id table-name it)))))

(provide 'pkm-device-handlers)
;;; pkm-device-handlers.el ends here
