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
;;; Code:
(require 'sqlite)
(require 'request)
(defvar pkm-device-range-length 1000 "Range a device is given")

(defun pkm-device-get-min-available-for-table (database-connection table-name)
  (let* ((query (format "SELECT max(higher_limit) from device_sync WHERE table_name LIKE '%s';" table-name))
         (values (-flatten (sqlite-select database-connection query)))
         (max_current_used (unless values (--> (format "SELECT id FROM %s ORDER BY id DESC LIMIT 1;" table-name ) (sqlite-select database-connection it) (caar it))))
         (max_range (if values (car values) (if max_current_used (+ max_current_used 1) 1))))
    max_range))

(defun pkm-device-get-next-id (database-connection table-name)
  (let* ((query (format "SELECT id, next_id from device_sync WHERE table_name like '%s' AND laptop_id LIKE '%s' ORDER BY next_id DESC LIMIT 1;" table-name pkm2-device-name))
         (values (sqlite-select database-connection query) ))
    (car values)))

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


(defun pkm-device-set-limits-for-table (database-connection device-id table-name created-at &optional min-value)
  (let* ((min-value (or min-value (pkm-device-get-min-available-for-table database-connection table-name) ))
         (higher_limit (+ min-value pkm-device-range-length))
         (next-id min-value)
         (column-names (list "table_name" "laptop_id" "lower_limit" "higher_limit" "next_id" "created_at"))
         (column-values `(,table-name ,device-id ,min-value ,higher_limit ,next-id ,created-at))
         (insert-statement (pkm2--db-compile-insert-statement "device_sync" column-names column-values)))
    (sqlite-execute database-connection insert-statement)))

(defun setup-new-device2 (database-connection device-id &optional table-settings)
  (--> (pkm2-get-current-timestamp)
       (-each pkm-core-db-tables
         (lambda (table-name)
           (pkm-device-set-limits-for-table database-connection device-id table-name it (plist-get table-settings table-name #'equal))))))

(defvar laptop-addresses
  '(("work-tufts-1" . "mcttsu14588")
    ("personal-air-1" . "manjinders-macbook-air")
    ("work-acs-1". "msingh-acs-1" )))

(defvar sync-devices '("work-tufts-1"))

(defun pkm-sync-server-call (post-data)
  (pcase (plist-get post-data :action)
    ('get-data (let* ((last-id (plist-get post-data :last-id)))
                 (pkm2--sync-get-local-events last-id)))
    ('wake-up (let* ((device (plist-get post-data :device-id)))
                `(:acknowledge t)))))

(defun pkm-sync-with-device (device-id)
  (let* ((last-sync-id (pkm-sync-get-last-sync-id-with-device  device-id)))
    (pkm-sync-request-new-events device-id last-sync-id)))

(defun pkm-sync-with-all-devices ()
  (-each laptop-addresses (lambda (device-cons)
                            (let* ((device-id (car device-cons)))
                              (when (not (equal device-id pkm2-device-name))
                                (pkm-sync-with-device device-id )))))
  )
(defun pkm-sync-request-new-events (device-id last-sync-id)
  (let* ((host-address (assoc-default device-id laptop-addresses))
         (host-port 8000)
         (url (format "%s:%d" host-address host-port)))
    (request url
      :type "POST"
      :data (format "(:action get-data :last-id %d)" last-sync-id)
      :headers '(("Content-Type" . "application/text"))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((read_data (read data))
                         (max_sync_id (-max (-map #'car read_data)))
                         (event_index 2)
                         (events  (-map (lambda (datum)
                                          (read (nth event_index datum))) read_data)))
                    (pkm-sync--apply-remote-events it t)
                    (pkm-sync-set-last-sync-id device-id max_sync_id)))))))

(defun pkm-sync-wake-up-notification (remote-device-id current-device-id)
  (let* ((host-address (assoc-default remote-device-id laptop-addresses))
         (host-port 8000)
         (url (format "%s:%d" host-address host-port)))
    (request url
      :type "POST"
      :data (format "(:action wake-up :device-id '%s')" current-device-id )
      :headers '(("Content-Type" . "application/text"))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (plist-get data :acknowledge))))))

(defun pkm-sync-get-last-sync-id-with-device (device-id)
  (let* ((query (format"SELECT last_sync_id FROM local_event_sync WHERE laptop_id LIKE '%s';" device-id))
         (query-output (sqlite-select pkm2-database-connection query)))
    (if query-output (car query-output)
      (progn (pkm-sync-set-sync-device device-id -1) -1))))

(defun pkm-sync-set-last-sync-id (device-id last-sync-id)
  (let* ((query-to-update-last-sync-id (pkm2--db-compile-update-statement "local_event_sync"
                                                                     (list (format "last_sync_id"))
                                                                     (list last-sync-id)
                                                                     (format "laptop_id LIKE '%s'" device-id))))
    (sqlite-execute pkm2-database-connection query-to-update-last-sync-id)))

(defun pkm-sync-set-sync-device (device-id last-sync-id)
  (let* ((query-to-update-last-sync-id (pkm2--db-compile-insert-statement "local_event_sync"
                                                                          (list "last_sync_id" "laptop_id")
                                                                          (list last-sync-id device-id))))
    (sqlite-execute pkm2-database-connection query-to-update-last-sync-id)))

(provide 'pkm-device-handlers)
;;; pkm-device-handlers.el ends here
