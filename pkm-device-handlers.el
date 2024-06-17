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
(defvar pkm-device-range-length 100000 "Range a device is given")

(defun pkm-device-get-min-available-for-table (database-connection table-name)
  (let* ((query (format "SELECT %s_higher_limit from device_sync;" table-name))
         (values (-flatten (sqlite-select database-connection query) ))
         (max_range (if values (apply #'max values) 0)))
    max_range))

(defun pkm-device-get-next-id (database-connection table-name)
  (let* ((query (format "SELECT %s_next_id from device_sync;" table-name))
         (values (-flatten (sqlite-select database-connection query) ))
         (max_range (if values (apply #'max values) 0)))
    max_range))

(defun pkm-device-set-next-id (database-connection device-id table-name current_id)
  (let* ((query (format "SELECT %s_next_id, %s_lower_limit, %s_higher_limit, created_at FROM device_sync WHERE created_at=(SELECT max(created_at) FROM device_sync WHERE laptop_id='%s');" table-name table-name table-name device-id))
         (values (car (sqlite-select database-connection query)))
         (previous_next_id (nth 0 values))
         (lower_limit (nth 1 values))
         (higher_limit (nth 2 values)))
    (cl-assert (>= current_id previous_next_id))
    (if (< (+ current_id 1) higher_limit)
        (sqlite-execute database-connection (pkm2--db-compile-update-statement "device_sync"
                                                                               (list (format "%s_next_id" table-name))
                                                                               (list (+ current_id 1))
                                                                               nil))
      (setup-new-device database-connection device-id))))

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

(provide 'pkm-device-handlers)
;;; pkm-device-handlers.el ends here
