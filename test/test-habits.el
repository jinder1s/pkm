;;; test-habits.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jinder
;;
;; Author: Jinder <jinder1s@gmail.com>
;; Maintainer: Jinder <jinder1s@gmail.com>
;; Created: January 03, 2024
;; Modified: January 03, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/msingh15/test-habits
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; (when (require 'undercover nil t)
;;   (undercover "pkm-new-core.el"
;;               (:report-file "/tmp/local-report.txt")
;;               (:report-format 'text)
;;               (:send-report nil)))

;; (require 'pkm-new-core)
;; (require 'pkm2-object-registrations)
;; (require 'pkm-habit)

;; (describe "Habit tests"
;;   :var (database-file)
;;   (before-each
;;     (setq database-file (make-temp-file "pkm-test" nil ".sqlite3"))
;;     (setq pkm2-database-connection (sqlite-open  database-file) )
;;     (setq debug-on-error t)
;;     (pkm2-setup-database pkm2-database-connection))
;;   (it "Create Morning brushing habit and create 1 future instance correctly"
;;     (pkm--object-cache-structure-info)
;;     (let* ((habit-content "Brush every morning")
;;            (habit-schedule-hour  6.5)
;;            (habit-duration-hours 5)
;;            (habit-start-timestamp  (--> (ts-apply :hour 0 :minute 0 :second 0 (ts-now))
;;                                         (ts-unix it)
;;                                         (truncate it))))
;;       (expect
;;        (pkm2--object-capture-object-verify
;;         'daily-habit-n
;;         nil
;;         t
;;         `(("base-node" . ,habit-content)
;;           ("frequency" . "daily")
;;           ("habit-schedule-hour" . ,habit-schedule-hour)
;;           ("habit-duration-hours" . ,habit-duration-hours)
;;           ("habit-start-timestamp" . ,habit-start-timestamp)))
;;        :not :to-throw)
;;       (expect (length (sqlite-select pkm2-database-connection "SELECT * FROM node;") ) :to-equal 1)
;;       (let* ((habit-pkm-node (pkm2--db-query-get-node-with-id 1))
;;              (habit-instances (pkm-habit-plan-daily-instances habit-pkm-node 1))
;;              (habit-instance (car habit-instances))
;;              (expected-schedule (truncate (+ habit-start-timestamp (* 60 60 habit-schedule-hour))))
;;              (expected-deadline (truncate (+  habit-start-timestamp
;;                                               (* 60 60 habit-schedule-hour)
;;                                               (* 60 60 habit-duration-hours)))))
;;         (expect  (length habit-instances )  :to-equal 1)
;;         (expect  (assoc-default "task-status" habit-instance )  :to-equal "TODO")
;;         (expect  (assoc-default "base-node" habit-instance )  :to-equal habit-content)
;;         (expect  (pkm2--convert-object-to-string (assoc-default "schedule" habit-instance ) 'DATETIME )
;;                  :to-equal
;;                  (pkm2--convert-object-to-string expected-schedule 'DATETIME ) )
;;         (expect  (pkm2--convert-object-to-string (assoc-default "deadline" habit-instance ) 'DATETIME )

;;                  :to-equal
;;                  (pkm2--convert-object-to-string
;;                   expected-deadline
;;                   'DATETIME)))))
;;   (it "Create Morning brushing habit and create 5 future instance correctly"
;;     (pkm--object-cache-structure-info)
;;     (let* ((habit-content "Brush every morning")
;;            (habit-schedule-hour  6.5)
;;            (habit-duration-hours 5)
;;            (habit-start-timestamp  (--> (ts-apply :hour 0 :minute 0 :second 0 (ts-now))
;;                                         (ts-unix it)
;;                                         (truncate it))))
;;       (expect
;;        (pkm2--object-capture-object-verify
;;         'daily-habit-n
;;         nil
;;         t
;;         `(("base-node" . ,habit-content)
;;           ("frequency" . "daily")
;;           ("habit-schedule-hour" . ,habit-schedule-hour)
;;           ("habit-duration-hours" . ,habit-duration-hours)
;;           ("habit-start-timestamp" . ,habit-start-timestamp)))
;;        :not :to-throw)
;;       (expect (length (sqlite-select pkm2-database-connection "SELECT * FROM node;") ) :to-equal 1)
;;       (let* ((habit-pkm-node (pkm2--db-query-get-node-with-id 1))
;;              (habit-instances (pkm-habit-plan-daily-instances habit-pkm-node 5))
;;              (habit-instance (car habit-instances))
;;              (expected-schedule (truncate (+ habit-start-timestamp (* 60 60 habit-schedule-hour))))
;;              (expected-deadline (truncate (+  habit-start-timestamp
;;                                               (* 60 60 habit-schedule-hour)
;;                                               (* 60 60 habit-duration-hours)))))
;;         (expect  (length habit-instances )  :to-equal 5)
;;         (expect  (assoc-default "task-status" habit-instance )  :to-equal "TODO")
;;         (expect  (assoc-default "base-node" habit-instance )  :to-equal habit-content)
;;         (expect  (pkm2--convert-object-to-string (assoc-default "schedule" habit-instance ) 'DATETIME )
;;                  :to-equal
;;                  (pkm2--convert-object-to-string expected-schedule 'DATETIME ) )
;;         (expect  (pkm2--convert-object-to-string (assoc-default "deadline" habit-instance ) 'DATETIME )

;;                  :to-equal
;;                  (pkm2--convert-object-to-string
;;                   expected-deadline
;;                   'DATETIME)))))

;;   (it "Create Morning brushing habit instances over next 5 days"
;;     (pkm--object-cache-structure-info)
;;     (let* ((habit-content "Brush every morning")
;;            (habit-schedule-hour  6.5)
;;            (habit-duration-hours 5)
;;            (habit-start-timestamp  (--> (ts-apply :hour 0 :minute 0 :second 0 (ts-now))
;;                                             (ts-unix it)
;;                                             (truncate it))))
;;       (expect
;;        (pkm2--object-capture-object-verify
;;         'daily-habit-n
;;         nil
;;         t
;;         `(("base-node" . ,habit-content)
;;           ("frequency" . "daily")
;;           ("habit-schedule-hour" . ,habit-schedule-hour)
;;           ("habit-duration-hours" . ,habit-duration-hours)
;;           ("habit-start-timestamp" . ,habit-start-timestamp)))
;;        :not :to-throw)
;;       (expect (length (sqlite-select pkm2-database-connection "SELECT * FROM node;") ) :to-equal 1)
;;       (let* ((habit-pkm-node (pkm2--db-query-get-node-with-id 1))
;;              (from-unix-timestamp (--> (ts-apply :hour 0 :minute 0  (ts-now))
;;                                        (ts-unix it)
;;                                        (truncate it)))
;;              (to-unix-timestamp (--> (ts-apply :hour 0 :minute 0  (ts-now))
;;                                      (ts-adjust  'day 5 it)
;;                                      (ts-unix it)
;;                                      (truncate it)))
;;              (habit-instances (pkm-habit-plan-daily-instances habit-pkm-node nil from-unix-timestamp to-unix-timestamp))

;;              (habit-instance (car habit-instances))
;;              (expected-schedule (truncate (+ habit-start-timestamp (* 60 60 habit-schedule-hour))))
;;              (expected-deadline (truncate (+  habit-start-timestamp
;;                                               (* 60 60 habit-schedule-hour)
;;                                               (* 60 60 habit-duration-hours)))))
;;         (expect  (length habit-instances )  :to-equal 5))))


;;   )
(provide 'test-habits)
;;; test-habits.el ends here
