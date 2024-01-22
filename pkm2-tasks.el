;;; pkm2-tasks.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jinder
;;
;; Author: Jinder <jinder1s@gmail.com>
;; Maintainer: Jinder <jinder1s@gmail.com>
;; Created: December 08, 2023
;; Modified: December 08, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/msingh15/pkm2-tasks
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'pkm-new-core)
(require 'pkm-hammerspoon)

(defvar pkm2-task-status-types (list "TODO" "DOING" "DONE" "HOLD" "KILL"))
(defvar pkm2-tasks-status-face-plist '(TODO org-todo DOING org-todo DONE org-done KILL +org-todo-cancel HOLD +org-todo-onhold ))
(defvar  pkm2-tasks-priority-types (list '("1" . 1) '("2" . 2) '("3" . 3) '("4" . 4) '("5" . 5)))

(pkm2-register-behavior `(:name task
                          :assets ((:pkm-type kvd :name "is-todo" :key "node-type" :value "todo"  :data-type TEXT)
                                   (:pkm-type kvd :name "task-status" :key "task-status" :choices ,pkm2-task-status-types
                                    :prompt "What is todo's status?"  :data-type TEXT :log-change t)
                                   (:pkm-type kvd :name "task-priority" :key "task-priority" :choices ,pkm2-tasks-priority-types
                                    :prompt "What is todo's priority?"  :data-type INTEGER :log-change t))))

(pkm2-register-behavior `(:name deadline :assets ((:pkm-type kvd :name "deadline" :key "deadline" :value ,#'pkm2-get-user-selected-timestamp
                                                   :link-to ("base-node") :data-type DATETIME )
                                                  (:pkm-type kvd :name "deadline-alert-minutes" :key "deadline-alert-minutes"
                                                   :link-to ("base-node") :data-type INTEGER :optional t )
                                                  )))

(pkm-register-structure 'schedule-node
                        (list :parent 'dependent-node
                              :browse-insert-format-string (concat
                                                            "[<insert>(:display kvd-value :key \"schedule-start\")</insert>]"
                                                            "--"
                                                            "[<insert>(:display kvd-value :key \"schedule-end\")</insert>]"
                                                            "<insert>(:display hidden :prefix \"\\n\")</insert>")
                              :managed-type t
                              :assets (list
                                       '(:pkm-type node :name "base-node" :primary-node t :no-input t)
                                       '(:pkm-type kvd :name "is-schedule" :key "node-type" :value "schedule" :link-to ("base-node") :data-type TEXT)
                                       `(:pkm-type kvd :name "schedule-start" :key "schedule-start" :value ,#'pkm2-get-user-selected-timestamp
                                         :link-to ("base-node") :data-type DATETIME )
                                       `(:pkm-type kvd :name "schedule-end" :key "schedule-end" :value ,#'pkm2-get-user-selected-timestamp
                                         :link-to ("base-node") :data-type DATETIME :optional t))))

(pkm-register-structure 'task-n
                        (list :parent 'base-n
                              :log-primary t
                              :browse-insert-format-string (format "%s %s %s %s"
                                                                   "<insert>(:display kvd-value :key \"task-status\" :face-value pkm2-tasks-status-face-plist)</insert>"
                                                                   "<insert>(:display kvd-value :key \"task-priority\")</insert>"
                                                                   "<insert>(:display content)</insert>"
                                                                   "<insert>(:display hidden :prefix \"\\n\")</insert>")
                              :is-behavior 'task
                              :behaviors (list `(:name task :link-to (primary)))))


(pkm-register-structure 'project-s
                        (list :parent 'task-n
                              :browse-insert-format-string (format "%s %s PROJ %s %s"
                                                                   "<insert>(:display kvd-value :key \"task-status\" :face-value pkm2-tasks-status-face-plist)</insert>"
                                                                   "<insert>(:display kvd-value :key \"task-priority\")</insert>"
                                                                   "<insert>(:display content)</insert>"
                                                                   "<insert>(:display hidden :prefix \"\\n\")</insert>")
                              :assets (list
                                       '(:pkm-type kvd :name "is-project" :key "node-type" :value "project" :link-to ("base-node") :data-type TEXT))
                              ))




(defun pkm2-tasks-schedule-node (&optional db-id)
  (interactive)
  (let* ((structure-name 'schedule-node)
         (parent-node-db-id (or db-id
                                (--> (funcall pkm2-get-pkm-node-at-point-func)
                                     (when it (pkm2-node-db-node it))
                                     (when it (pkm2-db-node-id it) ))
                                (pkm2-nodes-search "Search node to schedule into: ")))
         (link-label "instance")
         (link-definition (list :pkm-link-label link-label
                                :parent 'parent
                                :child 'child))
         (structure-schema (list :name 'parent-child-node
                                 :assets (list
                                          `(:pkm-type node :name "parent-node" :parent-node t :db-id ,parent-node-db-id)
                                          `(:pkm-type ,structure-name :name "child-node" :child-node t))
                                 :links (list link-definition))))

    (pkm2--object-capture-object-verify structure-schema "pkm-schedule" t)))

(defun pkm2-tasks-set-node-deadline (&optional db-id)
  (interactive)
  (let* ((target-node (or db-id (--> (funcall pkm2-get-pkm-node-at-point-func)
                                     (when it (pkm2-node-db-node it))
                                     (when it (pkm2-db-node-id it) ))
                          (pkm2-nodes-search "Search node to schedule into: ")))
         (link-label "instance")
         (structure-schema (list :name 'parent-child-node
                                 :assets (list
                                          `(:pkm-type node :name "target-node" :db-id ,target-node)
                                          `(:pkm-type kvd :name "deadline" :key "deadline" :value ,#'pkm2-get-user-selected-timestamp :link-to ("target-node") :data-type DATETIME )))))

    (pkm2--object-capture-object-verify structure-schema "pkm-schedule" t)))

(defun pkm2-tasks-alert ()
  (let* ((node-with-deadline
          (pkm2-get-nodes-with-pkm-query `((:or kvd (:key "deadline" :data-type INTEGER :after (hour -1) :before (hour 1)))
                                           (:and kvd (:key "task-status" :data-type TEXT :choices ("DOING" "TODO" "HOLD"))))))
         (deadlines (-map  (lambda (h-i-d)
                             (pkm2-node-get-kvd-value-with-key h-i-d "deadline"))
                           node-with-deadline))
         (alert-seconds (-map  (lambda (h-i-d)
                                 (--> (pkm2-node-get-kvd-value-with-key h-i-d "deadline-alert-minutes")
                                      (when it (* 60 it))))
                               node-with-deadline)))
    (-each-indexed node-with-deadline
      (lambda (index h-i-d)
        (when (nth index alert-seconds)
          (when  (< (- (nth index deadlines)
                       (nth index alert-seconds))
                    (pkm2-get-current-timestamp))
            (--> (pkm2-node-db-node h-i-d)
                 (pkm2-db-node-content it)
                 (pkm-hammerspoon-alert it))))))))

(provide 'pkm2-tasks)
;;; pkm2-tasks.el ends here
