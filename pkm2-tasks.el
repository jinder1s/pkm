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


(pkm2-register-behavior `(:name task
                          :assets ((:pkm-type kvd :name "is-todo" :key "node-type" :value "todo"  :data-type TEXT)
                                   (:pkm-type kvd :name "task-status" :key "task-status" :choices ,pkm-todo-status-types
                                    :prompt "What is todo's status?"  :data-type TEXT :log-change t)
                                   (:pkm-type kvd :name "task-priority" :key "task-priority" :choices ,pkm-todo-priority-types
                                    :prompt "What is todo's priority?"  :data-type INTEGER :log-change t))))


(pkm2-register-behavior `(:name schedule :assets ((:pkm-type kvd :name "is-schedule" :key "node-type" :value "schedule" :link-to ("base-node") :data-type TEXT)
                                                  (:pkm-type kvd :name "schedule-start" :key "schedule-start" :value ,#'pkm2-get-user-selected-timestamp
                                                    :link-to ("base-node") :data-type DATETIME )
                                                  (:pkm-type kvd :name "schedule-end" :key "schedule-end" :value ,#'pkm2-get-user-selected-timestamp
                                                    :link-to ("base-node") :data-type DATETIME :optional t))))
(pkm2-register-behavior `(:name deadline :assets ((:pkm-type kvd :name "is-schedule" :key "node-type" :value "schedule" :link-to ("base-node") :data-type TEXT)
                                                  (:pkm-type kvd :name "schedule-start" :key "schedule-start" :value ,#'pkm2-get-user-selected-timestamp
                                                    :link-to ("base-node") :data-type DATETIME )
                                                  (:pkm-type kvd :name "schedule-end" :key "schedule-end" :value ,#'pkm2-get-user-selected-timestamp
                                                    :link-to ("base-node") :data-type DATETIME :optional t))))
(pkm-register-structure 'task-n
                        (list :parent 'base-n
                              :log-primary t
                              :browse-insert-format-string (format "%s %s %s %s"
                                                                   "<insert>(:display kvd-value :key \"task-status\" :face-value pkm-todo-status-face-plist)</insert>"
                                                                   "<insert>(:display kvd-value :key \"task-priority\")</insert>"
                                                                   "<insert>(:display content)</insert>"
                                                                   "<insert>(:display hidden :prefix \"\\n\")</insert>")
                              :is-behavior 'task
                              :behaviors (list `(:name task :link-to (primary)))))


(pkm-register-structure 'project-s
                        (list :parent 'task-n
                              :assets (list
                                       '(:pkm-type kvd :name "is-project" :key "node-type" :value "project" :link-to ("base-node") :data-type TEXT))
                              ))

(provide 'pkm2-tasks)
;;; pkm2-tasks.el ends here
