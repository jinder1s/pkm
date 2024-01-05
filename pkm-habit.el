;;; pkm-habit.el Implementing methods to help start, keep track of, and continue doing habits -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jinder
;;
;; Author: Jinder <jinder1s@gmail.com>
;; Maintainer: Jinder <jinder1s@gmail.com>
;; Created: January 03, 2024
;; Modified: January 03, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/msingh15/pkm-habit
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'ts)
(require 'pkm-new-core)

(defvar pkm2-habit-hourly-paused nil)

(defvar pkm2-habit-freq-to-seconds `(("daily" . ,(* 60 60 24))
                                     ("hourly" . ,(* 60 60))
                                     ("weekly" . ,(* 60 60 24 7))
                                     ("monthly" . ,(* 60 60 24 28))
                                     ("yearly" . ,(* 60 60 24 365))))

(pkm-register-structure 'habit-n
                        (list :parent 'base-n
                              :assets (list
                                       '(:pkm-type kvd :name "is-habit" :key "node-type" :value "habit" :link-to (primary) :data-type TEXT)
                                       `(:pkm-type kvd :name "habit-start-timestamp" :key "habit-start-timestamp" :link-to (primary) :data-type DATETIME :optional t :prompt ""))))
(pkm-register-structure 'daily-habit-n
                        (list :parent 'habit-n
                              :assets (list
                                       '(:pkm-type kvd :name "is-habit" :key "node-type" :value "habit" :link-to (primary) :data-type TEXT)
                                       `(:pkm-type kvd :name "frequency" :key "habit-freq" :value "daily" :link-to (primary) :data-type TEXT)
                                       `(:pkm-type kvd :name "habit-interval" :key "habit-interval" :link-to (primary) :data-type INTEGER :optional t :prompt "")


                                       `(:pkm-type kvd :name "habit-schedule-hour" :key "habit-schedule-hour" :link-to (primary) :data-type REAL :optional t :prompt "")
                                       `(:pkm-type kvd :name "habit-duration-hours" :key "habit-duration-hours" :link-to (primary) :data-type REAL :optional t :prompt "")
                                       `(:pkm-type kvd :name "habit-deadline-hour" :key "habit-deadline-hour" :link-to (primary) :data-type REAL :optional t :prompt ""))))




(when nil (pkm-register-structure 'habit-alternative-node
                                  (list :parent 'dependent-node
                                        :browse-insert-format-string (concat "")
                                        :managed-type t
                                        :assets (list
                                                 '(:pkm-type node :name "base-node" :primary-node t :no-input t)
                                                 '(:pkm-type kvd :name "is-habit-alternative" :key "node-type" :value "habit-alternative" :link-to ("base-node") :data-type TEXT)))) )

(when nil (pkm-register-structure 'habit-choice-node
                        (list :parent 'dependent-node
                              :browse-insert-format-string (concat "")
                              :managed-type t
                              :assets (list
                                       '(:pkm-type node :name "base-node" :primary-node t :no-input t)
                                       '(:pkm-type kvd :name "is-habit-choice" :key "node-type" :value "habit-choice" :link-to ("base-node") :data-type TEXT)))) )

(pkm-register-structure 'habit-instance
                        (list :parent 'dependent-node
                              :behaviors (list '(:name task :link-to (primary)))
                              :asset-modifications (list '(:name "task-status" :plist-key :log-ask-timestamp :plist-value t))
                              :assets (list
                                       '(:pkm-type kvd :name "is-habit-instance" :key "node-type" :value "habit-instance" :link-to ("base-node") :data-type TEXT)
                                       '(:pkm-type node :name "base-node" :primary-node t :no-input t)
                                       '(:pkm-type kvd :name "schedule" :key "schedule"  :link-to ("base-node") :data-type DATETIME :optional t)
                                       `(:pkm-type kvd :name "deadline" :key "deadline"  :link-to ("base-node") :data-type DATETIME :optional t))))




(defun pkm-habit-plan-daily-instances (habit-pkm-node &optional num-instances from-unix-timestamp to-unix-timestamp)
  "Creates habit instaces for daily habits. "
  (when (and num-instances to-unix-timestamp) (error "Only specify num-instances or to-unix-timestamp"))
  (let* ((content (--> (pkm2-node-db-node habit-pkm-node) (pkm2-db-node-content it)))
         (habit-frequency (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-freq")
                               (when it
                                 (if (length> it 1)
                                     (error "habit node has more than one frequency")
                                   (pkm2-db-kvd-value (car it))))))
         (habit-period (assoc-default habit-frequency pkm2-habit-freq-to-seconds))
         (habit-interval (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-interval")
                              (when it
                                (if (length> it 1)
                                    (error "habit node has more than one interval")
                                  (pkm2-db-kvd-value (car it))))
                              (or it 1)))
         (period (* habit-period habit-interval))
         (habit-schedule-hour (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-schedule-hour")
                                   (when it
                                     (if (length> it 1)
                                         (error "habit node has more than one habit-schedule-hour")
                                       (pkm2-db-kvd-value (car it))))))
         (habit-duration-hours (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-duration-hours")
                                    (when it
                                      (if (length> it 1)
                                          (error "habit node has more than one habit-duration-hours")
                                        (pkm2-db-kvd-value (car it))))))
         (habit-deadline-hour (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-deadline-hour")
                                    (when it
                                      (if (length> it 1)
                                          (error "habit node has more than one habit-deadline-hour")
                                        (pkm2-db-kvd-value (car it))))))
         (from-unix-timestamp  (--> (or from-unix-timestamp
                                          (pkm2-get-current-timestamp))
                                      (make-ts :unix it)
                                      (ts-apply :hour 0 :minute 0 :second 0 it)
                                      (ts-unix it)))
         (num-instance (or num-instances (ceiling (/ (- to-unix-timestamp from-unix-timestamp) period)) 1))
         output)
    (dotimes (index num-instance output)
      (let* ((start-of-new-period (+ from-unix-timestamp (* period index)))
             (end-of-new-period (+ start-of-new-period period))
             (schedule-timestamp (if habit-schedule-hour
                                     (let* ((start (make-ts  :unix start-of-new-period))
                                            (schedule-hour (truncate habit-schedule-hour))
                                            (schedule-minute (truncate (* (mod habit-schedule-hour 1) 60 ) )))
                                       (--> (ts-apply :hour schedule-hour :minute schedule-minute start)
                                            (ts-unix it)
                                            (truncate it)))
                                   start-of-new-period))
             (deadline-timestamp (cond (habit-duration-hours (let* ((start (make-ts  :unix schedule-timestamp))
                                                                    (deadline-hour (truncate habit-duration-hours))
                                                                    (deadline-minute (truncate (* (mod habit-duration-hours 1) 60 ) )))
                                                               (--> (ts-adjust  'hour deadline-hour 'minute deadline-minute start)
                                                                    (ts-unix it)
                                                                    (truncate it))))
                                       (habit-deadline-hour (let* ((start (make-ts  :unix start-of-new-period))
                                                                   (deadline-hour (truncate habit-deadline-hour))
                                                                   (deadline-minute (truncate (* (mod habit-deadline-hour 1) 60 ) )))
                                                               (--> (ts-apply  :hour deadline-hour :minute deadline-minute start)
                                                                    (ts-unix it)
                                                                    (truncate it))))
                                       (t end-of-new-period)))
             (new-output `(("base-node" . ,content)
                           ("task-status" . "TODO")
                           ("task-priority" . 2)
                           ,(when schedule-timestamp `("schedule" . ,schedule-timestamp))
                           ,(when deadline-timestamp `("deadline" . ,deadline-timestamp)))))
        (setq output (append output (list new-output)))))
    output))







(provide 'pkm-habit)
;;; pkm-habit.el ends here
