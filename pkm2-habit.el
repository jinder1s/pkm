;;; pkm/pkm2-habit.el -*- lexical-binding: t; -*-

(require 'ts)

(defvar pkm2-habit-freq-to-seconds `(("daily" . ,(* 60 60 24))
                                     ("hourly" . ,(* 60 60))
                                     ("weekly" . ,(* 60 60 24 7))
                                     ("monthly" . ,(* 60 60 24 28))
                                     ("yearly" . ,(* 60 60 24 365))))

(pkm-register-structure 'habit-node
                        (list :parent 'base-n
                              :assets (list
                                       '(:pkm-type kvd
                                         :name "is-habit"
                                         :key "node-type"
                                         :value "habit"
                                         :link-to ("base-node")
                                         :data-type TEXT)
                                       `(:pkm-type kvd
                                         :name "frequency"
                                         :key "habit-freq"
                                         :choices ,(-map #'car pkm2-habit-freq-to-seconds)
                                         :link-to ("base-node")
                                         :data-type TEXT)
                                       `(:pkm-type kvd
                                         :name "habit-interval"
                                         :key "habit-interval"
                                         :link-to ("base-node")
                                         :data-type INTEGER
                                         :optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-schedule-hour"
                                         :key "habit-schedule-hour"
                                         :link-to ("base-node")
                                         :data-type REAL
                                         :optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-deadline-hour"
                                         :key "habit-deadline-hour"
                                         :link-to ("base-node")
                                         :data-type REAL
                                         :optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-required"
                                         :key "habit-required"
                                         :link-to ("base-node")
                                         :data-type TEXT
                                         :optional t
                                         :prompt ""))))

(pkm-register-structure 'habit-instance
                        (list :parent 'dependent-node
                              :behaviors (list '(:name task :link-to (primary)))
                              :asset-modifications (list '(:name "task-status" :plist-key :log-ask-timestamp :plist-value t))
                              :assets (list
                                       '(:pkm-type kvd :name "is-habit-instance" :key "node-type" :value "habit-instance" :link-to ("base-node") :data-type TEXT)
                                       '(:pkm-type node :name "base-node" :primary-node t :no-input t))))


(defun pkm2-habit-create-instances (habit-pkm-node)
  (let* ((node-id (--> (pkm2-node-db-node habit-pkm-node) (pkm2-db-node-id it)))
         (content (--> (pkm2-node-db-node habit-pkm-node) (pkm2-db-node-content it)))
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
         (habit-schedule-hour (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-schedule-hour")
                                   (when it
                                     (if (length> it 1)
                                         (error "habit node has more than one interval")
                                       (pkm2-db-kvd-value (car it))))))
         (habit-deadline-hour (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-deadline-hour")
                                   (when it
                                     (if (length> it 1)
                                         (error "habit node has more than one interval")
                                       (pkm2-db-kvd-value (car it))))))
         (habit-requireds (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-required")
                              (-map #'pkm2-db-kvd-value it)))


         (habit-period (* habit-interval habit-period) )
         (habit-instances (--> `((:or db-nodes (:db-node-ids (,node-id)))
                                 (:convert-and convert-to-children (:levels 3))
                                 (:and structure-type (:structure-name habit-instance)))
                               (progn (message "query: %S" it) it)
                               (pkm2--compile-full-db-query it)
                               (progn (message "query: %S" it) it )
                               (sqlite-select pkm2-database-connection it)
                               (-flatten it)
                               (-map #'pkm2--db-query-get-node-with-id it)))
         (todo-habit-instance (-find (lambda (pkm-node)
                                       (or (pkm2-node-has-key-value pkm-node "task-status" "TODO")
                                           (pkm2-node-has-key-value pkm-node "task-status" "HOLD")
                                           (pkm2-node-has-key-value pkm-node "task-status" "DOING")))
                                     habit-instances))
         (most-recent-done-time (unless todo-habit-instance
                                  (--> (-map (lambda (h-s-n)
                                               (pkm2-node-get-kvds-with-key h-s-n "task-status" ))
                                             habit-instances)
                                       (-flatten it)
                                       (-filter (lambda (kvd)
                                                  (or (equal (pkm2-db-kvd-value kvd) "DONE") (equal (pkm2-db-kvd-value kvd) "KILL") )) it)
                                       (-map (lambda (kvd)
                                               (pkm2--db-query-get-kvd-link (pkm2-db-kvd-link-id kvd) (pkm2-db-kvd-type kvd)))
                                             it)
                                       (-map #'pkm2-db-kvd-link-created_at it)
                                       (when it (-max it) ))))
         (create-instance (and
                           (-all? (lambda (required)
                                    (cond ((equal required "emacs-active")
                                           (current-idle-time))
                                          (t (error  (format "habit required not recongnized %S" required)))))
                                  habit-requireds)
                           (cond (todo-habit-instance nil)
                                 ((not most-recent-done-time) t)
                                 (most-recent-done-time
                                  (message "freq: %S, period: %S" habit-frequency habit-period)

                                  (> (- (pkm2-get-current-timestamp) habit-period) most-recent-done-time))) ))
         (schedule-timestamp (when (and create-instance habit-schedule-hour)
                               (let* ((now (ts-now))
                                      (current-hour (ts-hour now))
                                      (current-minute (ts-minute now))
                                      (schedule-hour (truncate habit-schedule-hour))
                                      (schedule-minute (truncate (* (mod habit-schedule-hour 1) 60 ) )))
                                 (when (and (<= current-hour schedule-hour) (< current-minute schedule-minute))
                                   (--> (ts-apply :hour schedule-hour :minute schedule-minute now)
                                        (ts-unix it)
                                        (truncate it))))))
         (deadline-timestamp (when (and create-instance habit-deadline-hour)
                               (--> (ts-apply :hour (truncate habit-deadline-hour) :minute (truncate (* (mod habit-deadline-hour 1) 60) ) (ts-now))
                                    (ts-unix it)
                                    (truncate it))))

         (link-label "instance")
         (link-definition (list :pkm-link-label link-label
                                :parent 'parent
                                :child 'child))
         (structure-schema (list :name 'parent-child-node
                                 :assets (-non-nil
                                          (list
                                           `(:pkm-type node :name "parent-node" :parent-node t :db-id ,node-id)
                                           `(:pkm-type habit-instance :name "child-node" :child-node t)
                                           (when schedule-timestamp
                                             `(:pkm-type kvd :name "schedule" :key "schedule" :value ,schedule-timestamp :link-to ("child-node") :data-type INTEGER))
                                           (when deadline-timestamp
                                             `(:pkm-type kvd :name "deadline" :key "deadline" :value ,deadline-timestamp :link-to ("child-node") :data-type INTEGER))) )
                                 :links (list link-definition))))
    (message "t-h-i: %S" todo-habit-instance)
    (message "create instance: %S" create-instance)
    (if todo-habit-instance
        todo-habit-instance
      (when create-instance (pkm2--object-capture-object-verify
                             structure-schema
                             nil
                             t
                             `(("child-node" . (("base-node" . ,content)
                                                ("task-status" . "TODO")
                                                ("task-priority" . 1)))))))))

(defun pkm-habit-setup-habits ()
  (--> `((:or structure-type (:structure-name habit-node)))
       (pkm2--compile-full-db-query it)
       (sqlite-select pkm2-database-connection it)
       (-flatten it)
       (-map #'pkm2--db-query-get-node-with-id it)
       (-each it #'pkm2-habit-create-instances)))
(defun pkm-habit-manage-habits ()
  (pkm-habit-setup-habits))


(provide 'pkm2-habit)
