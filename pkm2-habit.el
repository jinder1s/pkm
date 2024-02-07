;;; pkm/pkm2-habit.el -*- lexical-binding: t; -*-

(require 'ts)
(require 'pkm-new-core)
(require 'pkm2-clock)

(defvar pkm2-habit-hourly-paused nil)

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
                                         :name "habit-required"
                                         :key "habit-required"
                                         :link-to ("base-node")
                                         :data-type TEXT
                                         :optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-or-conditional"
                                         :key "habit-or-conditional"
                                         :link-to ("base-node")
                                         :data-type TEXT
                                         :optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-auto-kill"
                                         :key "habit-auto-kill"
                                         :link-to ("base-node")
                                         :data-type INTEGER
                                         :optional t
                                         :prompt "")

                                       `(:pkm-type kvd
                                         :name "habit-active-weekday"
                                         :key "habit-active-weekday"
                                         :link-to ("base-node")
                                         :data-type TEXT
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-active-day-hour"
                                         :key "habit-active-day-hour"
                                         :link-to ("base-node")
                                         :data-type INTEGER
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-active-month-week"
                                         :key "habit-active-month-week"
                                         :link-to ("base-node")
                                         :data-type INTEGER
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-active-month"
                                         :key "habit-active-month"
                                         :link-to ("base-node")
                                         :data-type INTEGER
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")

                                       `(:pkm-type kvd
                                         :name "habit-unactive-weekday"
                                         :key "habit-unactive-weekday"
                                         :link-to ("base-node")
                                         :data-type TEXT
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-unactive-day-hour"
                                         :key "habit-unactive-day-hour"
                                         :link-to ("base-node")
                                         :data-type INTEGER
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-unactive-month-week"
                                         :key "habit-unactive-month-week"
                                         :link-to ("base-node")
                                         :data-type INTEGER
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-unactive-month"
                                         :key "habit-unactive-month"
                                         :link-to ("base-node")
                                         :data-type INTEGER
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")

                                       `(:pkm-type kvd
                                         :name "habit-schedule-hour"
                                         :key "habit-schedule-hour"
                                         :link-to ("base-node")
                                         :data-type REAL
                                         :optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-schedule-week-day"
                                         :key "habit-schedule-week-day"
                                         :link-to ("base-node")
                                         :data-type REAL
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-schedule-month-day"
                                         :key "habit-schedule-month-day"
                                         :link-to ("base-node")
                                         :data-type REAL
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-schedule-year-day"
                                         :key "habit-schedule-year-day"
                                         :link-to ("base-node")
                                         :data-type REAL
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-schedule-month-week"
                                         :key "habit-schedule-month-week"
                                         :link-to ("base-node")
                                         :data-type REAL
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-schedule-year-week"
                                         :key "habit-schedule-year-week"
                                         :link-to ("base-node")
                                         :data-type REAL
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-schedule-year-month"
                                         :key "habit-schedule-year-month"
                                         :link-to ("base-node")
                                         :data-type REAL
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")

                                       `(:pkm-type kvd
                                         :name "habit-deadline-hour"
                                         :key "habit-deadline-hour"
                                         :link-to ("base-node")
                                         :data-type REAL
                                         :optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-deadline-week-day"
                                         :key "habit-deadline-week-day"
                                         :link-to ("base-node")
                                         :data-type REAL
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-deadline-month-day"
                                         :key "habit-deadline-month-day"
                                         :link-to ("base-node")
                                         :data-type REAL
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-deadline-year-day"
                                         :key "habit-deadline-year-day"
                                         :link-to ("base-node")
                                         :data-type REAL
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-deadline-month-week"
                                         :key "habit-deadline-month-week"
                                         :link-to ("base-node")
                                         :data-type REAL
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-deadline-year-week"
                                         :key "habit-deadline-year-week"
                                         :link-to ("base-node")
                                         :data-type REAL
                                         :optional t
                                         :advanced-optional t
                                         :prompt "")
                                       `(:pkm-type kvd
                                         :name "habit-deadline-year-month"
                                         :key "habit-deadline-year-month"
                                         :link-to ("base-node")
                                         :data-type REAL
                                         :optional t
                                         :advanced-optional t
                                         :prompt ""))))

(pkm-register-structure 'habit-alternative-node
                        (list :parent 'dependent-node
                              :browse-insert-format-string (concat "")
                              :managed-type t
                              :assets (list
                                       '(:pkm-type node :name "base-node" :primary-node t :no-input t)
                                       '(:pkm-type kvd :name "is-habit-alternative" :key "node-type" :value "habit-alternative" :link-to ("base-node") :data-type TEXT))))

(pkm-register-structure 'habit-choice-node
                        (list :parent 'dependent-node
                              :browse-insert-format-string (concat "")
                              :managed-type t
                              :assets (list
                                       '(:pkm-type node :name "base-node" :primary-node t :no-input t)
                                       '(:pkm-type kvd :name "is-habit-choice" :key "node-type" :value "habit-choice" :link-to ("base-node") :data-type TEXT))))

(pkm-register-structure 'habit-instance
                        (list :parent 'dependent-node
                              :behaviors (list '(:name task :link-to (primary)))
                              :asset-modifications (list '(:name "task-status" :plist-key :log-ask-timestamp :plist-value t))
                              :assets (list
                                       '(:pkm-type kvd :name "is-habit-instance" :key "node-type" :value "habit-instance" :link-to ("base-node") :data-type TEXT)
                                       '(:pkm-type node :name "base-node" :primary-node t :no-input t)
                                       '(:pkm-type kvd :name "schedule" :key "schedule"  :link-to ("base-node") :data-type DATETIME :optional t)
                                       `(:pkm-type kvd :name "deadline" :key "deadline"  :link-to ("base-node") :data-type DATETIME :optional t))))



(defun pkm2-habit-should-create-habit-instance (habit-pkm-node)
  (let* ((node-id (pkm-get-db-id habit-pkm-node))
         (habit-frequency (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-freq")
                               (when it
                                 (if (length> it 1)
                                     (error "habit node has more than one frequency")
                                   (oref (car it) :value)))))
         (habit-period (assoc-default habit-frequency pkm2-habit-freq-to-seconds))
         (habit-interval (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-interval")
                              (when it
                                (if (length> it 1)
                                    (error "habit node has more than one interval")
                                  (oref (car it) :value)))
                              (or it 1)))
         (habit-schedule-hour (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-schedule-hour")
                                   (when it
                                     (if (length> it 1)
                                         (error "habit node has more than one habit-schedule-hour")
                                       (oref (car it) :value)))))
         (habit-deadline-hour (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-deadline-hour")
                                   (when it
                                     (if (length> it 1)
                                         (error "habit node has more than one habit-deadline-hour")
                                       (oref (car it) :value)))))
         (habit-deadline-minute (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-deadline-minute")
                                     (when it
                                       (if (length> it 1)
                                           (error "habit node has more than one habit-deadline-minute")
                                         (oref (car it) :value)))))
         (habit-requireds (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-required")
                               (-map (lambda (kvd) (oref kvd :value)) it)))


         (habit-period (* habit-interval habit-period))
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
                                                  (or (equal (oref kvd :value) "DONE") (equal (oref kvd :value) "KILL") )) it)
                                       (-map (lambda (kvd)
                                               (pkm2--db-query-get-kvd-link (pkm2-db-kvd-link-id kvd) (pkm2-db-kvd-type kvd)))
                                             it)
                                       (-map #'pkm2-db-kvd-link-created_at it)
                                       (when it (-max it) ))))
         (most-recent-deadline (unless todo-habit-instance
                                 (--> (-map (lambda (h-s-n)
                                              (pkm2-node-get-kvds-with-key h-s-n "deadline" ))
                                            habit-instances)
                                      (-flatten it)
                                      (-map (lambda (kvd) (oref kvd :value)) it)
                                      (when it (-max it) ))))
         (create-instance (and
                           (not todo-habit-instance)
                           (if  (equal habit-frequency "hourly")
                               (if (pkm2-clock--get-recent-clock-pkm-nodes `(minute -20))
                                   t
                                 nil)
                             t)
                           (-all?
                            (lambda (required)
                              (cond ((equal required "emacs-active")
                                     (current-idle-time))
                                    (t (error  (format "habit required not recongnized %S" required)))))
                            habit-requireds)
                           (cond ((not most-recent-done-time) t)
                                 ((and most-recent-deadline (> (pkm2-get-current-timestamp)  most-recent-deadline) t)
                                  t))
                           (or
                            (if habit-schedule-hour
                                (when (or (equal habit-frequency "daily") (equal habit-frequency "hourly"))
                                  (let* ((now (ts-now))
                                         (current-hour (ts-hour now))
                                         (current-minute (ts-minute now))
                                         (schedule-hour (truncate habit-schedule-hour))
                                         (schedule-minute (truncate (* (mod habit-schedule-hour 1) 60))))
                                    (when (and (<= current-hour schedule-hour) (< current-minute schedule-minute))
                                      t)))
                              t)
                            (if habit-deadline-hour
                                (when (or (equal habit-frequency "daily") (equal habit-frequency "hourly"))
                                  (let* ((now (ts-now))
                                         (current-hour (ts-hour now))
                                         (current-minute (ts-minute now))
                                         (deadline-hour (truncate habit-deadline-hour))
                                         (deadline-minute (truncate (* (mod habit-deadline-hour 1) 60))))
                                    (when  (and (<= current-hour deadline-hour) (< current-minute deadline-minute))
                                      t)))
                              t)))))
    (when create-instance t)))

(defun pkm2-habit-should-delete-habit-instance (habit-instance-pkm-node)
  "If no active clock, and habit instance is hourly and no recent clocks, delete the hourly instance."
  (unless (pkm2-clock--get-current-clock-pkm-nodes)
    (let* ((node-id (pkm-get-db-id habit-instance-pkm-node))
           (habit-nodes (--> `((:or db-nodes (:db-node-ids (,node-id)))
                               (:convert-and convert-to-parents (:levels 1))
                               (:and structure-type (:structure-name habit-node)))
                             (pkm2--compile-full-db-query it)
                             (sqlite-select pkm2-database-connection it)
                             (-flatten it)
                             (-map #'pkm2--db-query-get-node-with-id it)))
           (habit-freqs (-map (lambda (habit-pkm-node)
                                (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-freq")
                                     (when it
                                       (if (length> it 1)
                                           (error "habit node has more than one habit-auto-kill value")
                                         (oref (car it) :value)))))
                              habit-nodes))
           (is-hourly (-find (lambda (freq) (equal freq "hourly")) habit-freqs))
           (recent-clocks (when is-hourly (pkm2-clock--get-recent-clock-pkm-nodes `(minute -20)))))
      (when (and is-hourly (not recent-clocks))
        t))))

(defun pkm2-habit-should-kill-habit-instance (habit-instance-pkm-node)
  (let* ((node-id (pkm-get-db-id habit-instance-pkm-node))
         (habit-instance-deadline (--> (pkm2-node-get-kvds-with-key habit-instance-pkm-node "deadline")
                              (when it
                                (if (length> it 1)
                                    (error "habit node has more than one deadline")
                                  (oref (car it) :value)))))
         (habit-nodes (--> `((:or db-nodes (:db-node-ids (,node-id)))
                             (:convert-and convert-to-parents (:levels 1))
                             (:and structure-type (:structure-name habit-node)))
                           (pkm2--compile-full-db-query it)
                           (sqlite-select pkm2-database-connection it)
                           (-flatten it)
                           (-map #'pkm2--db-query-get-node-with-id it)))
         (auto-kill-if-deadline (-find  (lambda (habit-pkm-node)
                                          (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-auto-kill")
                                               (when it
                                                 (if (length> it 1)
                                                     (error "habit node has more than one habit-auto-kill value")
                                                   (oref (car it) :value)))
                                               (when (and it (> it 0))
                                                 t)))
                                        habit-nodes)))
    (when (and auto-kill-if-deadline (> (--> (ts-now) (ts-unix it)) habit-instance-deadline))
      t)))

(defun pkm2-habit-log-habit-done (habit-pkm-node))


(defun pkm2-habit-switch-to-alternative ())

(defun pkm2-habit-create-instances (habit-pkm-node)
  (let* ((node-id (pkm-get-db-id habit-pkm-node))
         (content (oref habit-pkm-node :content))
         (habit-frequency (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-freq")
                               (when it
                                 (if (length> it 1)
                                     (error "habit node has more than one frequency")
                                   (oref (car it) :value)))))
         (habit-period (assoc-default habit-frequency pkm2-habit-freq-to-seconds))
         (habit-interval (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-interval")
                              (when it
                                (if (length> it 1)
                                    (error "habit node has more than one interval")
                                  (oref (car it) :value)))
                              (or it 1)))
         (habit-schedule-hour (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-schedule-hour")
                                   (when it
                                     (if (length> it 1)
                                         (error "habit node has more than one habit-schedule-hour")
                                       (oref (car it) :value)))))
         (habit-deadline-hour (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-deadline-hour")
                                   (when it
                                     (if (length> it 1)
                                         (error "habit node has more than one habit-deadline-hour")
                                       (oref (car it) :value)))))
         (habit-deadline-minute (--> (pkm2-node-get-kvds-with-key habit-pkm-node "habit-deadline-minute")
                                     (when it
                                       (if (length> it 1)
                                           (error "habit node has more than one habit-deadline-minute")
                                         (oref (car it) :value)))))
         (habit-period (* habit-interval habit-period))
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

         (schedule-timestamp (if (equal habit-frequency "hourly")
                                 (--> (ts-now)
                                      (ts-unix it)
                                      (truncate it))
                               (when   habit-schedule-hour
                                 (let* ((now (ts-now))
                                        (current-hour (ts-hour now))
                                        (current-minute (ts-minute now))
                                        (schedule-hour (truncate habit-schedule-hour))
                                        (schedule-minute (truncate (* (mod habit-schedule-hour 1) 60 ) )))
                                   (when (and (<= current-hour schedule-hour) (< current-minute schedule-minute))
                                     (--> (ts-apply :hour schedule-hour :minute schedule-minute now)
                                          (ts-unix it)
                                          (truncate it))))) ))
         (deadline-timestamp (cond
                              ((equal habit-frequency "hourly")
                               (--> (ts-now)
                                    (ts-apply :hour (+ (ts-hour it) 1) it)
                                    (ts-unix it)
                                    (truncate it)))
                              (habit-deadline-hour
                               (--> (ts-apply :hour (truncate habit-deadline-hour) :minute (truncate (* (mod habit-deadline-hour 1) 60) ) (ts-now))
                                    (ts-unix it)
                                    (truncate it)))
                              (t (-->
                                  (+ (ts-unix (ts-now)) habit-period)
                                  (truncate it)))))
         (deadline-alert-minutes (cond ((equal habit-frequency "hourly") 10)
                                       ((equal habit-frequency "daily") 60)))

         (link-label "instance")
         (link-definition (list :pkm-link-label link-label
                                :parent 'parent
                                :child 'child))
         (structure-schema (list :name 'parent-child-node
                                 :assets (-non-nil
                                          (list
                                           `(:pkm-type node :name "parent-node" :parent-node t :db-id ,node-id)
                                           `(:pkm-type habit-instance :name "child-node" :child-node t)))
                                 :links (list link-definition))))
    (if todo-habit-instance
        todo-habit-instance
      (pkm2--object-capture-object-verify
       structure-schema
       nil
       t
       `(("child-node" . (("base-node" . ,content)
                          ("task-status" . "TODO")
                          ("task-priority" . 1)
                          ,(when schedule-timestamp `("schedule" . ,schedule-timestamp))
                          ,(when deadline-timestamp `("deadline" . ,deadline-timestamp))
                          ,(when deadline-alert-minutes `("deadline-alert-minutes" . ,deadline-alert-minutes)))))))))


(defun pkm-log-habit ()
  "Log habit"
  ;; TODO Implement
  (error "Not implemented"))


(defun pkm-habit-alert ()
  (let* ((active-habit-instances-with-deadlines
          (--> `((:or structure-type (:structure-name habit-instance))
                 (:and kvd (:key "task-status" :data-type TEXT :choices ("DOING" "TODO" "HOLD")))
                 (:and kvd (:key "deadline" :data-type INTEGER)))
               (pkm2--compile-full-db-query it)
               (sqlite-select pkm2-database-connection it)
               (-flatten it)
               (-map #'pkm2--db-query-get-node-with-id it)))
         (deadlines (-map  (lambda (h-i-d)
                             (--> (pkm2-node-get-kvds-with-key h-i-d "deadline")
                                  (when it
                                    (if (length> it 1)
                                        (error "habit instance has more than one deadline")
                                      (oref (car it) :value)))) )
                           active-habit-instances-with-deadlines))
         (alert-seconds (-map  (lambda (h-i-d)
                                 (--> (pkm2-node-get-kvds-with-key h-i-d "deadline-alert-minutes")
                                      (when it
                                        (if (length> it 1)
                                            (error "habit instance has more than one deadline-alert-minutes")
                                          (oref (car it) :value)))
                                      (when it (* it 60))) )
                               active-habit-instances-with-deadlines))
         (is-hourly (-map (lambda (h-i-d)
                            (--> `((:or db-nodes (:db-node-ids (,(pkm-get-db-id h-i-d))))
                                   (:convert-or convert-to-parents (:levels ALL))
                                   (:and structure-type (:structure-name habit-node))
                                   (:and kvd (:key "habit-freq" :value "hourly" :data-type TEXT)))
                                 (pkm2--compile-full-db-query it)
                                 (sqlite-select pkm2-database-connection it)
                                 (-flatten it)
                                 (length> it 0)))
                          active-habit-instances-with-deadlines) ))
    (-each-indexed active-habit-instances-with-deadlines
      (lambda (index h-i-d)
        (when (not (and (nth index is-hourly) pkm2-habit-hourly-paused))
          (when  (< (- (nth index deadlines)  (or (nth index alert-seconds) (* 5 60))) (pkm2-get-current-timestamp))
            (message "Time to alert")
            (--> (oref h-i-d :content)
                 (format "Deadline soon: %s" it)
                 (hs-alert it))))))))


(defun pkm-pause-hourly ()
  (interactive)
  (setq pkm2-habit-hourly-paused (ts-now)))

(defun pkm-unpause-hourly ()
  (interactive)
  (setq pkm2-habit-hourly-paused nil)
  )

(defun pkm-habit-manage-habits ()
  (let* ((active-habit-instances
          (--> `((:or structure-type (:structure-name habit-instance))
                 (:and kvd (:key "task-status" :data-type TEXT :choices ("DOING" "TODO" "HOLD"))))
               (pkm2--compile-full-db-query it)
               (sqlite-select pkm2-database-connection it)
               (-flatten it)
               (-map #'pkm2--db-query-get-node-with-id it)))
         (a-h-i-delete (-filter #'pkm2-habit-should-delete-habit-instance active-habit-instances))
         (a-h-i-d-db-ids (-map (lambda (h-i-pkm-node)
                                  (pkm-get-db-id h-i-pkm-node)
                                      )
                               a-h-i-delete))
         (a-h-i-kill (-filter #'pkm2-habit-should-kill-habit-instance active-habit-instances))
         (a-h-i-kill-task-status-kvd (-map (lambda (a-h-i-pkm-node)
                                             (--> (pkm2-node-get-kvds-with-key a-h-i-pkm-node "task-status")
                                                  (when it
                                                    (if (length> it 1)
                                                        (error "habit instance has more than one task-status")
                                                      (car it)))))
                                           a-h-i-kill))
         (kill-kvd-id (--> (pkm2--db-get-or-insert-kvd "task-status" "KILL" 'TEXT)
                           (pkm-get-db-id it))))

    (-each a-h-i-d-db-ids #'pkm2--db-delete-node )
    (-each-indexed
        a-h-i-kill-task-status-kvd
      (lambda (index old-kvd)
        (pkm2--update-node-kvd (nth index a-h-i-kill)
                               old-kvd
                               kill-kvd-id
                               'TEXT
                               (pkm2-db-kvd-context-id old-kvd)
                               (pkm2-db-kvd-link-id old-kvd)
                               (pkm2-get-current-timestamp)))))
  (let* ((habit-nodes (-->
                       `((:or structure-type (:structure-name habit-node)))
                       (pkm2--compile-full-db-query it)
                       (sqlite-select pkm2-database-connection it)
                       (-flatten it)
                       (-map #'pkm2--db-query-get-node-with-id it)))
         (habits-to-create (-filter #'pkm2-habit-should-create-habit-instance habit-nodes))
         (new-instances (-map #'pkm2-habit-create-instances habits-to-create))
         )))


(defun hs-alert (message)
  "shows Hammerspoon's hs.alert popup with a MESSAGE"
  (when (and message (eq system-type 'darwin))
    (call-process (executable-find "hs")
                  nil 0 nil
                  "-c"
                  (concat "hs.alert.show(\"" message "\", 5)"))))
(provide 'pkm2-habit)
