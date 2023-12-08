;;; pkm2-clock.el -*- lexical-binding: t; -*-

(require 'dash)
(require 'pkm-new-core)
(require 'pkm2-browse)

(defvar pkm2-clock-auto-clock-into-parent t)
(defvar pkm2-clock-auto-clock-node-types '(project-s area-s task-n))


(pkm-register-structure 'clock-node
                        (list :parent 'dependent-node
                              :browse-insert-format-string (concat
                                                            "[<insert>(:display kvd-value :key \"clock-start\")</insert>]"
                                                            "--"
                                                            "[<insert>(:display kvd-value :key \"clock-end\")</insert>]"
                                                            "<insert>(:display hidden :prefix \"\\n\")</insert>")
                              :managed-type t
                              :assets (list
                                       '(:pkm-type node :name "base-node" :primary-node t :no-input t)
                                       '(:pkm-type kvd :name "is-clock" :key "node-type" :value "clock" :link-to ("base-node") :data-type TEXT)
                                       `(:pkm-type kvd :name "clock-start" :key "clock-start" :value ,#'pkm2-get-user-selected-timestamp
                                         :link-to ("base-node") :data-type DATETIME )
                                       `(:pkm-type kvd :name "clock-end" :key "clock-end" :value ,#'pkm2-get-user-selected-timestamp
                                         :link-to ("base-node") :data-type DATETIME :managed t))))





(defun pkm2-clock--get-current-clock-pkm-nodes ()
  ; TODO implment and test this
  (--> `((:or structure-type (:structure-name clock-node)) (:not kvd (:key "clock-end" :data-type INTEGER)) )
       (pkm2--compile-full-db-query it)
       (sqlite-select pkm2-database-connection it)
       (-flatten it)
       (-map #'pkm2--db-query-get-node-with-id it)))

(defun pkm2-clock--get-current-clock-parent-pkm-nodes ()
  ; TODO implment and test this
  (--> `((:or structure-type (:structure-name clock-node))
          (:not kvd (:key "clock-end" :data-type INTEGER))
          (:convert-and convert-to-parents (:levels 1 :link-type "clock")) )
       (pkm2--compile-full-db-query it)
       (sqlite-select pkm2-database-connection it)
       (-flatten it)
       (-map #'pkm2--db-query-get-node-with-id it)))

(defun pkm2-clock--find-clockable-parents (db-ids auto-clock-types)
  "Returns clockable"
  (let* ((parent-ids (-flatten
                      (-non-nil
                       (-map (lambda (s-type)
                               (--> `((:or db-nodes (:db-node-ids ,db-ids))
                                      (:convert-and convert-to-parents (:levels ALL))
                                      (:and structure-type (:structure-name ,s-type)))
                                    (pkm2--compile-full-db-query it)
                                    (sqlite-select pkm2-database-connection it)
                                    (-flatten it)
                                    ))
                             auto-clock-types) )))
         (clockable-parents-pkm-nodes (-map #'pkm2--db-query-get-node-with-id parent-ids )))
    clockable-parents-pkm-nodes
    ))

(defun pkm2-clock-in (&optional db-id)
  (interactive)
  (let* ((structure-name 'clock-node)
         (parent-node-db-id (or db-id (get-text-property  (point) :pkm2-node-db-id) (pkm2-nodes-search "Search node to clock into: ")))
         (existing-clock (--> (pkm2-clock--get-current-clock-pkm-nodes)
                              (-find
                               (lambda (clock-pkm-node)
                                 (-as-> (pkm2-node-parent-links clock-pkm-node)
                                        it-clock
                                        (-map  #'pkm2-db-nodes-link-node_a it-clock)
                                        (member parent-node-db-id it-clock)))
                               it)))


         (link-label "clock")
         (link-definition (list :pkm-link-label link-label
                                :parent 'parent
                                :child 'child))
         (structure-schema (list :name 'parent-child-node
                                 :assets (list
                                          `(:pkm-type node :name "parent-node" :parent-node t :db-id ,parent-node-db-id)
                                          `(:pkm-type ,structure-name :name "child-node" :child-node t))
                                 :links (list link-definition))))
    (if existing-clock (message "Already clocked in")
      (pkm2--object-capture-object-verify structure-schema "pkm-clock" t))))

(defun pkm2-clock-switch ()
  (interactive)
  (pkm2-clock-out)
  (pkm2-clock-in))


(defun pkm2-clock-out ()
  (interactive)
  (if-let* ((active-clocks  (pkm2-clock--get-current-clock-pkm-nodes))
            (active-clocks-parents (-map (lambda (a-c)
                                           (--> (pkm2-node-parent-links a-c)
                                                (-filter (lambda (link)
                                                           (equal "clock" (pkm2-db-nodes-link-type link))) it)
                                                (-map #'pkm2--link-get-link-parent-id it)
                                                (-map #'pkm2--db-query-get-node-with-id it)))
                                         active-clocks))
            (completing-read-choices (-flatten-n 1 (-map-indexed (lambda (index a-c-ps)
                                                               (--> (-map #'pkm2-node-db-node a-c-ps)
                                                                    (-map #'pkm2-db-node-content it)
                                                                    (-map-indexed (lambda (index2 clock-content)
                                                                                    (cons clock-content (cons (nth index active-clocks) (nth index2 a-c-ps) ))) it)))
                                                             active-clocks-parents) ) )
            (choice (when completing-read-choices
                      (--> (completing-read "Which clock would you like to stop?" completing-read-choices)
                           (assoc-default it completing-read-choices)) ))
            (active-clock (car choice))
            (active-clock-parent (cdr choice))
            (start-time-string (--> (pkm2-node-get-kvds-with-key active-clock "clock-start") (car it) (pkm2-db-kvd-value it) (pkm2--convert-object-to-string it 'DATETIME)))
            (end (pkm2-get-user-selected-timestamp (format "start: %s, Clock end: " start-time-string)))
            (kvd  (pkm2--db-get-or-insert-kvd "clock-end" end 'INTEGER))
            (node-id (--> (pkm2-node-db-node active-clock) (pkm2-db-node-id it)))
            (kvd-id (pkm2-db-kvd-id kvd))
            (link (pkm2--db-insert-link-between-node-and-kvd node-id kvd-id (pkm2-get-current-timestamp) 'INTEGER)))
      (when (y-or-n-p "Clock out done, would you like to clock into a parent node?")
        (if-let* ((clocked-node-db-id (--> (pkm2-node-db-node active-clock-parent) (pkm2-db-node-id it)))
                  (clockable-parents (when pkm2-clock-auto-clock-into-parent
                                       (pkm2-clock--find-clockable-parents `(,clocked-node-db-id) pkm2-clock-auto-clock-node-types)))
                  (completing-read-choices (-flatten (-map (lambda (c-p)
                                                                     (--> (pkm2-node-db-node c-p)
                                                                          (pkm2-db-node-content it)
                                                                          (cons it c-p)))
                                                                   clockable-parents)))
                  (choice (when completing-read-choices
                            (--> (completing-read "Which clock would you like to stop?" completing-read-choices)
                                 (assoc-default it completing-read-choices))))
                  (parent-clock (--> (pkm2-node-db-node choice) (pkm2-db-node-id it) (pkm2-clock-in it))))
            parent-clock
          (message "No parent clock to clock into")))
    (message "No active clock")))



(defun pkm2--convert-into-db-query-active-clock (input &optional nodes-queries)
  (--> `((:or structure-type (:structure-name clock-node))
         (:not kvd (:key "clock-end" :data-type INTEGER))
         (:convert-or convert-to-parents (:levels 1 :link-type "clock")) )
       (pkm2--compile-full-db-query it)) )

(defun pkm--convert-into-get-spec-active-clock ())

(pkm2--register-query-spec-option 'active-clock '() #'pkm--convert-into-get-spec-active-clock #'pkm2--convert-into-db-query-active-clock)

(provide 'pkm2-clock)
