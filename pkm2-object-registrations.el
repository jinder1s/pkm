;;; pkm-object-todo.el -*- lexical-binding: t; -*-
(require 'pkm-new-core)


(pkm2-register-behavior-2 (behavior-schema
                           :name "log"
                           :assets (list
                                    (kvd-schema :name "is-log" :key "node-type" :value "log"  :data-type 'TEXT)
                                    (kvd-schema :name "timestamp" :key "timestamp" :value #'pkm2-get-current-timestamp  :data-type 'DATETIME))))



(pkm2-register-behavior-2 (behavior-schema :name "dependent" :assets (list (kvd-schema :name "is-dependent" :key "node-type" :value "DEPENDENT" :data-type 'TEXT))))
;; (pkm2-register-behavior-2 (behavior-schema :name "time-interval"
;;                           :assets (list
;;                                    (kvd-schema
;;                                     :name "time-interval"
;;                                     :assets
;;                                     (list (kvd-schema  :name "time-start" :key "time-start" :value #'pkm2-get-user-selected-timestamp :data-type 'DATETIME)
;;                                           (kvd-schema  :name "time-end" :key "time-end" :value #'pkm2-get-user-selected-timestamp :data-type 'DATETIME :managed t)
;;                                           (kvd-schema  :name "interval-type" :key "interval-type" :value "time-interval" :data-type 'TEXT))))))


;; (pkm-register-structure-2 (object-schema :name "time-n" :parent-schema-name "base-n"
;;                                          :is-behavior "time-interval"
;;                                          :behaviors (list `(:name "time-interval" :link-to (primary)))))

(pkm-register-structure-2
 (object-schema :name "base-n" :assets  (list
                                (node-schema :name "base-node" :primary-node t))))


(pkm-register-structure-2
 (object-schema :name "dependent-n" :parent-schema-name "base-n"
                :assets
                (list
                 (kvd-schema  :name "is-dependent" :key "node-type" :value "DEPENDENT" :link-to '("base-node") :data-type 'TEXT))))

(pkm-register-structure-2
      (object-schema :name "note-n" :parent-schema-name "base-n"
            :assets (list
                     (kvd-schema  :name "is-note" :key "node-type" :value "note" :link-to '("base-node") :data-type 'TEXT))))

(pkm-register-structure-2
      (object-schema :name "container-s" :parent-schema-name "base-n"
            :assets (list
                     (kvd-schema :name "is-container" :key "node-type" :value "container" :link-to '("base-node") :data-type 'TEXT))))



(pkm-register-structure-2
                        (object-schema :name "goal-n" :parent-schema-name "base-n"
                              :log-primary t
                              :assets (list
                                       (kvd-schema  :name "is-goal" :key "node-type" :value "goal" :link-to '("base-node") :data-type 'TEXT))))

(pkm-register-structure-2
      (object-schema :name "area-s" :parent-schema-name "base-n"
            :assets (list
                     (kvd-schema  :name "is-area" :key "node-type" :value "area" :link-to '("base-node") :data-type 'TEXT))))

(pkm-register-structure-2
 (object-schema :parent-schema-name "base-n" :name "log-n"
                :browse-insert-format-string (concat
                                              "<insert>(:display kvd-value :key \"timestamp\")</insert> "
                                              "<insert>(:display content)</insert>"
                                              "<insert>(:display hidden :prefix \"\\n\")</insert>")
                :is-behavior "log"
                :behaviors (list `(:name "log" :link-to (primary)))))

(pkm-register-structure-2
                        ; TODO this should be both a note and a log
      (object-schema :name "observation-log-n" :parent-schema-name "log-n"
            :assets (list
                     (kvd-schema  :name "is-observation" :key "node-type" :value "observation" :link-to '(primary) :data-type 'TEXT))))

(pkm-register-structure-2
                        ; TODO this should be both a note and a log
      (object-schema  :name "documentation-log-n" :parent-schema-name "log-n"
            :assets (list
                     (kvd-schema  :name "is-documentation" :key "node-type" :value "documentation" :link-to '(primary) :data-type 'TEXT))))

(pkm-register-structure-2
                        (object-schema :name "idea-note-n" :parent-schema-name "note-n"
                              :assets (list
                                       (kvd-schema  :name "is-idea" :key "node-type" :value "idea" :link-to '("base-node") :data-type 'TEXT))))


(pkm-register-structure-2
  (object-schema :name "entity-s" :parent-schema-name "base-n"
        :assets (list
                 (kvd-schema  :name "is-entity" :key "node-type" :value "entity" :link-to '("base-node") :data-type 'TEXT))))

(pkm-register-structure-2 (object-schema :name "resource-n" :parent-schema-name "base-n"
                                         :assets (list
                                                  (kvd-schema  :name "is-resource" :key "node-type" :value "resource" :link-to '("base-node") :data-type 'TEXT))))

;; (pkm-register-structure-2 'link-n
;;       (list :parent-schema-name 'resource-n
;;             :assets (list
;;                      (kvd-schema  :name "is-link" :key "node-type" :value "link" :link-to '("base-node") :data-type 'TEXT))))

;; (pkm-register-structure-2 'book-n
;;       (list :parent-schema-name 'resource-n
;;             :assets (list
;;                      (kvd-schema  :name "is-book" :key "node-type" :value "book" :link-to '("base-node") :data-type 'TEXT)
;;                      (kvd-schema  :name "author" :key "author"  :link-to '("base-node") :data-type 'TEXT)
;;                      (kvd-schema  :name "description" :key "description"  :link-to '("base-node") :data-type 'TEXT)
;;                      (kvd-schema  :name "web-link" :key "web-link"  :link-to '("base-node") :data-type 'TEXT :optional t))))

(pkm-register-structure-2
      (object-schema :name "webpage-n"
                     :parent-schema-name "resource-n"
                     :browse-insert-format-string (format "%s %s"
                                                          "<insert>(:display content)</insert>"
                                                          "<insert>(:display hidden :prefix \"\\n\")</insert>")



                     :asset-modifications (list '(:name "base-node" :plist-key :prompt :plist-value "Description of link"))
                     :assets (list
                              (kvd-schema  :name "is-webpage" :key "node-type" :value "webpage" :link-to '("base-node") :data-type 'TEXT)
                              (kvd-schema  :name "author" :key "author"  :link-to '("base-node") :data-type 'TEXT :optional t)
                              (kvd-schema  :name "web-link" :key "web-link"  :link-to '("base-node") :data-type 'TEXT ))))

;; (pkm-register-structure-2 'location-s
;;                         (list :parent-schema-name 'entity
;;                               :assets (list
;;                                        '(kvd-schema  :name "is-location" :key "node-type" :value "location" :link-to '("base-node") :data-type 'TEXT))))

;; (pkm-register-structure-2 'person-s
;;   (list :parent-schema-name 'entity
;;         :assets (list '(kvd-schema  :name "is-person" :key "node-type" :value "person" :link-to '("base-node") :data-type 'TEXT)
;;                       '(kvd-schema  :name "date-of-birth" :key "date-of-birth"
;;                               :prompt "When is this person's birthdate"
;;                               :link-to '("base-node")
;;                               :data-type 'DATETIME
;;                               :optional t)
;;                       '(kvd-schema  :name "person-relation" :key "person-relation"
;;                               :prompt "What is their relation to me?"
;;                               :choices ("sister" "friend" "family" "work")
;;                               :allow-additional-choices t
;;                               :link-to '("base-node")
;;                               :optional t
;;                               :data-type 'TEXT))))






(provide 'pkm2-object-registrations)
