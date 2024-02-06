;;; pkm-object-todo.el -*- lexical-binding: t; -*-


(require 'pkm-new-core)

(pkm2-register-behavior `(:name log :assets ((:pkm-type kvd :name "is-log" :key "node-type" :value "log"  :data-type TEXT)
                                             (:pkm-type kvd :name "timestamp" :key "timestamp" :value ,#'pkm2-get-current-timestamp  :data-type DATETIME))))


(pkm2-register-behavior `(:name dependent :assets ((:pkm-type kvd :name "is-dependent" :key "node-type" :value "DEPENDENT" :data-type TEXT))))
(pkm2-register-behavior `(:name time-interval
                          :assets ((:pkm-type kvd-group
                                    :assets
                                    ((:pkm-type kvd :name "time-start" :key "time-start" :value ,#'pkm2-get-user-selected-timestamp :data-type DATETIME)
                                     (:pkm-type kvd :name "time-start" :key "time-end" :value ,#'pkm2-get-user-selected-timestamp :data-type DATETIME)
                                     (:pkm-type kvd :name "interval-type" :key "interval-type" :value "time-interval" :data-type TEXT))))))

(pkm-register-structure
 'base-n
 (list :assets  (list
                 '(:pkm-type node :name "base-node" :primary-node t))))

(pkm-register-structure 'dependent-n
                        (list :parent 'base-n
                              :must-be-sub t ; TODO implement must be sub
                              :assets (list
                                       `(:pkm-type kvd :name "is-dependent" :key "node-type" :value "DEPENDENT" :link-to ("base-node") :data-type TEXT))))

(pkm-register-structure 'note-n
      (list :parent 'base-n
            :assets (list
                     `(:pkm-type kvd :name "is-note" :key "node-type" :value "note" :link-to ("base-node") :data-type TEXT))))

(pkm-register-structure 'container-s
      (list :parent 'base-n
            :assets (list
                     `(:pkm-type kvd :name "is-container" :key "node-type" :value "container" :link-to ("base-node") :data-type TEXT))))



(pkm-register-structure 'goal-n
                        (list :parent 'base-n
                              :log-primary t
                              :assets (list
                                       `(:pkm-type kvd :name "is-goal" :key "node-type" :value "goal" :link-to ("base-node") :data-type TEXT))))

(pkm-register-structure 'area-s
      (list :parent 'base-n
            :assets (list
                     `(:pkm-type kvd :name "is-area" :key "node-type" :value "area" :link-to ("base-node") :data-type TEXT))))

(pkm-register-structure 'log-n
                        (list :parent 'base-n
                              :browse-insert-format-string (concat
                                                            "<insert>(:display kvd-value :key \"timestamp\")</insert> "
                                                            "<insert>(:display content)</insert>"
                                                            "<insert>(:display hidden :prefix \"\\n\")</insert>")
                              :is-behavior 'log
                              :behaviors (list `(:name log :link-to (primary)))))

(pkm-register-structure 'observation-log-n
                        ; TODO this should be both a note and a log
      (list :parent 'log-n
            :assets (list
                     `(:pkm-type kvd :name "is-observation" :key "node-type" :value "observation" :link-to (primary) :data-type TEXT))))

(pkm-register-structure 'documentation-log-n
                        ; TODO this should be both a note and a log
      (list :parent 'log-n
            :assets (list
                     `(:pkm-type kvd :name "is-documentation" :key "node-type" :value "documentation" :link-to (primary) :data-type TEXT))))

(pkm-register-structure 'idea-note-n
                        (list :parent 'note-n
                              :assets (list
                                       `(:pkm-type kvd :name "is-idea" :key "node-type" :value "idea" :link-to ("base-node") :data-type TEXT))))


(pkm-register-structure 'entity-s
  (list :parent 'base-n
        :assets (list
                 '(:pkm-type kvd :name "is-entity" :key "node-type" :value "entity" :link-to ("base-node") :data-type TEXT))))

(pkm-register-structure 'resource-n
                        (list :parent 'base-n
                              :assets (list
                                       `(:pkm-type kvd :name "is-resource" :key "node-type" :value "resource" :link-to ("base-node") :data-type TEXT))))

(pkm-register-structure 'link-n
      (list :parent 'resource-n
            :assets (list
                     `(:pkm-type kvd :name "is-link" :key "node-type" :value "link" :link-to ("base-node") :data-type TEXT))))

(pkm-register-structure 'book-n
      (list :parent 'resource-n
            :assets (list
                     `(:pkm-type kvd :name "is-book" :key "node-type" :value "book" :link-to ("base-node") :data-type TEXT)
                     `(:pkm-type kvd :name "author" :key "author"  :link-to ("base-node") :data-type TEXT)
                     `(:pkm-type kvd :name "description" :key "description"  :link-to ("base-node") :data-type TEXT)
                     `(:pkm-type kvd :name "web-link" :key "web-link"  :link-to ("base-node") :data-type TEXT :optional t))))

(pkm-register-structure 'webpage-n
      (list :parent 'resource-n
            :browse-insert-format-string (format "%s %s"
                                                 "<insert>(:display content)</insert>"
                                                 "<insert>(:display hidden :prefix \"\\n\")</insert>")



            :asset-modifications (list '(:name "base-node" :plist-key :prompt :plist-value "Description of link"))
            :assets (list
                     `(:pkm-type kvd :name "is-webpage" :key "node-type" :value "webpage" :link-to ("base-node") :data-type TEXT)
                     `(:pkm-type kvd :name "author" :key "author"  :link-to ("base-node") :data-type TEXT :optional t)
                     `(:pkm-type kvd :name "web-link" :key "web-link"  :link-to ("base-node") :data-type TEXT ))))

(pkm-register-structure 'location-s
                        (list :parent 'entity
                              :assets (list
                                       '(:pkm-type kvd :name "is-location" :key "node-type" :value "location" :link-to ("base-node") :data-type TEXT))))

(pkm-register-structure 'person-s
  (list :parent 'entity
        :assets (list '(:pkm-type kvd :name "is-person" :key "node-type" :value "person" :link-to ("base-node") :data-type TEXT)
                      '(:pkm-type kvd :name "date-of-birth" :key "date-of-birth"
                              :prompt "When is this person's birthdate"
                              :link-to ("base-node")
                              :data-type DATETIME
                              :optional t)
                      '(:pkm-type kvd :name "person-relation" :key "person-relation"
                              :prompt "What is their relation to me?"
                              :choices ("sister" "friend" "family" "work")
                              :allow-additional-choices t
                              :link-to ("base-node")
                              :optional t
                              :data-type TEXT))))






(provide 'pkm2-object-registrations)
