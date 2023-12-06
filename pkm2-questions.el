;;; pkm/pkm2-questions.el -*- lexical-binding: t; -*-

(pkm-register-structure 'answer-n
  (list :parent 'base-n
        :assets (list
                 '(:pkm-type kvd :name "is-answer" :key "node-type" :value "answer" :link-to ("base-node") :data-type TEXT))))

(pkm-register-structure 'question-n
 (list :parent 'base-n
       :assets (list
                '(:pkm-type kvd :name "is-question" :key "node-type" :value "question" :link-to ("base-node") :data-type TEXT))))

(pkm-register-structure 'flash-card-s
                        (list
                         :assets (list
                                  '(:pkm-type answer-n :name "back")
                                  '(:pkm-type question-n :name "front" :primary-node t)
                                  '(:pkm-type kvd :name "is-flash-card" :key "node-type" :value "flash-card" :link-to (primary) :data-type TEXT))
                         :links (list
                                 '(:pkm-link-label "sub" :child "back" :parent "front"))))
