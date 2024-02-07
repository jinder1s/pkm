;;; pkm/pkm2-questions.el -*- lexical-binding: t; -*-

(require 'pkm-new-core)
(require 'pkm-compile-db-query)
(pkm-register-structure 'answer-n
  (list :parent 'base-n
        :behaviors (list `(:name log :link-to (primary)))
        :assets (list
                 '(:pkm-type kvd :name "is-answer" :key "node-type" :value "answer" :link-to ("base-node") :data-type TEXT))))

(pkm-register-structure 'question-n
 (list :parent 'base-n
       :assets (list
                '(:pkm-type kvd :name "is-question" :key "node-type" :value "question" :link-to ("base-node") :data-type TEXT)
                `(:pkm-type kvd :name "answer-type" :key "answer-type" :choices ,(-map #'symbol-name (doom-plist-keys pkm2--key_value_data-type-to-table-plist) ) :link-to (primary) :data-type TEXT))))

(pkm-register-structure 'flash-card-s
                        (list
                         :assets (list
                                  '(:pkm-type answer-n :name "back")
                                  '(:pkm-type question-n :name "front" :primary-node t)
                                  '(:pkm-type kvd :name "is-flash-card" :key "node-type" :value "flash-card" :link-to (primary) :data-type TEXT))
                         :links (list
                                 '(:pkm-link-label "sub" :child "back" :parent "front"))))


(defun pkm-questions ()
  (interactive)
  (let* ((questions-pkm-query '((:or structure-type (:structure-name question-n))))
         (questions (pkm2-get-nodes-with-pkm-query questions-pkm-query))
         (questions-choices (-map (lambda (question)
                                    (cons (--> (pkm2-node-db-node question) (pkm2-db-node-content it))
                                          question))
                                  questions))
         (questions-strings (completing-read-multiple "What questions would you like to answer?" questions-choices))
         (questions-pkm-nodes (-map (lambda (q-s)
                                      (assoc-default q-s questions-choices))
                                    questions-strings))
         (answers (-map #'pkm-ques-get-answer questions-pkm-nodes)))
    (-each-indexed answers (lambda (index answer)
                     (pkm-ques-commit-answer (assoc-default (nth index questions-strings) questions-choices)
                                             answer
                                             (pkm2-get-current-timestamp))))))

(defun pkm-ques-get-answer (question-pkm-node)
  (let* ((content (--> (pkm2-node-db-node question-pkm-node) (pkm2-db-node-content it)))
         (answer-type (--> (pkm2-node-get-kvds-with-key question-pkm-node "answer-type")
                                   (when it
                                     (if (length> it 1)
                                         (error "question node has more than one answer-type")
                                       (read (pkm2-db-kvd-value (car it)) )))))
         (answer (pkm-read (format "%s: " content) answer-type)))
    answer))

(defun pkm-ques-commit-answer (question answer timestamp)
  (let* ((question-db-id (--> (pkm2-node-db-node question) (oref it :id)))
         (link-label "clock")
         (link-definition (list :pkm-link-label link-label
                                :parent 'parent
                                :child 'child))
         (structure-schema (list :name 'parent-child-node
                                 :assets (list
                                          `(:pkm-type node :name "parent-node" :parent-node t :db-id ,question-db-id)
                                          `(:pkm-type answer-n :name "child-node" :child-node t))
                                 :links (list link-definition)))
         (values `(("child-node" . (("base-node" . ,answer )
                                     ("timestamp" . ,timestamp))))))
    (pkm2--object-capture-object-verify
     structure-schema
     nil
     t
     values)))

(provide 'pkm2-questions)
