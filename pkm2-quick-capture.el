;;; pkm/pkm2-log.el -*- lexical-binding: t; -*-


(defvar pkm2--capture-parent-nodes-candidates ())

(defun pkm2-quick-capture-into-search-node ( &optional structure-name link-type)
  (interactive)
  (if-let* ((node-db-id (pkm2-nodes-search (format "Where would you like to add a %s?" (symbol-name structure-name)))))
      (pkm--object-capture-sub-eieio node-db-id structure-name link-type)))

(defun pkm2-quick-capture-into-candidates (&optional structure-name link-type)
  (if pkm2--capture-parent-nodes-candidates
      (let* ((choice-node
              (if (length> pkm2--capture-parent-nodes-candidates 1)
                  (let* ((completing-read-choices
                          (-map
                           (lambda (a-c-p)
                             (--> (oref a-c-p :content)
                                  
                                  (cons it a-c-p)))
                           pkm2--capture-parent-nodes-candidates))
                         (choice (when completing-read-choices
                                   (--> (completing-read
                                         (format "Where would you like to add a %s?" (symbol-name structure-name))
                                         completing-read-choices)
                                        (assoc-default it completing-read-choices)))))
                    choice)
                (car pkm2--capture-parent-nodes-candidates)))
             (node-db-id  (pkm-get-db-id choice-node))
             (link-type (or link-type "sub")))
        (pkm--object-capture-sub-eieio node-db-id structure-name link-type))
    (message "No capture cadidates, please set capture candidate")))

(provide 'pkm2-quick-capture)
