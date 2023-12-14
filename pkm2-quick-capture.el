;;; pkm/pkm2-log.el -*- lexical-binding: t; -*-
(require 'pkm2-clock)

(defvar pkm2--capture-parent-nodes-candidates ())

(defun pkm2-log-into-active-clock ()
  (interactive)
  (pkm2-quick-capture-into-active-clock 'log-n))


(defun pkm2-note-into-active-clock ()
  (interactive)
  (pkm2-quick-capture-into-active-clock 'note-n))

(defun pkm2-idea-note-into-active-clock ()
  (interactive)
  (pkm2-quick-capture-into-active-clock 'idea-note-n))

(defun pkm2-quick-capture-into-active-clock (&optional structure-name  link-type)
  (interactive)
  (if-let* ((active-clocked-nodes (pkm2-clock--get-current-clock-parent-pkm-nodes))
            (clocked-node
             (when active-clocked-nodes
               (if (length> active-clocked-nodes 1)
                   (let* ((completing-read-choices
                           (-map
                            (lambda (a-c-p)
                              (--> (pkm2-node-db-node a-c-p)
                                   (pkm2-db-node-content it)
                                   (cons it a-c-p)))
                            active-clocked-nodes))
                          (choice (when completing-read-choices
                                    (--> (completing-read
                                          (format "Where would you like to add a %s?" (symbol-name structure-name))
                                          completing-read-choices)
                                         (assoc-default it completing-read-choices)))))
                     choice)
                 (car active-clocked-nodes))))

            (clocked-node-id (--> (pkm2-node-db-node clocked-node) (pkm2-db-node-id it)))
            (link-type (or link-type "sub")))
      (pkm--object-capture-sub clocked-node-id structure-name link-type)
    (message "No active clocked-node")))


(defun pkm2-quick-capture-into-search-node ( &optional structure-name link-type)
  (interactive)
  (if-let* ((node-db-id (pkm2-nodes-search (format "Where would you like to add a %s?" (symbol-name structure-name)))))
      (pkm--object-capture-sub node-db-id structure-name link-type)))

(defun pkm2-quick-capture-into-candidates (&optional structure-name link-type)
  (if pkm2--capture-parent-nodes-candidates
      (let* ((choice-node
              (if (length> pkm2--capture-parent-nodes-candidates 1)
                  (let* ((completing-read-choices
                          (-map
                           (lambda (a-c-p)
                             (--> (pkm2-node-db-node a-c-p)
                                  (pkm2-db-node-content it)
                                  (cons it a-c-p)))
                           pkm2--capture-parent-nodes-candidates))
                         (choice (when completing-read-choices
                                   (--> (completing-read
                                         (format "Where would you like to add a %s?" (symbol-name structure-name))
                                         completing-read-choices)
                                        (assoc-default it completing-read-choices)))))
                    choice)
                (car pkm2--capture-parent-nodes-candidates)))
             (node-db-id (--> (pkm2-node-db-node choice-node) (pkm2-db-node-id it)))
             (link-type (or link-type "sub")))
        (pkm--object-capture-sub node-db-id structure-name link-type))
    (message "No capture cadidates, please set capture candidate")))

(provide 'pkm2-quick-capture)
