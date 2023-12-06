;;; pkm2-hierarchy.el -*- lexical-binding: t; -*-


(defun pkm2-browse-hierarchy-organize-as-hierarchy (browse-nodes)
  (let* ((pkm-nodes (-map #'pkm2-browse-node-pkm-node browse-nodes))
         (parent-links (-map #'pkm2-node-parent-links pkm-nodes))
         (parent-db-ids-list (-map (lambda (p-links)
                                     (-map #'pkm2-db-nodes-link-node_a p-links)) parent-links))
         (children-links (-map #'pkm2-node-children-links pkm-nodes))
         (children-db-ids-list (-map (lambda (c-links)
                                       (-map #'pkm2-db-nodes-link-node_b c-links)) children-links))
         (db-nodes (-map #'pkm2-node-db-node pkm-nodes))
         (db-ids (-map #'pkm2-db-node-id db-nodes))
         (parent-less-nodes (--> (-map-indexed (lambda (index p-ids)
                                                 (cond ((not p-ids) index)
                                                       ((not (-any? (lambda (p-id)
                                                                      (member p-id db-ids)) p-ids))
                                                        index)))
                                               parent-db-ids-list)
                                 (-non-nil it)
                                 (-map (lambda (index)
                                         (nth index browse-nodes)) it)))
         (present-children-indexes (-map (lambda (c-ids)
                                                (-non-nil
                                                 (-map (lambda (c-id)
                                                         (-elem-index c-id db-ids))
                                                       c-ids)))
                                              children-db-ids-list)))
    (-each parent-less-nodes (lambda (b-n)
                               (--> (pkm2-browse-node-state b-n) (setf (pkm2-browse-node-state-level it) 0))))
    (-each-indexed browse-nodes (lambda (index p-b-n)
                                  (when-let ((p-b-n-level (--> (pkm2-browse-node-state p-b-n) (pkm2-browse-node-state-level it)))
                                             (children-indexes-list  (nth index present-children-indexes))
                                             (children-browse-nodes (-map (lambda (index)
                                                                            (nth index browse-nodes))
                                                                          children-indexes-list))
                                             ;; Only modify child nodes that don't already have parent
                                             ;; In the future, I might want to duplicate here
                                             (c-b-n-with-no-parent (-filter (lambda (c-b-n)
                                                                              (not (pkm2-browse-node-parent c-b-n)))
                                                                            children-browse-nodes)))
                                    (setf (pkm2-browse-node-children p-b-n) c-b-n-with-no-parent)
                                    (-each c-b-n-with-no-parent (lambda (c-b-n) (setf (pkm2-browse-node-parent c-b-n) p-b-n)))
                                    (-each c-b-n-with-no-parent (lambda (c-b-n) (--> (pkm2-browse-node-state c-b-n) (setf (pkm2-browse-node-state-level it) (+ p-b-n-level 1))))))))
    parent-less-nodes))

(defun pkm2-browse-heirarchy-add-as-child-browse-node (parent child))


(defun pkm2-browse-heirarchy-goto-first-child (parent))
(defun pkm2-browse-heirarchy-goto-parent (child))
(defun pkm2-browse-see-child-nodes (parent levels))
(defun pkm2-browse-see-parent-nodes (child levels))
(defun pkm2-browse-refresh-tree (browse))
(defun pkm2-browse-get-tree-nodes (browse))


(provide 'pkm2-browse-hierarchy)
