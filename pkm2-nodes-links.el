;;; pkm2-nodes-links.el -*- lexical-binding: t; -*-

(defvar pkm-links-types '(HIERARCHICAL SEQUENCAL FLAT))
(defvar pkm-links-label-to-type-equal-plist ())

(defvar pkm-links-type-to-label-eq-plist ())


(defun pkm2--register-link-label (link-type label)
  (unless (member link-type pkm-links-types) (error "%S is not a accepted link type. Needs to be one of following %S" link-type pkm-links-types))
  (setq pkm-links-label-to-type-equal-plist (plist-put pkm-links-label-to-type-equal-plist label link-type #'equal))
  (setq pkm-links-type-to-label-eq-plist
        (plist-put pkm-links-type-to-label-eq-plist link-type
                   (-distinct (-concat (list label) (plist-get pkm-links-type-to-label-eq-plist link-type))))))

(defun pkm2--link-get-link-parent-id (link)
  (pkm2-db-nodes-link-node_a link))
(defun pkm2--link-get-link-child-id (link)
  (pkm2-db-nodes-link-node_b link))


(pkm2--register-link-label 'HIERARCHICAL "sub")
(pkm2--register-link-label 'HIERARCHICAL "related")
(pkm2--register-link-label 'HIERARCHICAL "clock")
(pkm2--register-link-label 'HIERARCHICAL "type-of")
