;;; pkm-browse-eieio.el --- pkm browse implementation using eieio classes -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jinder
;;
;; Author: Jinder <jinder1s@gmail.com>
;; Maintainer: Jinder <jinder1s@gmail.com>
;; Created: April 05, 2024
;; Modified: April 05, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/msingh15/pkm-browse-eieio
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:





(cl-defmethod pkm-browse-insert-eieio ((section pkm-browse-section) &optional buffer-name)
  (when (not pkm2-browse-ewoc) (error (format "No ewoc, %S" (buffer-name) )))
  (let* ((nodes-spec (oref section :spec))
         (show-as-hierarchy (oref section :show-as-hierarchy))
         (pkm-nodes (--> (pkm2--browse-get-section-nodes-db-ids nodes-spec)
                         (-map #'pkm2--db-query-get-node-with-id it)))
         (sorter (oref section :sorter))
         (browse-nodes (when pkm-nodes (cond (show-as-hierarchy
                                              (--> (pkm2-browse-hierarchy-organize-as-hierarchy2 pkm-nodes)
                                                   (-map (lambda (branch)
                                                           (pkm2-convert-pkm-nodes-tree-to-browse-nodes-tree branch section 0)) it)))
                                             (t (-map (lambda (pkm-node)
                                                        (pkm-browse-node :datum pkm-node :section section :level 0))
                                                      pkm-nodes))) ))
         (sorted-sorted-nodes  (if sorter
                                   (pkm2-browse-sort-browse-nodes browse-nodes sorter)
                                 browse-nodes) )
         (section-id (or (oref  section section-id)
                         (oset section :section-id (pkm-random-id))))
         (section-start-ewoc (when pkm-nodes (or
                                              (oref section :start-ewoc)
                                              (ewoc-enter-last pkm2-browse-ewoc (format "---%s---" (or (oref section :name) "section") )) ) ))
         (section-end-ewoc (when pkm-nodes (or
                                            (oref section :end-ewoc)
                                            (ewoc-enter-after pkm2-browse-ewoc section-start-ewoc "\n")) )))
    (-each sorted-sorted-nodes (lambda (b-n)
                                 (pkm2-browse--insert-browse-node b-n `(before . ,section-end-ewoc))))

    (oset section :start-ewoc section-start-ewoc)
    (oset section :end-ewoc section-end-ewoc)
    (setq pkm2-browse--browse-sections-alist (assoc-delete-all section-id pkm2-browse--browse-sections-alist) )
    (push (cons section-id section) pkm2-browse--browse-sections-alist)))
(cl-defmethod pkm-browse-insert-eieio ((p-node pkm-node) section-id &optional buffer-name))
(cl-defmethod pkm-browse-insert-eieio ((c-node captured-node) section-id &optional buffer-name))
(cl-defmethod pkm-browse-insert-eieio ((c-object-schema captured-compiled-object-schema) section-id &optional buffer-name))


(cl-defmethod pkm-browse-remove-eieio ((p-node pkm-node) section-id &optional buffer-name))
(cl-defmethod pkm-browse-remove-eieio ((c-node captured-node) section-id &optional buffer-name))
(cl-defmethod pkm-browse-remove-eieio ((c-object-schema captured-compiled-object-schema) section-id &optional buffer-name))
(cl-defmethod pkm-browse-remove-eieio ((section pkm-browse-section) &optional buffer-name))


(cl-defmethod pkm-browse-insert-eieio ((browse-buffer pkm-browse-buffer) &optional buffer-name)
  (let* ((objects (oref browse-buffer :objects)))
    (-each objects (lambda (object) (pkm-browse-insert-eieio object buffer-name buffer-name)))))

(cl-defmethod pkm-browse-insert-eieio ((browse-object pkm-browse-object) &optional buffer-name)
  (let* ((datum (oref browse-object :datum))
         (browse-ewoc (with-current-buffer buffer-name pkm2-browse-ewoc))
         (object-id (oref browse-object :browse-id)))
    ))


(cl-defmethod pkm-browse-insert-eieio ((section pkm-browse-section) where &optional options buffer-name))
(cl-defmethod pkm-browse-insert-eieio ((p-node pkm-node) where &optional options buffer-name))
(cl-defmethod pkm-browse-insert-eieio ((c-node captured-node) where &optional options buffer-name))
(cl-defmethod pkm-browse-insert-eieio ((c-object-schema captured-compiled-object-schema) where &optional options buffer-name))
(cl-defmethod pkm-browse-insert-eieio ((spec pkm-browse-spec) where &optional options buffer-name))
(cl-defmethod pkm-browse-insert-eieio ((s-spec pkm-section-spec) where &optional options buffer-name))
(cl-defmethod pkm-browse-insert-eieio ((q-spec pkm-query-spec) where &optional options buffer-name))


(cl-defmethod pkm-browse-print-eieio ((browse-buffer pkm-browse-buffer) &optional buffer-name))
(cl-defmethod pkm-browse-print-eieio ((browse-object pkm-browse-object) &optional buffer-name))
(cl-defmethod pkm-browse-print-eieio ((section pkm-browse-section) where &optional options buffer-name))
(cl-defmethod pkm-browse-print-eieio ((p-node pkm-node) where &optional options buffer-name))
(cl-defmethod pkm-browse-print-eieio ((c-node captured-node) where &optional options buffer-name))
(cl-defmethod pkm-browse-print-eieio ((c-object-schema captured-compiled-object-schema) where &optional options buffer-name))
(cl-defmethod pkm-browse-print-eieio ((spec pkm-browse-spec) where &optional options buffer-name))
(cl-defmethod pkm-browse-print-eieio ((s-spec pkm-section-spec) where &optional options buffer-name))
(cl-defmethod pkm-browse-print-eieio ((q-spec pkm-query-spec) where &optional options buffer-name))

(cl-defmethod pkm-browse-remove-eieio ((object pkm-browse-object) &optional buffer-name))

(cl-defmethod pkm-browse-promote-eieio ((b-o pkm-browse-object)))
(cl-defmethod pkm-browse-promote-eieio ((p-n pkm-node)))
(cl-defmethod pkm-browse-promote-eieio ((c-n captured-node)))
(cl-defmethod pkm-browse-promote-eieio ((c-object-schema captured-compiled-object-schema) ))

(cl-defmethod pkm-browse-edit-eieio ((b-o pkm-browse-object)))
(cl-defmethod pkm-browse-edit-eieio ((p-n pkm-node)))
(cl-defmethod pkm-browse-edit-eieio ((c-n captured-node)))
(cl-defmethod pkm-browse-edit-eieio ((c-object-schema captured-compiled-object-schema) ))
(cl-defmethod pkm-browse-edit-eieio ((b-spec pkm-browse-spec)))
(cl-defmethod pkm-browse-edit-eieio ((s-spec pkm-section-spec)))
(cl-defmethod pkm-browse-edit-eieio ((q-spec pkm-query-spec)))

(cl-defmethod pkm-browse-demote-eieio ((b-o pkm-browse-object)))
(cl-defmethod pkm-browse-demote-eieio ((p-n pkm-node)))
(cl-defmethod pkm-browse-demote-eieio ((c-n captured-node)))
(cl-defmethod pkm-browse-demote-eieio ((c-object-schema captured-compiled-object-schema) ))


(cl-defmethod pkm-browse-move-down-eieio ((b-o pkm-browse-object)))
(cl-defmethod pkm-browse-move-down-eieio ((p-n pkm-node)))
(cl-defmethod pkm-browse-move-down-eieio ((c-n captured-node)))
(cl-defmethod pkm-browse-move-down-eieio ((c-object-schema captured-compiled-object-schema) ))

(cl-defmethod pkm-browse-move-up-eieio ((b-o pkm-browse-object)))
(cl-defmethod pkm-browse-move-up-eieio ((p-n pkm-node)))
(cl-defmethod pkm-browse-move-up-eieio ((c-n captured-node)))
(cl-defmethod pkm-browse-move-up-eieio ((c-object-schema captured-compiled-object-schema) ))

(provide 'pkm-browse-eieio)
;;; pkm-browse-eieio.el ends here
