;;; test-lister.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jinder
;;
;; Author: Jinder <jinder1s@gmail.com>
;; Maintainer: Jinder <jinder1s@gmail.com>
;; Created: April 24, 2024
;; Modified: April 24, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/msingh15/test-lister
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(cl-defmethod pkm-get-string-representation ((p-node pkm-node) browse-node)
  (let* ((browse-insert-format-string
          (or
           (-->
            (oref p-node :types)
            (-map (lambda (type)
                    (let* ((schema (plist-get pkm-structure-defined-schemas-plist type) )
                           (parents (plist-get schema :parents))
                           (f-s (plist-get schema :browse-insert-format-string)))
                      (when f-s
                        (list type parents f-s))))
                  it)
            (-non-nil it)
            (-distinct it)
            (-filter (lambda (info)
                       (unless (member (car info) (-flatten (-map #'cadr it)))
                         t))
                     it)
            (progn (when (length> it 1)
                     (display-warning 'pkm2-browse (format "Node has multiple insert funcs: %S"   it )))
                   (car it))
            (-last-item it))
           "<insert>(:display content)</insert>"))
         (base-string (pkm2--browse-format-insert browse-insert-format-string browse-node)))
    base-string))

(defun pkm-lister-browse--insert-ewoc-item (ewoc-item)
  (cond ((assoc-default ewoc-item pkm2-browse--browse-nodes-alist)
         (let* ((browse-node (assoc-default ewoc-item pkm2-browse--browse-nodes-alist))
                (level (oref browse-node :level))
                (prefix2 (concat (propertize " " 'display `(space :width ,(*  2 (abs (or level 0)))))))

                (datum (oref browse-node :datum))
                (base-string (pkm-get-string-representation datum browse-node))
                (output (string-split (propertize base-string 'field t 'inhibit-read-only t) "\n" )))
           (message "Output: %S" output)
           output))
        ((stringp ewoc-item) ewoc-item)) )

(defun test-lister ()
  (let*  ((buffer-name "TEST")
          (ewoc (lister-setup buffer-name (lambda (item)
                                            (let* ((output (pkm-lister-browse--insert-ewoc-item item)))

                                              (message "Output: %S" output)
                                              (list output ))) )))
    (with-current-buffer buffer-name
      (lister-set-list ewoc '("ITEM1" ("ITEM2" ("ITEM3")))))
    (display-buffer-same-window (get-buffer-create buffer-name) nil)))

(defun pkm2--lister-browse (buffer-state &optional buffer-name)
  (message "lister browse: %S" buffer-name)
  (with-current-buffer (get-buffer-create buffer-name )
    (erase-buffer)
    (setq pkm2-browse-ewoc nil)
    (let* ((ewoc (lister-setup buffer-name #'pkm-lister-browse--insert-ewoc-item)))
      (message "After ewoc creation")
      (setq pkm2-browse-ewoc ewoc)
      (setq pkm2-browse--browse-nodes-alist ())
      (setq pkm2-browse--browse-sections-alist ())
      (setq pkm2-browse-buffer-states-equal-plist (plist-put pkm2-browse-buffer-states-equal-plist buffer-name buffer-state #'equal))
      (-each
          (oref buffer-state :sections)
        #'pkm2-lister-browse--insert-section)
      (persp-add-buffer  buffer-name)
      (display-buffer-same-window (get-buffer-create buffer-name) nil))))

(defun pkm2-lister-browse--insert-section (section)
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
         (browse-node-lister  (-map  #'pkm2-convert-pkm-nodes-tree-to-browse-nodes-tree-lister browse-nodes) )
         (final_list (list "section" browse-node-lister)))
    (message "Final list: %S" final_list)
    (lister-set-list pkm2-browse-ewoc final_list)))

(defun pkm2-convert-pkm-nodes-tree-to-browse-nodes-tree-lister (browse-node)
  (let* ((children (oref browse-node :children-ids))
         (parents (oref browse-node :parents-ids))
         (current-id (oref browse-node :browse-id))
         (children-nodes-list (-map (lambda (child-id)
                                      (pkm2-convert-pkm-nodes-tree-to-browse-nodes-tree-lister (assoc-default child-id pkm2-browse--browse-nodes-alist) ))
                                    children))
         (parents-nodes-list (-map (lambda (parent-id)
                                     (pkm2-convert-pkm-nodes-tree-to-browse-nodes-tree-lister (assoc-default parent-id pkm2-browse--browse-nodes-alist) ))
                                   parents))
         (output (-non-nil (list current-id children-nodes-list parents-nodes-list))))
    (message "current output: %S" output)
    output))

(provide 'test-lister)
;;; test-lister.el ends here
