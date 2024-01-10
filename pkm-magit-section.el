;;; pkm-magit-section.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jinder
;;
;; Author: Jinder <jinder1s@gmail.com>
;; Maintainer: Jinder <jinder1s@gmail.com>
;; Created: January 10, 2024
;; Modified: January 10, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/msingh15/pkm-magit-section
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'magit-section)

(defun pkm-magit-section-delete-section-at-point ()
  (let* ((current-section (magit-current-section))
         (parent (oref current-section parent))
         (parent-children (oref parent children))
         (new-parent-children (-filter (lambda (child)
                                         (not (eq child current-section)))
                                       parent-children))
         (start (oref current-section start))
         (end (oref current-section end))
         (inhibit-read-only t))
    (oset parent children new-parent-children)
    (delete-region start end)))

(defun pkm-magit-section-insert-new-section ()
  (let* ((current-section (magit-current-section))
         (parent (oref current-section parent))
         (children (oref parent children))
         (elem-index (-elem-index current-section children))
         (end (when current-section
                (oref current-section end) ))
         (inhibit-read-only t)
         (section (progn
                    (goto-char end)
                    (magit-insert-section (magit-section "Damn")
                      (magit-insert-heading (insert "heading"))
                      (magit-insert-section-body (insert "body\n")))))
         (new-children (-insert-at (+ elem-index 1) section children)))
    (oset section parent parent)
    (oset parent children new-children)
    "haha"))

(defun pkm-magit-section-get-node-at-point ()
  (let* ((current-section (magit-current-section)))
    (oref current-section value)))

(provide 'pkm-magit-section)
;;; pkm-magit-section.el ends here
