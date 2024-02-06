;;; pkm-create-pkm-query-ewoc.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jinder
;;
;; Author: Jinder <jinder1s@gmail.com>
;; Maintainer: Jinder <jinder1s@gmail.com>
;; Created: December 09, 2023
;; Modified: December 09, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/msingh15/pkm-create-pkm-query-ewoc
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'dash)
(require 'ewoc)
(require 'pkm-create-pkm-query)
(defun pkm-create-pkm-query-ewoc-pretty-print-filter (subquery)
  (let* ((start-section  (nth 0 subquery))
         (start-query  (nth 1 subquery))
         (start-subquery (nth 2 subquery))
         (filter (nth 3 subquery)))
    (when (and  start-query  start-subquery )
      (unless start-section
        (insert "\n"))
      (insert  "Section"))
    (when (and start-subquery)
      (insert  "\nquery: "))
    (unless (and start-subquery)
      (insert  "\n       "))
    (insert (format "%S" filter))))
(defvar-local pkm-create-pkm-query-ewoc-ewoc nil)
(defvar-local pkm-create-pkm-query-ewoc-finish-func nil)
(defvar-local pkm-create-pkm-query-ewoc-cancel-finish-func nil)

(defvar-keymap pkm-create-pkm-query-ewoc-mode-map
  :doc ""
  "C-c C-c" (lambda () (interactive) (funcall pkm-create-pkm-query-ewoc-finish-func))
  "C-c C-k" (lambda () (interactive) (funcall pkm-create-pkm-query-ewoc-cancel-finish-func))
  "a b q" #'pkm-compile-add-query-before
  "a b s" #'pkm-compile-add-section-before
  "a b f" #'pkm-compile-add-filter-before
  "a a q" #'pkm-compile-add-query-after
  "a a s" #'pkm-compile-add-section-after
  "a a f" #'pkm-compile-add-filter-after
  "d f" #'pkm-compile-remove-current-filter
  "d q" #'pkm-compile-remove-current-query
  "d s" #'pkm-compile-remove-current-section)

(setq pkm-create-pkm-query-ewoc-mode-map
      (define-keymap
        ;; :doc ""
        "C-c C-c" (lambda () (interactive) (funcall pkm-create-pkm-query-ewoc-finish-func))
        "C-c C-k" (lambda () (interactive) (funcall pkm-create-pkm-query-ewoc-cancel-finish-func))
        "C-c a b q" #'pkm-compile-add-query-before
        "C-c a b s" #'pkm-compile-add-section-before
        "C-c a b f" #'pkm-compile-add-filter-before
        "C-c a a q" #'pkm-compile-add-query-after
        "C-c a a s" #'pkm-compile-add-section-after
        "C-c a a f" #'pkm-compile-add-filter-after
        "C-c d f" #'pkm-compile-remove-current-filter
        "C-c d q" #'pkm-compile-remove-current-query
        "C-c d s" #'pkm-compile-remove-current-section
        "C-c e" #'pkm-compile-manually-edit-current-filter))
(define-minor-mode pkm2-query-compile-mode
  "Minor mode for simple finish/cancel keybindings."
  :keymap pkm-create-pkm-query-ewoc-mode-map)

(defun pkm-compile-create-browse-spec (post-create-function &optional query section)
  (let* ((buffer-name "* p b com *")
         (buffer (get-buffer-create buffer-name))
         (browse-ewoc (with-current-buffer buffer (ewoc-create #'pkm-create-pkm-query-ewoc-pretty-print-filter nil nil t) ))
         (seed-query (when (not section) (or query (pkm2--create-query-filter :or) ) ))
         (data (if section
                   `(:sections (,section))
                 `(:sections ((:queries ((,seed-query))))) ))
         (ewoc-data (-flatten-n 2
                                (-map-indexed
                                 (lambda (s-i section)
                                   (-map-indexed
                                    (lambda (q-i query)
                                      (-map-indexed
                                       (lambda (sq-i sub-query)
                                         (list (equal s-i 0)
                                               (equal q-i 0)
                                               (equal sq-i 0)
                                               sub-query))
                                       query))
                                    (plist-get section :queries)))
                                 (plist-get data :sections)) )))
    (switch-to-buffer-other-window buffer)
    (with-current-buffer buffer
      (delete-region (point-min) (point-max))
      (setq pkm-create-pkm-query-ewoc-ewoc nil)
      (setq pkm-create-pkm-query-ewoc-ewoc browse-ewoc)
      (setq pkm-create-pkm-query-ewoc-finish-func
            (lambda ()
              (with-current-buffer buffer
                (let* ((browse-spec (pkm-compile-collect-queries)))
                  (kill-buffer buffer)
                  (funcall post-create-function browse-spec)))))
      (setq pkm-create-pkm-query-ewoc-cancel-finish-func
            (lambda ()
              (kill-buffer buffer-name)))
      (pkm2-query-compile-mode))
    (-each ewoc-data (lambda (filter) (ewoc-enter-last browse-ewoc filter)))))

(defun pkm-compile-add-filter-before (&optional filter)
  (interactive)
  (let* ((current-ewoc-node (ewoc-locate pkm-create-pkm-query-ewoc-ewoc (point)))
         (c-e-n-data (ewoc-data current-ewoc-node))
         (current-subquery-start (nth 2 c-e-n-data))
         (new-filter (or filter (pkm2--create-query-filter (when current-subquery-start :or)) ))
         (new-e-n-data (-replace-at 3 new-filter c-e-n-data)))
    (when current-subquery-start
      (let* ((new-c-e-n-data (-replace-at 2 nil c-e-n-data)))
        (ewoc-set-data current-ewoc-node new-c-e-n-data)
        (ewoc-invalidate pkm-create-pkm-query-ewoc-ewoc current-ewoc-node)))
    (ewoc-enter-before pkm-create-pkm-query-ewoc-ewoc current-ewoc-node new-e-n-data)))

(defun pkm-compile-add-filter-after (&optional filter)
  (interactive)
  (let* ((current-ewoc-node (ewoc-locate pkm-create-pkm-query-ewoc-ewoc (point)))
         (c-e-n-data (ewoc-data current-ewoc-node))
         (new-filter (or filter (pkm2--create-query-filter)))
         (new-e-n-data (-replace-at 3 new-filter c-e-n-data))
         (new-e-n-data (-replace-at 2 nil new-e-n-data)))
    (ewoc-enter-after pkm-create-pkm-query-ewoc-ewoc current-ewoc-node new-e-n-data)))

(defun pkm-compile-add-query-before (&optional node)
  (interactive)
(let* ((current-ewoc-node (or node (ewoc-locate pkm-create-pkm-query-ewoc-ewoc (point)) ))
       (c-e-n-data (ewoc-data current-ewoc-node))
       (current-subquery-start (nth 2 c-e-n-data))
       (current-query-start (nth 2 c-e-n-data)))
    (if current-subquery-start
        (let* ((new-filter (pkm2--create-query-filter :or) )
               (new-e-n-data (-replace-at 3 new-filter c-e-n-data)))
          (ewoc-enter-before pkm-create-pkm-query-ewoc-ewoc current-ewoc-node new-e-n-data)
          (when current-query-start
            (let* ((new-c-e-n-data (-replace-at 1 nil c-e-n-data)))
              (ewoc-set-data current-ewoc-node new-c-e-n-data)
              (ewoc-invalidate pkm-create-pkm-query-ewoc-ewoc current-ewoc-node))))
      (pkm-compile-add-query-before (ewoc-prev pkm-create-pkm-query-ewoc-ewoc current-ewoc-node)))))

(defun pkm-compile-add-query-after (&optional node)
  (interactive)
  (let* ((current-ewoc-node (or node (ewoc-locate pkm-create-pkm-query-ewoc-ewoc (point)) ))
         (c-e-n-data (ewoc-data current-ewoc-node))
         (next-ewoc-node (ewoc-next pkm-create-pkm-query-ewoc-ewoc current-ewoc-node))
         (n-e-n-data (when next-ewoc-node (ewoc-data next-ewoc-node) ))
         (next-subquery-start (nth 2 n-e-n-data)))
    (if (or (not next-ewoc-node) next-subquery-start)
        (let* ((new-filter (pkm2--create-query-filter :or))
               (new-e-n-data `(,(nth 0 c-e-n-data) ,(nth 0 c-e-n-data) t ,new-filter)))
          (ewoc-enter-after pkm-create-pkm-query-ewoc-ewoc current-ewoc-node new-e-n-data))
      (pkm-compile-add-query-after next-ewoc-node))))

(defun pkm-compile-add-section-before (&optional node)
  (interactive)
  (let* ((current-ewoc-node (or node (ewoc-locate pkm-create-pkm-query-ewoc-ewoc (point)) ))
         (c-e-n-data (ewoc-data current-ewoc-node))
         (current-subquery-start (nth 2 c-e-n-data))
         (current-query-start (nth 1 c-e-n-data)))
    (if (and current-subquery-start current-query-start)
        (let* ((new-filter (pkm2--create-query-filter :or) )
               (new-e-n-data `(,(nth 0 c-e-n-data) t t ,new-filter)))
          (ewoc-enter-before pkm-create-pkm-query-ewoc-ewoc current-ewoc-node new-e-n-data))
      (pkm-compile-add-section-before (ewoc-prev pkm-create-pkm-query-ewoc-ewoc current-ewoc-node)))))

(defun pkm-compile-add-section-after (&optional node)
  (interactive)
  (let* ((current-ewoc-node (or node (ewoc-locate pkm-create-pkm-query-ewoc-ewoc (point)) ))
         (next-ewoc-node (ewoc-next pkm-create-pkm-query-ewoc-ewoc current-ewoc-node))
         (n-e-n-data (when next-ewoc-node (ewoc-data next-ewoc-node)))
         (next-subquery-start (nth 2 n-e-n-data))
         (next-query-start (nth 1 n-e-n-data)))
    (if (or (not next-ewoc-node)
            (and next-subquery-start next-query-start) )
        (let* ((new-filter (pkm2--create-query-filter :or) )
               (new-e-n-data `(,(nth 0 n-e-n-data) t t ,new-filter)))
          (ewoc-enter-after pkm-create-pkm-query-ewoc-ewoc current-ewoc-node new-e-n-data))
      (pkm-compile-add-section-after next-ewoc-node))))

(defun pkm-compile-manually-edit-current-filter ()
  (interactive)
  (let* ((current-ewoc-node (ewoc-locate pkm-create-pkm-query-ewoc-ewoc (point)))
         (c-e-n-data (ewoc-data current-ewoc-node))
         (filter-string (format "%S" (nth 3 c-e-n-data) ))
         (edited-filter (read (read-string "Edit: " filter-string) ))
         (new-c-e-n-data (-replace-at 3 edited-filter c-e-n-data)))
    (ewoc-set-data current-ewoc-node new-c-e-n-data)
    (ewoc-invalidate pkm-create-pkm-query-ewoc-ewoc current-ewoc-node)))

(defun pkm-compile-remove-current-filter ()
  (interactive)
  (ewoc-delete pkm-create-pkm-query-ewoc-ewoc (ewoc-locate pkm-create-pkm-query-ewoc-ewoc (point))))
(defun pkm-compile-remove-current-query ()
  (interactive))
(defun pkm-compile-remove-current-section ()
  (interactive)
  )
(defun pkm-compile-move-filter-up ()
  (let* ((current-ewoc-node (ewoc-locate pkm-create-pkm-query-ewoc-ewoc (point)))
         (c-e-n-data (ewoc-data current-ewoc-node))
         (prev-ewoc-node (ewoc-prev pkm-create-pkm-query-ewoc-ewoc current-ewoc-node))
         (p-e-n-data (when prev-ewoc-node (ewoc-data prev-ewoc-node)))
         (current-subquery-start (nth 2 c-e-n-data))
         (prev-subquery-start (nth 2 p-e-n-data)))
    (if current-subquery-start
        (message "can't do that")
      (if prev-subquery-start
          (let* ((new-c-e-n-data (-replace-at 2 prev-subquery-start c-e-n-data))
                 (new-p-e-n-data (-replace-at 2 current-subquery-start p-e-n-data)))
            (ewoc-set-data current-ewoc-node new-p-e-n-data)
            (ewoc-set-data prev-ewoc-node new-c-e-n-data)
            (ewoc-invalidate pkm-create-pkm-query-ewoc-ewoc current-ewoc-node prev-ewoc-node))
        (progn (ewoc-delete pkm-create-pkm-query-ewoc-ewoc current-ewoc-node)
               (ewoc-enter-before pkm-create-pkm-query-ewoc-ewoc prev-ewoc-node c-e-n-data))))))

(defun pkm-compile-move-filter-down ()
  (let* ((current-ewoc-node (ewoc-locate pkm-create-pkm-query-ewoc-ewoc (point)))
         (c-e-n-data (ewoc-data current-ewoc-node))
         (next-ewoc-node (ewoc-next pkm-create-pkm-query-ewoc-ewoc current-ewoc-node))
         (n-e-n-data (when next-ewoc-node (ewoc-data next-ewoc-node)))
         (current-subquery-start (nth 2 c-e-n-data))
         (next-subquery-start (nth 2 n-e-n-data)))
    (if (or (not next-ewoc-node) next-subquery-start )
        (message "Can't do that")
      (if current-subquery-start
          (let* ((new-c-e-n-data (-replace-at 2 next-subquery-start c-e-n-data))
                 (new-n-e-n-data (-replace-at 2 current-subquery-start n-e-n-data)))
            (ewoc-set-data current-ewoc-node new-n-e-n-data)
            (ewoc-set-data next-ewoc-node new-c-e-n-data)
            (ewoc-invalidate pkm-create-pkm-query-ewoc-ewoc current-ewoc-node next-ewoc-node))
        (progn (ewoc-delete pkm-create-pkm-query-ewoc-ewoc current-ewoc-node)
               (ewoc-enter-after pkm-create-pkm-query-ewoc-ewoc next-ewoc-node c-e-n-data))))))

(defun pkm-compile-collect-queries ()
  (let*  ((ewoc-data-output (ewoc-collect pkm-create-pkm-query-ewoc-ewoc (lambda (node) t)))
          (output (list :output nil :current-section nil :current-query nil))
          (combined-output (-reduce-from (lambda (output subquery)
                                           (message "output: %S\nsq; %S" output subquery )
                                           (let* ((start-section  (nth 0 subquery))
                                                  (start-query  (nth 1 subquery))
                                                  (start-subquery (nth 2 subquery))
                                                  (filter (nth 3 subquery)))
                                             (when start-subquery
                                               (when (plist-get output :current-query)
                                                 (let* ((reduced-query (plist-get output :current-query))
                                                        (reduced-section (plist-get output :current-section))
                                                        (current-section-queries (plist-get reduced-section :queries) )
                                                        (new-section-queries (-concat current-section-queries (list reduced-query)))
                                                        (new-reduced-section (plist-put reduced-section :queries new-section-queries))
                                                        (new-output (plist-put output :current-section new-reduced-section))
                                                        (new-output (plist-put new-output :current-query nil)))
                                                   (message "New subquery, o: %S" new-output)
                                                   (setq output new-output)) ) )
                                             (when (and start-query start-subquery)
                                               (unless start-section
                                                 (let* ((reduced-section (plist-get output :current-section))
                                                        (reduced-output (plist-get output :output))
                                                        (current-output-sections (plist-get reduced-output :sections))
                                                        (new-output-sections (-concat current-output-sections (list reduced-section)))
                                                        (new-reduced-output (plist-put reduced-output :sections new-output-sections))
                                                        (new-output (plist-put output :output new-reduced-output))
                                                        (new-output (plist-put new-output :current-section nil)))
                                                   (message "New query, o: %S" new-output)
                                                   (setq output new-output)) ))
                                             (let* ((current-reduced-query (plist-get output :current-query))
                                                    (new-reduced-query (-concat current-reduced-query (list filter))))
                                               (setq output (plist-put output :current-query new-reduced-query)))
                                             output))
                                         output
                                         ewoc-data-output))
          (browse-spec (let* ((output combined-output))
                         (let* ((reduced-query (plist-get output :current-query))
                                (reduced-section (plist-get output :current-section))
                                (current-section-queries (plist-get reduced-section :queries) )
                                (new-section-queries (-concat current-section-queries (list reduced-query)))
                                (new-reduced-section (plist-put reduced-section :queries new-section-queries))
                                (new-output (plist-put output :current-section new-reduced-section))
                                (new-output (plist-put new-output :current-query nil)))
                           (setq output new-output))
                         (let* ((reduced-section (plist-get output :current-section))
                                (reduced-output (plist-get output :output))
                                (current-output-sections (plist-get reduced-output :sections))
                                (new-output-sections (-concat current-output-sections (list reduced-section)))
                                (new-reduced-output (plist-put reduced-output :sections new-output-sections))
                                (new-output (plist-put output :output new-reduced-output))
                                (new-output (plist-put new-output :current-section nil)))
                           (setq output new-output))
                         (plist-get output :output))))
    browse-spec))

(provide 'pkm-create-pkm-query-ewoc)
;;; pkm-create-pkm-query-ewoc.el ends here
