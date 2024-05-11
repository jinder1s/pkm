;;; pkm-create-pkm-query.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jinder
;;
;; Author: Jinder <jinder1s@gmail.com>
;; Maintainer: Jinder <jinder1s@gmail.com>
;; Created: December 30, 2023
;; Modified: December 30, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/manjindersingh/pkm-compile-db-query-old
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(require 'dash)
(require 'pkm-new-core)

(defvar pkm2--query-spec-options-plist ())

(defun pkm2--create-query (&optional  print-output initial-queries initial-action)
  (let* ((action-options '(:or :and :not :convert-or :convert-and))
         (prompts '(("Filter nodes down?" . :and)
                    ("Add nodes to current selection?" . :or)
                    ("Remove nodes" . :not)
                    ("Convert nodes to thier parents or children" . :convert-and)
                    ("Also get children or parents of current nodes." . :convert-or)
                    ("Done and name query" . "DONE-NAME")
                    ("I'm done" .  "DONE")))
         (action (or initial-action :or))
         (options (doom-plist-keys pkm2--query-spec-options-plist))
         (convert-options '("convert-to-parents" "convert-to-children" "convert-dependent-to-parent"))
         (query-spec (-copy initial-queries)))
    (while  (member action action-options)
      (message "action: %S" action)
      (when print-output (funcall print-output query-spec) )
      (setq query-spec (-as-> (cond ((equal action :or)
                                     (--> (completing-read "How would you like to select nodes to add to current selection?" options)
                                          (intern it)
                                          (list it (pkm--convert-into-get-spec it))))
                                    ((equal action :and)
                                     (--> (completing-read "How would you filter current nodes?" options)
                                          (intern it)
                                          (list it (pkm--convert-into-get-spec it))))
                                    ((equal action :not)
                                     (--> (completing-read "How would you remove nodes from current selection?" options)
                                          (intern it)
                                          (list it (pkm--convert-into-get-spec it))))
                                    ((equal action :convert-and)
                                     (--> (completing-read "How would you like to convert current selection?" convert-options)
                                          (intern it)
                                          (list it (pkm--convert-into-get-spec it))))
                                    ((equal action :convert-or)
                                     (--> (completing-read "How would you like to convert current selection?" convert-options)
                                          (intern it)
                                          (list it (pkm--convert-into-get-spec it))))
                                    ((not (equal action "DONE"))
                                     (error "Something weird happened")))
                              it2
                              (pkm2--create-query-2 action (nth 0 it2) (nth 1 it2))
                              (list it2)
                              (-concat query-spec it2)))
      (when print-output (funcall print-output query-spec) )
      (setq action (--> (completing-read "What would you like to do next?" prompts)
                        (assoc-default it prompts))))
    (if (equal action "DONE-NAME")
        (--> (read-string (format "Name for: %S" query-spec))
             (-concat pkm2-browse-saved-named-queries (list (cons it (format "%S" query-spec )) ))
             (setq pkm2-browse-saved-named-queries it))
      (--> (-concat pkm2-browse-saved-queries (list (format "%S" query-spec) ))
           (setq pkm2-browse-saved-queries it)))
    (persist-save 'pkm2-browse-saved-queries)
    (persist-save 'pkm2-browse-saved-named-queries)
    query-spec))


(defun pkm2--create-query-filter (&optional action)
  (let* ((action-options '(:or :and :not :convert-or :convert-and :name :combine-or :combine-and))
         (action (or action (intern (completing-read "What action would you like to do?" action-options) ) ))
         (options (doom-plist-keys pkm2--query-spec-options-plist))
         (convert-options '("convert-to-parents" "convert-to-children" "convert-dependent-to-parent"))
         (query-spec (-as-> (cond ((equal action :or)
                                   (--> (completing-read "How would you like to select nodes to add to current selection?" options)
                                        (intern it)
                                        (list it (pkm--convert-into-get-spec it))))
                                  ((equal action :and)
                                   (--> (completing-read "How would you filter current nodes?" options)
                                        (intern it)
                                        (list it (pkm--convert-into-get-spec it))))
                                  ((equal action :not)
                                   (--> (completing-read "How would you remove nodes from current selection?" options)
                                        (intern it)
                                        (list it (pkm--convert-into-get-spec it))))
                                  ((equal action :convert-and)
                                   (--> (completing-read "How would you like to convert current selection?" convert-options)
                                        (intern it)
                                        (list it (pkm--convert-into-get-spec it))))
                                  ((equal action :convert-or)
                                   (--> (completing-read "How would you like to convert current selection?" convert-options)
                                        (intern it)
                                        (list it (pkm--convert-into-get-spec it))))
                                  ((equal action :name)
                                   (list nil (list :name (read-string "Name for current selection: "))))
                                  ((not (equal action "DONE"))
                                   (error "Something weird happened")))
                            it2
                            (pkm2--create-query-2 action (nth 0 it2) (nth 1 it2)))))

    query-spec))

(defvar pkm2--query-spec-options-plist ())

(defun pkm2--register-query-spec-option (spec-option-name inputs read-info-function convert-to-db-func  )
  (setq pkm2--query-spec-options-plist (plist-put pkm2--query-spec-options-plist spec-option-name (list :inputs inputs
                                                                                                        :read-info read-info-function
                                                                                                        :get-db-query convert-to-db-func ))))

(defun pkm2--create-query-2 (action query-type query-type-inputs)
  `(,action ,query-type ,query-type-inputs))

(defun pkm--convert-into-get-spec (key)
  (--> (plist-get pkm2--query-spec-options-plist key)
       (plist-get it :read-info)
       (funcall it)))


(defun pkm--convert-into-get-spec-empty ())


(defun pkm--convert-into-get-spec-structure-type ()
  (list :structure-name (completing-read "What structure-type?" (doom-plist-keys pkm-structure-2-undefined-schemas-plist)) ))


(defun pkm--convert-into-get-spec-between ()
  (let* ((what-to-get (completing-read "What would you like to set?" '("after" "before" "both")))
         (relative-time (y-or-n-p "Default is absolute time. Would you like to switch to relative time now?"))

         (after (when (or (equal "after" what-to-get) (equal "both" what-to-get))
                  (if relative-time
                      (let* ((relative-by (completing-read-multiple "How would you like to specify after?"
                                                                    '("hour" "day" "minute" "second" "week" "month")))
                             (units-amounts (-map (lambda (time-unit)
                                                    (cons (intern time-unit ) (read-number (format "Relative by %s?" time-unit)) )) relative-by))
                             output)

                        (-each units-amounts (lambda (unit-amount) (setq output (-concat output (list (car unit-amount) (cdr unit-amount))))))
                        output)
                      (--> (read-string "Time after:\n" (format-time-string "%FT%T%z" (current-time) ))
                           (date-to-time it)
                           (time-convert it 'integer)) )
                  ))
         (before (when (or (equal "before" what-to-get) (equal "both" what-to-get))
                   (if relative-time
                       (let* ((relative-by (completing-read-multiple "How would you like to specify before?"
                                                                     '("hour" "day" "minute" "second" "week" "month")))
                              (units-amounts (-map (lambda (time-unit)
                                                     (cons (intern time-unit ) (read-number (format "Relative by %s?" time-unit)) )) relative-by))
                              output)

                         (-each units-amounts (lambda (unit-amount) (setq output (-concat output (list (car unit-amount) (cdr unit-amount))))))
                         output)
                     (--> (read-string "Time before:\n" (format-time-string "%FT%T%z" (current-time) ))
                          (date-to-time it)
                          (time-convert it 'integer)) )
                   )))
    (list :after after :before before)))


(defun pkm--convert-into-get-spec-kvd (&optional what-to-get key keys)
  (let* ((what-to-get (or what-to-get (completing-read "How would you like to filter by kvd?" '("key" "key and value" "key and choices" "key and number range" "key and time range"))))
         (key (or key (completing-read "What key?" (or keys
                                                       (pkm2--db-query-get-all-keys-to-types-alist)) nil 'confirm) ))
         (type (assoc-default key (pkm2--db-query-get-all-keys-to-types-alist )))
         (value (when (equal what-to-get "key and value")
                  (completing-read "What value?" (pkm2--db-query-get-all-values-with-key key type) )))
         (choices (when (equal what-to-get "key and choices")
                    (completing-read-multiple "What choices?" (pkm2--db-query-get-all-values-with-key key type))))
         (what-to-get-for-range
          (when (equal what-to-get "key and number range")
            (completing-read "What would you like to set?" '("after" "before" "both"))))
         (what-to-get-for-time-range
          (when (equal what-to-get "key and time range")
            (completing-read "What would you like to set?" '("after" "before" "both" "after-relative" "both-relative" "before-relative"))))
         (after (when (and what-to-get-for-range (or (equal "after" what-to-get-for-range) (equal "both" what-to-get-for-range)) )
                  (read-number "number greater than:\n" )))
         (before (when (and what-to-get-for-range (or (equal "before" what-to-get-for-range) (equal "both" what-to-get-for-range)) )
                   (read-number "number less than\n" )))
         (after (if (member what-to-get-for-time-range (list "after" "both" "after-relative" "both-relative"))
                    (if (member what-to-get-for-time-range (list "after-relative" "both-relative"))
                        (let* ((relative-by (completing-read-multiple "How would you like to specify after?"
                                                                      '("hour" "day" "minute" "second" "week" "month")))
                               (units-amounts (-map (lambda (time-unit)
                                                      (cons (intern time-unit ) (read-number (format "Relative by %s?" time-unit)) )) relative-by))
                               output)
                          (-each units-amounts (lambda (unit-amount) (setq output (-concat output (list (car unit-amount) (cdr unit-amount))))))
                          output)
                      (--> (read-string "Time after:\n" (format-time-string "%FT%T%z" (current-time) ))
                           (date-to-time it)
                           (time-convert it 'integer)) )
                  after
                  ))
         (before (if (member what-to-get-for-time-range (list "before" "both" "before-relative" "both-relative"))
                     (if (member what-to-get-for-time-range (list "before-relative" "both-relative"))
                         (let* ((relative-by (completing-read-multiple "How would you like to specify before?"
                                                                       '("hour" "day" "minute" "second" "week" "month")))
                                (units-amounts (-map (lambda (time-unit)
                                                       (cons (intern time-unit ) (read-number (format "Relative by %s?" time-unit)) )) relative-by))
                                output)

                           (-each units-amounts (lambda (unit-amount) (setq output (-concat output (list (car unit-amount) (cdr unit-amount))))))
                           output)
                       (--> (read-string "Time before:\n" (format-time-string "%FT%T%z" (current-time) ))
                            (date-to-time it)
                            (time-convert it 'integer)))
                   before))
         (output (-concat (list :key key :data-type type)
                          (when value (list :value value))
                          (when choices (list :choices choices))
                          (when after (list :after after))
                          (when before (list :before before)))))
    (when (and (or after before) (not (or (equal type 'INTEGER) (equal type 'REAL))))
      (error "kvd type is not a number"))
    output))

(defun pkm--convert-into-get-spec-text ()
  (list :text (read-string "What text would you like pkm to include?")))
(defun pkm--convert-into-get-spec-db-id ()
  (list :db-id (read-number "What is the id of node you would like?")))
(defun pkm--convert-into-get-spec-db-node-ids ()
  (list :db-node-ids
        (pkm2-nodes-search-multiple)))
(defun pkm--convert-into-get-spec-covert-parent ()
  (list :levels (intern (completing-read "How many levels of parents would you like?" `("1" "2" "3" "4" "5" "6" "7" "ALL")) ) ))
(defun pkm--convert-into-get-spec-covert-children ()
  (list :levels (read (completing-read "How many levels of children would you like?" `("1" "2" "3" "4" "5" "6" "7" "ALL")) ) ))

(provide 'pkm-create-pkm-query)
;;; pkm-create-pkm-query.el ends here
