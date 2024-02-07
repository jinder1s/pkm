;;; test-eieio-capture.el  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jinder
;;
;; Author: Jinder <jinder1s@gmail.com>
;; Maintainer: Jinder <jinder1s@gmail.com>
;; Created: February 07, 2024
;; Modified: February 07, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/msingh15/test-eieio-capture
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(when (require 'undercover nil t)
  (undercover "pkm-new-core.el"
              (:report-file "/tmp/local-report.txt")
              (:report-format 'text)
              (:send-report nil)))
(require 'pkm-new-core)

(describe "Eieio object definition tests"
  (it "Define base node"
    (let* ((node-asset (node-schema  :name "base-node" :primary-node t))
           (schema  (object-schema
                     :name "base-n"
                     :assets (list node-asset)))
           (object-def (schema-compile schema)))
      (expect object-def :not :to-be nil)
      (expect (oref object-def :assets) :not :to-be nil)
      (expect (length (oref object-def :assets) ) :to-be 1)
      (expect (car (oref object-def :assets)) :not :to-be node-asset)
      (expect (car (oref object-def :assets)) :to-equal node-asset)
      ))


  (it "Define node with parent"
    (let* ((node-asset (node-schema :name "base-node" :primary-node t))
           (base-schema (object-schema
                         :name "base-n"
                         :assets (list node-asset)))
           (pkm-structure-uncompiled-schemas (list base-schema))
           (new-schema (object-schema
                        :parent-schema-name "base-n"))


           (object-def (schema-compile new-schema)))
      (expect object-def :not :to-be nil)
      (expect (oref object-def :assets) :not :to-be nil)
      (expect (length (oref object-def :assets) ) :to-be 1)
      (expect (car (oref object-def :assets)) :not :to-be node-asset)
      (expect (car (oref object-def :assets)) :to-equal node-asset)))
  (it "Define base node with modifications"
    (let* ((node-asset (node-schema  :name "base-node" :content "Hello" :primary-node t))
           (mod (list :name "base-node" :plist-key :content :plist-value "World"))
           (schema  (object-schema
                     :name "base-n"
                     :assets (list node-asset)
                     :asset-modifications (list mod )))
           (object-def (schema-compile schema)))
      (expect object-def :not :to-be nil)
      (expect (oref object-def :assets) :not :to-be nil)
      (expect (length (oref object-def :assets) ) :to-be 1)
      (expect (car (oref object-def :assets)) :not :to-be node-asset)
      (expect (car (oref object-def :assets)) :not :to-equal node-asset)
      (message "assets eieio: %S" (oref object-def :assets))
      (expect (oref (car (oref object-def :assets)) :content) :to-equal "World")

      ))
  (it "Define base node with behavior"
    (let* ((node-asset (node-schema  :name "base-node" :content "Hello" :primary-node t))
           (schema  (object-schema
                     :name "base-n"
                     :behaviors '((:name "test-behavior" :link-to (primary)))))
           (pkm-structure-behavior-schemas (list (behavior-schema :name "test-behavior" :assets (list node-asset))))
           (object-def (schema-compile schema)))
      (expect object-def :not :to-be nil)
      (expect (oref object-def :assets) :not :to-be nil)
      (expect (length (oref object-def :assets) ) :to-be 1)
      (expect (car (oref object-def :assets)) :not :to-be node-asset)
      (expect (car (oref object-def :assets)) :not :to-equal node-asset)
      (message "assets eieio: %S" (oref object-def :assets))
      (expect (oref (car (oref object-def :assets)) :content) :to-equal "Hello"))))


(provide 'test-eieio-capture)
;;; test-eieio-capture.el ends here
