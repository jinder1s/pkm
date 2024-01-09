;;; pkm-hammerspoon.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jinder
;;
;; Author: Jinder <jinder1s@gmail.com>
;; Maintainer: Jinder <jinder1s@gmail.com>
;; Created: January 09, 2024
;; Modified: January 09, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/msingh15/pkm-hammerspoon
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun pkm-hammerspoon-alert (message)
  "shows Hammerspoon's hs.alert popup with a MESSAGE"
  (when (and message (eq system-type 'darwin))
    (call-process (executable-find "hs")
                  nil 0 nil
                  "-c"
                  (concat "hs.alert.show(\"" message "\", 5)"))))

(provide 'pkm-hammerspoon)
;;; pkm-hammerspoon.el ends here
