;;; pkm.el --- Personal knowledge management system based on slite -*- lexical-binding: t; -*-

;; Author: Jinder Singh
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4")(emacs-sqlite3-api "0")(magit-section "3.0.0")(dash "2.13"))


;;; Commentary:
;; A personal management system based around sqlite.
;; Meant to mimic some of org-mode functionality and to make it easy for you to access your
;; pkm on multiple devices. VERY WIP.

;;; Code:

(require 'pkm-new-core)

(provide 'pkm)
