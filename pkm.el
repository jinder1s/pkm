;;; pkm.el --- Personal Knowledge Manager  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Manjinder Singh

;; Author: Manjinder Singh
;; Maintainer: Manjinder Singh
;; URL: https://hello.com
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: notes, tasks, habits, clocks

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Personal Knowledge Manager
;;
;;

;;; Code:

;; Happy coding! ;)

(require 'ts)
(require 'dash)
(require 'pkm2-browse )
(require 'pkm2-browse-hierarchy )
(require 'pkm2-browse-insert )
(require 'pkm2-browse-interactivity )
(require 'pkm2-clock )
(require 'pkm2-compile-queries )
(require 'pkm2-ewoc-capture )
(require 'pkm2-experiment-index )
(require 'pkm2-habit )
(require 'pkm2-log )
(require 'pkm2-nodes-links )
(require 'pkm2-object-behavior )
(require 'pkm2-object-registrations )
(require 'pkm2-questions )
(require 'pkm2-quick-capture )
(require 'pkm2-search )
(require 'pkm2-utils )
(require 'pkm-data-change )
(require 'pkm-new-core )
(require 'pkm-object )
(require 'pkm-object-capture )

(provide 'pkm)
;;; pkm.el ends here
