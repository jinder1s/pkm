;;; pkm.el --- Personal Knowledge Manager  -*- lexical-binding: t; -*-

; Copyright (C) 2023  Manjinder Singh

; Author: Manjinder Singh
; Maintainer: Manjinder Singh
; URL: https://github.com/jinder1s/pkm.git
; Version: 0.0.1
; Package-Requires: ((emacs "29.1")("dash" "2.13") ("ts" "0.3")("persist" "0.5"))
; Keywords: notes, tasks, habits, clocks

; This file is not part of GNU Emacs.

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
; Personal Knowledge Manager
;;
;;

;;; Code:

; Happy coding! ;)

(require 'pkm-compile-db-query-old)
(require 'pkm-compile-db-query)
(require 'pkm-new-core )
(require 'pkm2-browse)
(require 'pkm2-tasks)
(require 'pkm2-clock )
(require 'pkm2-habit )
(require 'pkm2-object-registrations)
(require 'pkm2-questions )
(require 'pkm2-quick-capture )

(provide 'pkm)
;;; pkm.el ends here
