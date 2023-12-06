;;; pkm/pkm2-object-behaviours.el -*- lexical-binding: t; -*-


(defvar pkm-structure-defined-behavior-plist ())

(defun pkm2-register-behavior (behavior)
  (setq pkm-structure-defined-behavior-plist (plist-put pkm-structure-defined-behavior-plist (plist-get behavior :name) behavior)))


(defun pkm2--get-behavior-assets (behavior-name link-to)
  (--> (plist-get pkm-structure-defined-behavior-plist behavior-name)
       (plist-get it :assets)
       (-map (lambda (asset)
               (plist-put asset :link-to link-to))
             it)))
