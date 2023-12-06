;;; pkm2-mode-map.el -*- lexical-binding: t; -*-

;; (defvar pkm2-browse-key (kbd "C-l"))

(defvar pkm2-browse-mode-map (make-sparse-keymap))
;; (defvar pkm2-browse-leader-map (make-sparse-keymap))

;; (define-key pkm2-browse-leader-map  pkm2-browse-key pkm2-browse-mode-map)
;; (define-key pkm2-browse-mode-map (kbd "TAB") #'pkm2--browse-toggle-hidden-info-at-point )
;; (define-key pkm2-browse-mode-map (kbd "<tab>") #'pkm2--browse-toggle-hidden-info-at-point)
;; (define-key pkm2-browse-mode-map (kbd "e") #'pkm2--browse-edit-object-at-point)
;; (define-key pkm2-browse-mode-map (kbd "s") #'pkm2--browse-see-children-at-point)
;; (define-key pkm2-browse-mode-map (kbd "c") #'pkm--object-capture)
;; (define-key pkm2-browse-mode-map (kbd "r") #'pkm2--refresh-current-buffer)


(map! :map pkm2-browse-mode-map
      ;; Recently, a [tab] keybind in `outline-mode-cycle-map' has begun
      ;; overriding org's [tab] keybind in GUI Emacs. This is needed to undo
      ;; that, and should probably be PRed to org.

      "C-M-RET"      #'pkm--browse-capture-node-as-child-of-node-at-point
      [C-M-return] #'pkm--browse-capture-node-as-child-of-node-at-point

      ;; TODO modify functions to add captured note to browser below
      "C-RET"      #'pkm--browse-capture-node-as-sibling-of-node-at-point
      [C-return]   #'pkm--browse-capture-node-as-sibling-of-node-at-point

      ;; TODO modify functions to add captured note to browser above
      "C-S-RET"    #'pkm--browse-capture-node-as-sibling-of-node-at-point
      [C-S-return] #'pkm--browse-capture-node-as-sibling-of-node-at-point
      (:when IS-MAC
        [s-return]   #'pkm--browse-capture-node-as-sibling-of-node-at-point
        [s-S-return] #'pkm--browse-capture-node-as-sibling-of-node-at-point
        [s-M-return] #'pkm--browse-capture-node-as-child-of-node-at-point)

      :localleader
      [tab]        #'pkm2--browse-toggle-hidden-info-at-point
      (:prefix ("e" . "edit")
               "e" #'pkm2--browse-edit-object-at-point)
      (:prefix ("a" . "a")
               "c" #'pkm--browse-capture-node-as-child-of-node-at-point
               "s" #'pkm--browse-capture-node-as-sibling-of-node-at-point
               )
      (:prefix ("r" . "relations")
               "c" #'pkm2--browse-add-node-as-child-to-node-at-point
               "p" #'pkm2--browse-add-node-as-parent-to-node-at-point
               (:prefix ("s" . "show")
                :desc "Children" "c" #'pkm2--browse-see-children-at-point
                :desc "Parents"  "p" #'pkm2--browse-see-parents-at-point))
      (:prefix ("b" . "buffer")
               "r" #'pkm2--refresh-current-buffer)
      (:prefix ("s" . "section")
               "r" #'pkm2--refresh-current-buffer)
      (:prefix "n"
               (:prefix ("a" . "add")
                :desc "Add kvd" "k" #'pkm2--browse-add-kvd-at-point
                "c" #'pkm--browse-capture-node-as-child-of-node-at-point
                "s" #'pkm--browse-capture-node-as-sibling-of-node-at-point )
               (:prefix ("c" . "clock")
                "e" #'pkm2-clock-edit-clock-at-point
                "i" #'pkm2-clock-in
                "o" #'pkm2-clock-out
                "s" #'pkm2-clock-switch)

               "f" #'pkm2--browse-filter-children-nodes-at-point
               "n" #'pkm2--narrow-to-node-and-children-at-point
               "d" #'pkm2--browse-delete-node-at-point))



(define-minor-mode pkm2-browse-mode
  "Minor mode for simple finish/cancel keybindings."
  :keymap pkm2-browse-mode-map)

(provide 'pkm2-browse-mode-map)
