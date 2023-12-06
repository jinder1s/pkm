;;; pkm-object.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dash)
(require 'pkm2-utils)
(require 'pkm-new-core)
(require 'pkm2-object-behavior)
(defvar pkm-structure-undefined-schemas-plist ())
(defvar pkm-structure-defined-schemas-plist ())




(defun pkm-register-structure (name structure-plist)
  (--> (plist-put structure-plist :name name) (plist-put it :pkm-type name) (plist-put pkm-structure-undefined-schemas-plist name it) (setq pkm-structure-undefined-schemas-plist it))
  )



(defvar pkm--object-known-keys (list :assets :links :parent :parents))

(defun pkm--object-psuedo-map-by-type-and-name (input-list)
  "INPUT-LIST should be formatted (type (plist)) plist might have :name key"
  (-map (lambda (item)
          (let* ((type (plist-get item :pkm-type))
                 (name (plist-get item :name)))
            (list (list type name) item)))
        input-list))

(defun pkm--object-nondestructively-combine (from to)
  "Things from FROM will overwrite things in TO"
  (let* (output
         (from-mapped (pkm--object-psuedo-map-by-type-and-name from))
         (to-mapped (pkm--object-psuedo-map-by-type-and-name to)))
    (-each from-mapped (lambda (item-mapped)
                         (unless (assoc-default (car item-mapped) to-mapped)
                           (push (cadr item-mapped) output))))
    (-each to-mapped (lambda (item-mapped)
                       (push (cadr item-mapped) output)))
    output))


(defun pkm--object-define (structure-name)
  "Used to create internal representation of pkm-object."
  (let* (fully-defined-schema
         (schema (plist-get pkm-structure-undefined-schemas-plist structure-name))
         (behaviors (plist-get schema :behaviors))
         (behavior-assets  (-map (lambda (behavior)
                                   (pkm2--get-behavior-assets (plist-get behavior :name) (plist-get behavior :link-to)))
                                 behaviors))
         (parent-name (plist-get schema :parent))
         (parent-schema (when parent-name (pkm--object-define parent-name)))
         (parent-keys (doom-plist-keys parent-schema))
         (self-keys (doom-plist-keys schema))
         (parent-assets (-map #'-copy (plist-get parent-schema :assets) )
                        )
         (self-assets (plist-get schema :assets))
         (combined-assets (-reduce-from #'pkm--object-nondestructively-combine (pkm--object-nondestructively-combine parent-assets self-assets) behavior-assets))
         (asset-modifications (plist-get schema :asset-modifications))
         (combined-assets (-map (lambda (asset)
                                  (if-let*
                                      ((a-mod (-find (lambda (mod)
                                                       (when (equal (plist-get mod :name) (plist-get asset :name))
                                                         t)) asset-modifications))
                                       (plist-key (plist-get a-mod :plist-key))
                                       (plist-value (plist-get a-mod :plist-value)))
                                      (plist-put asset  plist-key plist-value)
                                    asset))
                                combined-assets))

         (parent-links (-map #'-copy (plist-get parent-schema :links)))
         (self-links (plist-get schema :links))
         (links-in-both (pkm--object-nondestructively-combine self-links parent-links))
         (unhandled-keys (-filter (lambda (key)
                                    (not (member key pkm--object-known-keys)))
                                  (-concat parent-keys self-keys)))
         (parents (-concat (when parent-name `(,parent-name) ) (plist-get parent-schema :parents))) )
    (when combined-assets
      (setq fully-defined-schema (plist-put fully-defined-schema :assets combined-assets)))
    (when links-in-both (setq fully-defined-schema (plist-put fully-defined-schema :links links-in-both)))
    (-each unhandled-keys (lambda (key)
                            (setq fully-defined-schema
                                  (if (member key self-keys)
                                      (plist-put fully-defined-schema key (plist-get schema key))
                                    (plist-put fully-defined-schema key (plist-get parent-schema key))))))
    (setq fully-defined-schema (plist-put fully-defined-schema :parents parents))
    (when unhandled-keys
      (display-warning 'pkm-object "Unhandled keys, For now child is overwritting its parent, might want to do something else: %S" unhandled-keys))
    fully-defined-schema))

(defun pkm--object-validate-structure-schema (structure-schema)
  (let* ((primary-nodes (--> (plist-get structure-schema :assets)
                             (-filter (lambda (asset) (when (eq (plist-get asset :pkm-type) 'node) (plist-get asset :primary-node)))
                                      it)))
         (has-one-primary-node (= 1 (length primary-nodes))))
    (if (not has-one-primary-node) t (display-warning 'pkm-object "Object definition need to have one and only one primary node.\nPrimary nodes: %S" primary-nodes))))

(defun pkm--object-does-specifier-match (specifier asset-schema)
  (cond
   ((stringp specifier)                 ; node-specifier is a name
    (equal (plist-get asset-schema :name) specifier))
   ((eq specifier 'primary)
    (plist-get asset-schema :primary-node))
   ((eq specifier 'parent)
    (plist-get asset-schema :parent-node))
   ((eq specifier 'child)
    (message "in child asset, returning: %S, got: %S" (plist-get asset-schema :child-node) asset-schema)
    (plist-get asset-schema :child-node))
   (t (progn (display-warning 'pkm-object "node-specifier is ambigious: %S" specifier)
             (error "node-specifier was ambigious, be better!")))))





(defun pkm-object-get-structures (pkm-node)
  (--> (doom-plist-keys pkm-structure-defined-schemas-plist)
       (-filter (lambda (structure-name)
                  (pkm-objectp (plist-get pkm-structure-defined-schemas-plist structure-name) pkm-node)) it)))




(defvar pkm-structure-fully-specified-kvds-plist ())
(defvar pkm-structure-required-kvds-plist ())
(defvar pkm-structure-unique-required-plist ())
(defvar pkm-kvd-schemas-plist ())
(defvar pkm-kvd-key-to-structure-plist ())
(defvar pkm-data-type-to-kvd-key-plist ())

(defun pkm-object-register-structure-with-kvd (kvd structure-name)
  (let* ((key (plist-get kvd :key))
         (current-registrations  (plist-get pkm-kvd-key-to-structure-plist key #'equal)))
    (unless (member structure-name current-registrations)
      (setq  pkm-kvd-key-to-structure-plist (plist-put  pkm-kvd-key-to-structure-plist key (-concat `(,structure-name) current-registrations) #'equal)))))

(defun pkm-object-register-kvd-key-with-data-type (kvd)
  (let* ((key (plist-get kvd :key))
         (data-type (plist-get kvd :data-type))
         (current-registrations  (plist-get pkm-data-type-to-kvd-key-plist data-type #'equal)))
    (unless (member key current-registrations)
      (setq  pkm-data-type-to-kvd-key-plist (plist-put  pkm-data-type-to-kvd-key-plist data-type (-concat `(,key) current-registrations) #'equal)))))


(defun pkm-object-kvd-get-kvd-spec-for-key-in-structure (key structure-name)
  (--> (plist-get pkm-structure-defined-schemas-plist structure-name)
       (plist-get it :assets)
       (-find (lambda (asset-spec)
                  (when (and (equal (plist-get asset-spec :pkm-type) 'kvd)
                             (equal (plist-get asset-spec :key) key))
                    t))
                it)))

(defun pkm--object-find-unique-identifiers (structure-name structure-plist required-kvds-plist)
  (list (pkm--object-find-unique-identifiers-unique-key structure-name structure-plist required-kvds-plist)
        (pkm--object-find-unique-identifiers-unique-kvds structure-name structure-plist required-kvds-plist)))

(defun pkm--object-find-unique-identifiers-unique-key (structure-name  structure-plist required-kvds-plist)
  (let* ((self-fully-defined-kvds (plist-get required-kvds-plist structure-name))
         (keys (-distinct (-map (lambda (kvd) (plist-get kvd :key)) self-fully-defined-kvds)))
         (structure-names (doom-plist-keys required-kvds-plist))
         (unique-keys (-filter
                       (lambda (key)
                         (not (-any? (lambda (s-name)
                                       (unless (or (eq s-name structure-name)
                                                   (member structure-name (--> (plist-get structure-plist s-name) (plist-get it :parents))))
                                           (--> (plist-get required-kvds-plist s-name)
                                                (-map (lambda (kvd)
                                                        (plist-get kvd :key)) it)
                                                (member key it)) ))
                                     structure-names) ))
                       keys)))
    unique-keys))

(defun pkm--object-find-unique-identifiers-unique-kvds (structure-name structure-plist required-kvds-plist)
  (let* ((self-fully-defined-kvds (plist-get required-kvds-plist structure-name))
         (structure-names (doom-plist-keys required-kvds-plist))
         (unique-kvds (-filter
                       (lambda (s-kvd)
                         (not (-any? (lambda (temp-name)
                                       (if (or (eq temp-name structure-name) (member structure-name (--> (plist-get structure-plist temp-name) (plist-get it :parents))))
                                           nil
                                         (--> (plist-get required-kvds-plist temp-name)
                                              (-any? (lambda (kvd)
                                                       (if (equal (plist-get s-kvd :key) (plist-get kvd :key))
                                                           (or (equal (plist-get s-kvd :value) (plist-get kvd :value))
                                                               (and (and (plist-get s-kvd :choices) (plist-get kvd :choices))
                                                                    (or  (-intersection (plist-get s-kvd :choices) (plist-get kvd :choices))
                                                                         (-intersection (plist-get kvd :choices) (plist-get s-kvd :choices))) ))
                                                         nil))
                                                     it))))
                                     structure-names)))
                       self-fully-defined-kvds)))
    unique-kvds))



(defun pkm--object-get-required-kvds (object-schema)
  (let* ((assets (plist-get object-schema :assets))
         (primary-asset (-find (lambda (asset)
                                 (plist-get asset :primary-node))
                               assets))
         (required-kvds (-filter (lambda (possibly-kvd)
                                   (and (eq 'kvd (plist-get possibly-kvd :pkm-type))
                                        (not (plist-get possibly-kvd :optional))
                                        (not (plist-get possibly-kvd :managed))
                                        (-any
                                         (lambda (specifier)
                                           (pkm--object-does-specifier-match specifier primary-asset))
                                         (plist-get possibly-kvd :link-to))))
                                 assets)))
    required-kvds))

(defun pkm--behavior-get-required-kvds (object-schema)
  (let* ((assets (plist-get object-schema :assets))
         (required-kvds (-filter (lambda (possibly-kvd)
                                   (and (eq 'kvd (plist-get possibly-kvd :pkm-type))
                                        (not (plist-get possibly-kvd :optional))
                                        (not (plist-get possibly-kvd :managed))
                                        ))
                                 assets)))
    required-kvds))


(defun pkm--object-get-required-fully-specified-kvds (object-schema)
  (let* ((required-kvds (pkm--object-get-required-kvds object-schema))
         (fully-specified (pkm--object-get-required-fully-specified-kvds2 required-kvds) ))
    fully-specified))
(defun pkm--object-get-required-fully-specified-kvds2 (kvds)
"TODO replace v1 of this function with this."
  (-filter (lambda (kvd-schema)
             (or (stringp (plist-get kvd-schema :value))
                 (when (plist-get kvd-schema :choices)
                   (listp (plist-get kvd-schema :choices)))))
                                   kvds))
(defun pkm--object-get-time-related-kvd-keys ()
  (plist-get pkm-data-type-to-kvd-key-plist 'DATETIME))

(defun pkm-objectp (object-schema pkm-node)
  "Check if pkm-NODE is the primary pkm-node of OBJECT-SCHEMA"

  ;; (message "schema: %S" object-schema)
  (let* ((name (plist-get object-schema :name))
         (fully-specified (plist-get pkm-structure-fully-specified-kvds-plist name))
         (has-kvd (-non-nil (-map (lambda (kvd-schema)
                                    (cond ((plist-get kvd-schema :value)
                                           (pkm2-node-has-key-value pkm-node (plist-get kvd-schema :key) (plist-get kvd-schema :value)))
                                          ((plist-get kvd-schema :choices)
                                           (-any (lambda (choice)
                                                   (let* ((actual-choice (if (listp choice) (cdr choice) choice))
                                                          (key (plist-get kvd-schema :key)))
                                                     ;; (message "in choices: %S, key: %s, choice: %s, %S" name key actual-choice (pkm2-node-has-key-value pkm-node key actual-choice))
                                                     (pkm2-node-has-key-value pkm-node key actual-choice)))
                                                 (plist-get kvd-schema :choices)))))
                                  fully-specified) )))
    ;; (message "fs: %S, hk: %S\nkvd:%S" fully-specified has-kvd (pkm2-node-kvds pkm-node))
    (eq (length fully-specified ) (length has-kvd))))


(defun pkm--object-cache-structure-info ()
  (let* ((structure-names  (doom-plist-keys pkm-structure-undefined-schemas-plist))
         (behavior-structures (-filter (lambda (s-name)
                                         (--> (plist-get pkm-structure-undefined-schemas-plist s-name)
                                              (plist-get it :is-behavior)))
                                       structure-names)))
    (-each structure-names
      (lambda (structure-name)
        (setq pkm-structure-defined-schemas-plist
              (plist-put pkm-structure-defined-schemas-plist structure-name (pkm--object-define structure-name)))))
    (-each  structure-names
      (lambda (structure-name)
        (--> (plist-get pkm-structure-defined-schemas-plist structure-name)
             (pkm--object-get-required-kvds it)
             (setq pkm-structure-required-kvds-plist (plist-put pkm-structure-required-kvds-plist structure-name it)))))
    (-each  structure-names
      (lambda (structure-name)
        (--> (plist-get pkm-structure-defined-schemas-plist structure-name)
             (pkm--object-get-required-fully-specified-kvds it)
             (setq pkm-structure-fully-specified-kvds-plist (plist-put pkm-structure-fully-specified-kvds-plist structure-name it)))))
    (-each  structure-names
      (lambda (structure-name)
        (--> (pkm--object-find-unique-identifiers structure-name pkm-structure-defined-schemas-plist pkm-structure-required-kvds-plist)
             (setq pkm-structure-unique-required-plist (plist-put pkm-structure-unique-required-plist structure-name it)))))
    ;; For behaviour based structures, only compare with other behivor based structures
    (let* ((structure-to-behavior-schema-plist  (mm-alist-to-plist (-map (lambda (s-name)
                                                                           (cons s-name
                                                                                 (--> (plist-get pkm-structure-undefined-schemas-plist s-name)
                                                                                      (plist-get it :is-behavior)
                                                                                      (plist-get pkm-structure-defined-behavior-plist it) )))
                                                                         behavior-structures) ))
           (required-kvds-plist (mm-alist-to-plist (-map (lambda (s-name)
                                                           (cons s-name
                                                                 (--> (plist-get structure-to-behavior-schema-plist s-name)
                                                                      (pkm--behavior-get-required-kvds
                                                                       it))))
                                                         behavior-structures))))
      (-each  behavior-structures
        (lambda (structure-name)
          (--> (pkm--object-find-unique-identifiers structure-name structure-to-behavior-schema-plist  required-kvds-plist)
               (setq pkm-structure-unique-required-plist (plist-put pkm-structure-unique-required-plist structure-name it))))))))


(provide 'pkm-object)
