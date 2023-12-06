;;; pkm2-managed-structures.el -*- lexical-binding: t; -*-
(defvar pkm2--managed-structure-name-to-managed-assets-plist ())

(defun pkm2--structure-get-managed-structure-names ()
  (let* ((names (doom-plist-keys pkm-structure-defined-schemas-plist))
         (managed (-filter (lambda (s-n)
                             (--> (plist-get pkm-structure-defined-schemas-plist s-n)
                                  (plist-get it :managed-type))) names)))
    managed))
(defun pkm2--structure-is-structure-managed-p (structure-name)
  (--> (plist-get pkm-structure-defined-schemas-plist structure-name)
       (plist-get it :managed-type)))


(defun pkm2--structure-get-managed-asset-for-structure-name (structure-name)
  (when (pkm2--structure-is-structure-managed-p structure-name)
    (--> (plist-get pkm-structure-defined-schemas-plist structure-name)
         (plist-get it :assets)
         (-filter (lambda (asset-spec)
                    (plist-get asset-spec :managed)) it))))

(defun pkm2--structure-get-managed-nodes-query ()
  (let* ((managed-structure-names (pkm2--structure-get-managed-structure-names))
         (query (-map (lambda (m-s-n) `(:or structure-type ( :structure-name ,m-s-n)))
                      managed-structure-names)))
    query))

(defun pkm2--structure-get-unfinished-managed-nodes-of-type (structure-name)
  ;; TODO (JINDER) start here
  (when (pkm2--structure-is-structure-managed-p structure-name)
    (let* ((managed-asset-specs (pkm2--structure-get-managed-asset-for-structure-name structure-name))
           (structure-query  `(:or structure-type (:structure-name ,structure-name)))
           (queries (-map (lambda (asset-spec)
                            (pcase (plist-get asset-spec :pkm-type)
                              ('node
                               (error "Nod management has not been implemented."))
                              ('kvd
                               `(,structure-query (:not kvd (:key ,(plist-get asset-spec :key) :data-type ,(plist-get asset-spec :data-type))) ))
                              ((pred (lambda (pkm-type) (member pkm-type structure-names)))
                               (error "sub structure managemente has not been implemented."))))
                          managed-asset-specs) )
           (db-queries (-map #'pkm2--compile-full-db-query queries))
           (output (-map (lambda (db-query)
                           (sqlite-select jinder_dbh db-query)) db-queries)))
      output)))
