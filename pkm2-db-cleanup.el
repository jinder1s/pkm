;;; pkm/pkm2-db-cleanup.el -*- lexical-binding: t; -*-
(defun pkm2--db-find-kvds-with-no-links ()
  (let* ((query (concat "SELECT kvd.id FROM key_value_data_integer kvd "
                        "LEFT JOIN data_or_properties_link_integer link "
                        "ON kvd.id = link.key_value_data "
                        "WHERE link.node IS NULL;"))
         (output (sqlite-select jinder_dbh query)))
    output))
(defun pkm2--db-delete-kvds-with-no-links ()
  (-each (doom-plist-keys pkm2--key_value_data-type-to-table-plist)
    (lambda (type)
      (let* ((data-table (pkm2--db-get-kvd-data-table-for-type type))
             (select-query (concat (format "SELECT kvd.id FROM %s kvd " (pkm2--db-get-kvd-data-table-for-type type) )
                                   (format "LEFT JOIN %s link " (pkm2--db-get-kvd-link-table-for-type type))
                                   "ON kvd.id = link.key_value_data "
                                   "WHERE link.node IS NULL"))
             (delete-query (format "DELETE FROM %s WHERE id IN (%s)" data-table select-query)))
        (sqlite-execute jinder_dbh delete-query)))))

(provide 'pkm2-db-cleanup)
