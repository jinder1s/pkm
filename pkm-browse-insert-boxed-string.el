;;; pkm-browse-insert-boxed-string.el -*- lexical-binding: t; -*-

(cl-defun mb-insert-shadowed-text
    (text &key (horizontal-margins 20)
          (padding 5)
          (shadow-distance 5)
          (background-color "DarkRed")
          (right-align-spec 'right))
  (let* (;; Legacy stuff
         (border-width 5)

         ( stipple
           ;; Everything can be converted to bits, so as far as I can tell,
           ;; everything will work.
           '(2 2 "a
a"))
         ( segment-left-margin
           (lambda (&optional additonal-specs)
             (propertize " "
                         'display `(space :align-to (,horizontal-margins)
                                    ,@additonal-specs))))
         ( segment-filler
           (lambda (&optional additonal-specs)
             (propertize " "
                         'display `(space :align-to
                                    (- ,right-align-spec
                                       (,border-width)
                                       (,horizontal-margins))
                                    ,@additonal-specs)
                         'face `(:background ,background-color))))
         ( segment-r-shadow
           (lambda (&optional additonal-specs)
             (propertize " " 'display `(space :width (,border-width)
                                        ,@additonal-specs)
                         'face `(:foreground "Black" :stipple ,stipple))))

         ( segment-lr-padding
           (lambda ()
             (propertize " " 'display `(space :width (,padding))
                         'face `(:background ,background-color))))
         ( segment-b-shadow
           (lambda ()
             (propertize
              (concat
               (funcall segment-left-margin `(:height (,border-width)))
               (propertize " "
                           'display `(space :align-to
                                      (- ,right-align-spec
                                         (,horizontal-margins))
                                      :height (,shadow-distance)
                                      :width (,shadow-distance)))
               (propertize " "
                           'display `(space :align-to
                                      (- ,right-align-spec
                                         (,horizontal-margins))
                                      :height (,shadow-distance))
                           'face `(:foreground "Black" :stipple ,stipple))
               (propertize "\n" 'line-height t))
              ;; 'point-entered 'mb-kick-cursor
              )))
         ( segment-tb-padding
           (lambda ()
             (propertize
              (concat
               (funcall segment-left-margin  `(:height (,padding)))
               (funcall segment-filler  `(:height (,padding))))
              ;; 'point-entered 'mb-kick-cursor
              )))
         ( segment-line
           (lambda (text)
             (concat (propertize
                      (concat
                       (funcall segment-left-margin)
                       (funcall segment-lr-padding))
                      ;; 'point-entered 'mb-kick-cursor
                      )
                     (propertize text
                                 'wrap-prefix
                                 (concat (funcall segment-left-margin)
                                         (funcall segment-r-shadow)
                                         (funcall segment-lr-padding))
                                 'face `(:background ,background-color))
                     (propertize
                      (concat
                       (funcall segment-filler)
                       (funcall segment-r-shadow)
                       "\n")
                      ;; 'point-entered 'mb-kick-cursor
                      )))))


    (mapc (lambda (line) (insert (funcall segment-line line)))
          (split-string text "\n"))
    ))
