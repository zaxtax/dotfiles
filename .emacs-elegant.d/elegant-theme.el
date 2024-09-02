(deftheme elegant
  "A simple and elegant theme.")

(defun mode-line-align (left right)
  "Return a string with LEFT and RIGHT at the edges of the
current window."
  (format (format "%%s %%%ds" (- (window-total-width) (length left) 2))
          left right))

(custom-theme-set-variables
 'elegant
 '(default-frame-alist
    '((internal-border-width . 20)))
 '(mode-line-format
   '(:eval
     (mode-line-align
      (format-mode-line
       (list " " mode-line-buffer-identification " " mode-line-modified " " mode-name))
      (format-mode-line
       (list minor-mode-alist " " mode-line-misc-info))))))

(custom-theme-set-faces
 'elegant
 '(mode-line
   ((((type tty)) (:inverse-video t))
    (default (:overline t))))
 '(mode-line-inactive
   ((((type tty)) (:inherit (shadow)))
    (default (:inherit (mode-line shadow))))))

(provide-theme 'elegant)
