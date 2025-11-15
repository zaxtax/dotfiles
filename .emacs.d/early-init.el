(setq default-frame-alist '(
    ;; Setting the face in here prevents flashes of
    ;; color as the theme gets activated
    (background-color . "#1e1e2e")
    (ns-appearance . dark)
    (ns-transparent-titlebar . t)))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(set-face-attribute 'mode-line nil
                    :background "#181825"  ;; Catppuccin mocha dark bg
                    :foreground "#cdd6f4"  ;; Soft foreground
                    :box nil
                    :height 90)            ;; Smaller font size if desired
