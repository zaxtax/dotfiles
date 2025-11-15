;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

(when window-system 
   ;(set-face-font 'default "6x10")
   (set-frame-font "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
   (set-face-attribute 'default nil :height 90)
   (setq-default line-spacing 2)
   (set-face-font 'tooltip "6x10"))

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ;("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ;("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

;; (add-to-list 'load-path "~/.emacs.d")
(setq custom-file "~/.emacs.d/custom.el")

;; (add-to-list 'load-path "/home/z/upstream/elisp/zenburn-emacs")
;; (require 'color-theme-zenburn)
;; (color-theme-zenburn)

;; (setq zenburn-override-colors-alist
;;       '(("zenburn-bg+05" . "#282828")
;;         ("zenburn-bg+1"  . "#3F3F3F")
;;         ("zenburn-bg+2"  . "#3F3F3F")
;; 	("zenburn-bg+3"  . "#4F4F4F")))
;; (load-theme 'zenburn t)

;; (set-face-attribute 'mode-line-inactive nil :box '(:line-width 1 :color "#2b2b2b")
;;                     :weight 'normal :foreground "#5f7f5f" :background "#2b2b2b")
;; (set-face-attribute 'mode-line nil :box '(:line-width 1 :color "#282828")
;;                     :weight 'normal :foreground "#8fb28f" :background "#2b2b2b")
(load-theme 'catppuccin t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(transient-mark-mode 1)
(horizontal-scroll-bar-mode -1)
(scroll-bar-mode -1)
(setq use-dialog-box nil)
(setq inhibit-splash-screen t)
(setq column-number-mode t)
(winner-mode 1)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq calendar-holidays nil)

(setq temporary-file-directory "~/.emacs.d/tmp/")

; return a backup file path of a give file path
; with full directory mirroring from a root dir
; non-existant dir will be created
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let (backup-root bpath)
    (setq backup-root "~/.emacs.d/emacs-backup")
    (setq bpath (concat backup-root fpath "~"))
    (make-directory (file-name-directory bpath) bpath)
    bpath))

(setq make-backup-file-name-function 'my-backup-file-name)

(defvar real-keyboard-keys
  '(("M-<up>"        . "\M-[1;3A")
    ("M-<down>"      . "\M-[1;3B")
    ("M-<right>"     . "\M-[1;3C")
    ("M-<left>"      . "\M-[1;3D")
    ("C-<return>"    . "\C-j")
    ("C-<delete>"    . "\M-[3;5~")
    ("C-<up>"        . "\M-[1;5A")
    ("C-<down>"      . "\M-[1;5B")
    ("C-<right>"     . "\M-[1;5C")
    ("C-<left>"      . "\M-[1;5D")))

(set-fontset-font t 'unicode "Symbola" nil 'prepend)
(global-xah-math-input-mode 1)
;(add-hook 'after-init-hook #'global-emojify-mode)
;(global-set-key [(control tab)] 'bury-buffer)

;; (use-package dired-sidebar
;;   :bind (("C-x /" . dired-sidebar-toggle-sidebar))
;;   :config
;;   (setq dired-sidebar-close-sidebar-on-file-open t)
;;   (setq dired-sidebar-window-fixed nil))

(add-to-list 'load-path "~/.emacs.d/doc-sidebar/")
(require 'doc-sidebar)
(global-set-key (kbd "C-x /") 'toggle-sidebar)

(require 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

(defun key (desc)
  (or (and window-system (read-kbd-macro desc))
      (or (cdr (assoc desc real-keyboard-keys))
          (read-kbd-macro desc))))

(global-set-key (key "M-<left>") 'windmove-left)          ; move to left windnow
(global-set-key (key "M-<right>") 'windmove-right)        ; move to right window
(global-set-key (key "M-<up>") 'windmove-up)              ; move to upper window
(global-set-key (key "M-<down>") 'windmove-down)          ; move to downer window

;; (add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
;; (setq highlight-symbol-on-navigation-p t)
;; (global-set-key (kbd "C-*") 'highlight-symbol-next)
;; (global-set-key (kbd "C-#") 'highlight-symbol-prev)

(setq lsp-pyright-multi-root nil)
(setq lsp-pyright-typechecking-mode "off")

(setenv "LOCAL_MAPLE" "/home/z/Desktop/maple18/bin/maple")
(setenv "PATH" (concat "/home/z/.lein/bin:"
		       "/home/z/.local/bin:"
		       "/home/z/upstream/android-sdk-linux/tools:"
		       "/home/z/Desktop/maple18/bin:"
		       "/home/z/.cabal/bin:"
		       "/home/z/.cargo/bin:"
		       "/sbin:/usr/sbin:"
		       (getenv "PATH")))

(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

(setq ispell-dictionary "british")

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Flyspell
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(defun flyspell-generic-textmode-verify ()
  "Used for `flyspell-generic-check-word-predicate' in text modes."
  ;; (point) is next char after the word. Must check one char before.
  (let ((f (get-text-property (- (point) 1) 'face)))
    (not (memq f '(markdown-pre-face markdown-language-keyword-face)))))

(setq flyspell-generic-check-word-predicate 'flyspell-generic-textmode-verify)
(add-hook 'markdown-mode-hook 'flyspell-mode)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Workgroups
(require 'workgroups2)
(setq wg-prefix-key (kbd "C-c w"))
(workgroups-mode 1)

;; ESS settings
(autoload 'r-mode "ess-site.el" "ESS" t)
(add-to-list 'auto-mode-alist '("\\.R$" . r-mode))

;; (defun read-lines (file)
;;   "Return a list of lines in FILE."
;;   (with-temp-buffer
;;     (insert-file-contents file)
;;     (split-string
;;      (buffer-string) "\n" t)))

;; (add-hook 'ess-mode-hook
;; 	    '(lambda()
;; 	       (setq ess-my-extra-R-function-keywords
;; 		     (read-lines "~/.emacs.d/R-function-names.txt"))
;; 	       (setq ess-R-mode-font-lock-keywords
;; 		     (append ess-R-mode-font-lock-keywords
;; 			     (list (cons (concat "\\<" (regexp-opt
;; 							ess-my-extra-R-function-keywords 'enc-paren) "\\>")
;; 					 'font-lock-function-name-face))))))

;; writegood
(require 'writegood-mode)
(global-set-key "\C-cg" 'writegood-mode)

;; writeroom
(setq writeroom-width 120)
(setq writeroom-restore-window-config t)

;; doc-view

(when window-system
  (use-package doc-view
    :config ;; another option to do it with `use-package'
    (setq doc-view-scale-internally nil)
    (define-key doc-view-mode-map (kbd "Q") 'image-kill-buffer))
  
  (add-to-list 'auto-mode-alist '("\\.djvu\\'" . doc-view-mode))
  (use-package pdf-tools
    :config
    ;; initialise
    (pdf-loader-install)
    ;; open pdfs scaled to fit page
    (setq-default pdf-view-display-size 'fit-page)
    ;; automatically annotate highlights
    (setq pdf-annot-activate-created-annotations t)
    ;; use normal isearch
    (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)))


(require 'ido)
(ido-mode t)

(mouse-wheel-mode)

(when window-system
  ;; Session Saving
  ;; use only one desktop
  (setq desktop-path '("~/.emacs.d/"))
  (setq desktop-dirname "~/.emacs.d/")
  (setq desktop-base-file-name "emacs-desktop")

  ;; remove desktop after it's been read
  ;; (add-hook 'desktop-after-read-hook
  ;; 	    '(lambda ()
  ;; 	       ;; desktop-remove clears desktop-dirname
  ;; 	       (setq desktop-dirname-tmp desktop-dirname)
  ;; 	       (desktop-remove)
  ;; 	       (setq desktop-dirname desktop-dirname-tmp)))

  (defun saved-session ()
    (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

  ;; use session-restore to restore the desktop manually
  (defun session-restore ()
    "Restore a saved emacs session."
    (interactive)
    (if (saved-session)
	  (desktop-read)
      (message "No desktop found.")))

  ;; use session-save to save the desktop manually
  (defun session-save ()
    "Save an emacs session."
    (interactive)
    (if (saved-session)
	(if (y-or-n-p "Overwrite existing desktop? ")
            (progn
              (wg-create-workgroup "wg0")
	      (desktop-save-in-desktop-dir))
	  (message "Session not saved."))
      (desktop-save-in-desktop-dir)))

  
  ;; ask user whether to restore desktop at start-up
  (add-hook 'after-init-hook
	    (lambda ()
	       (if (saved-session)
		    (if (y-or-n-p "Restore desktop? ")
		        (progn
		         	 (session-restore)
			         (org-agenda-list)
			         (wg-open-workgroup "wg0")))))))

;; Dynamic Completion
(load-library "completion")                                                    
(dynamic-completion-mode) 
(global-set-key (kbd "M-<RETURN>") 'complete)

(cua-selection-mode 1) ;; only for rectangles

;; Hide/Show tag
(global-set-key (kbd "M-+") 'hs-toggle-hiding)

(require 'rainbow-delimiters)

(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)


;; rust
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-to-list 'exec-path "/home/z/.cargo/bin")
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t))

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; These are optional configurations
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-headerline-breadcrumb-segments '(symbols))
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-enable-snippet nil)
  ;; Use a different target dir for rust analyzer to not clobber cargo cache
  (setq lsp-rust-analyzer-cargo-target-dir t))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  ;; (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-peek-always-show t))

(with-eval-after-load 'lsp-ui
  ;; Remap `xref-find-definitions' (bound to M-. by default)
  ;; (define-key lsp-ui-mode-map
  ;;             [remap xref-find-definitions]
  ;;             #'lsp-ui-peek-find-definitions)

  ;; Remap `xref-find-references' (bound to M-? by default)
  (define-key lsp-ui-mode-map
              [remap xref-find-references]
              #'lsp-ui-peek-find-references))


(require 'python)
(autoload 'python-mode "python-mode" "Mode for editing Python source files")
(add-to-list 'auto-mode-alist '("\\.py" . python-mode))
(setq python-indent-guess-indent-offset t)  
(setq python-indent-guess-indent-offset-verbose nil)

(setq interpreter-mode-alist
      (cons '("python" . python-mode)
          interpreter-mode-alist)

      python-mode-hook
      (lambda () (progn
           (set-variable 'py-smart-indentation t)
           (set-variable 'indent-tabs-mode nil) )))

(add-hook 'python-mode-hook 'flycheck-mode)
(setq flycheck-python-pycompile-executable "python3")
(setq flymake-python-pyflakes-extra-arguments '("--extend-ignore=E22 --max-line-length=199"))

(global-font-lock-mode t)
(show-paren-mode t)

;; mouse config
(mouse-wheel-mode)
;;(set-scroll-bar-mode 'right)
(setq scroll-step 1)

;;; Org-mode
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)
(require 'org-bullets)
(require 'ox-md)
(require 'ox-beamer)
(require 'ox-twbs)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-html-postamble nil)

(setq org-element-use-cache nil)
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-ido-switchb)

(setq org-default-notes-file (concat org-directory "notes.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
 '(("j" "Journal entry" plain
    (file+datetree "~/notes/journal.org")
    "\n%U\n%?\n%i\n\n"
    :empty-lines 1)
   ("t" "Tasks" entry
    (file+headline "~/notes/orgfiles/refile.org" "Tasks")
    ; (file+olp "filename" "Level 1 heading" "Level 2" ...)
    "* TODO %?\n  %i")
   ("c" "Clipboard" entry
    (file+headline "~/notes/orgfiles/refile.org" "Tasks")
     "* NOTE %?\n%c\n  %i")
   ("a" "Appointments" entry
    (file+headline "~/notes/orgfiles/refile.org" "Appointments")
    "* Appointment: %?\n%^T\n%i")))

(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "NEXT(n)" "|" "DONE(d)")
			  (sequence "WAITING(w)" "|" "DEFERRED(f)" "DELEGATED(l)")))
(setq org-use-fast-todo-selection t)

(setq org-agenda-custom-commands
    '(("w" todo "WAITING" nil) 
      ("n" todo "NEXT" nil)
      ("c" todo "DONE|DEFERRED|DELEGATED|CANCELLED" nil)
      ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT")))))
(setq org-startup-folded nil)
(setq org-log-done t)
(setq org-agenda-include-diary t)   
(setq org-agenda-include-all-todo t)
(setq org-deadline-warning-days 30)
(setq org-agenda-todo-ignore-with-date t)

(setq org-agenda-files (list "~/notes/orgfiles/work.org"
                             "~/notes/orgfiles/calendar.org"
                             "~/notes/orgfiles/organizer.org"))
(setq org-archive-location "archives/%s_archive::")

;; Org-gcal
;; (require 'org-gcal)
;; (load "~/.emacs.d/org-gcal-vars.el")
;; (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))

;; Org-habits
(add-to-list 'org-modules 'org-habit)
(require 'org-habit)
(setq org-habit-show-habits-only-for-today t)

(setq org-agenda-sorting-strategy
      (quote ((agenda habit-down time-up user-defined-up priority-down effort-up category-keep)
              (todo category-up priority-down effort-up)
              (tags category-up priority-down effort-up)
              (search category-up))))

;; Task Complete bindings
(eval-after-load "org"
  '(progn
     (define-prefix-command 'org-todo-state-map)

     (define-key org-mode-map "\C-cx" 'org-todo-state-map)

     (define-key org-todo-state-map "x"
       #'(lambda nil (interactive) (org-todo "TODO")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "f"
       #'(lambda nil (interactive) (org-todo "DEFERRED")))
     (define-key org-todo-state-map "l"
       #'(lambda nil (interactive) (org-todo "DELEGATED")))
     (define-key org-todo-state-map "s"
       #'(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w"
       #'(lambda nil (interactive) (org-todo "WAITING")))))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; For MobileOrg
(setq org-directory "~/notes/orgfiles/")
(setq org-mobile-files (quote ("organizer.org")))
(setq org-mobile-inbox-for-pull "~/notes/orgfiles/inbox.org")
(setq org-mobile-directory "~/Dropbox/orgfiles/")
(setq org-mobile-force-id-on-agenda-items nil)
(setq org-agenda-skip-scheduled-if-done t)
      

;; ;;(require 'slime)
;; (eval-after-load "slime"
;;   '(progn
;;      ;; fancy slime startup
;;      (add-to-list 'load-path "~/.sbcl/site/slime/contrib")
;;      (require 'slime-banner)
;;      (slime-banner-init)
;;      ;; fuzzy-completion
;;      (require 'slime-fuzzy)
;;      (setq slime-complete-symbol*-fancy t)
;;      (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
;;      (slime-setup)))
;; (add-hook 'clojure-mode '(lambda () (paredit-mode t)))

;; This is where slime is loaded.
;; (setq inferior-lisp-program "/usr/bin/sbcl")
;; (add-to-list 'load-path "/home/z/.sbcl/site/slime")

(add-hook 'haskell-mode-hook 'rainbow-delimiters-mode)
(require 'hindent)
(add-hook 'haskell-mode-hook #'hindent-mode)

(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.[hg]s$"  . haskell-mode)
                ("\\.hi$"     . haskell-mode)
                ("\\.l[hg]s$" . literate-haskell-mode))))
(autoload 'haskell-mode "haskell-mode"
   "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
   "Major mode for editing literate Haskell scripts." t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'turn-on-font-lock)

(autoload 'run-ghci "haskell-ghci"
  "Go to the *ghci* buffer" t nil)
(set-variable 'haskell-program-name "ghci")
(defalias 'run-haskell (quote switch-to-haskell))
(autoload 'switch-to-haskell "inf-haskell"
  "Show the inferior-haskell buffer.  Start the process if needed." t nil)

(defface ghc-face-warn
  '((((class color) (background dark)) (:bold t :underline "wheat"))
    (((class color) (background light)) (:background "LightBlue2"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'ghc)

(defface ghc-face-error
  '((((class color) (background dark)) (:background "maroon"))
    (((class color) (background light)) (:background "LightPink"))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'ghc)

;; Hakaru
; (load "/home/zv/.emacs.d/hakaru.elc")

;; (require 'multi-web-mode)
;; (setq mweb-default-major-mode 'html-mode)
;; (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;                   (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;;                   (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;; (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;; (multi-web-global-mode 1)

;; Markdown
(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)$" . markdown-mode))

;; (require 'window-number)
;; (window-number-mode)
;; (window-number-meta-mode)

;; Set up matlab-mode to load on .m files
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)

;; Customization:
(setq matlab-indent-function t)	; if you want function bodies indented
(setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
(defun my-matlab-mode-hook ()
  (setq fill-column 76))		; where auto-fill should wrap
(add-hook 'matlab-mode-hook 'my-matlab-mode-hook)
(setq matlab-shell-command-switches "-nojvm") 

;;;;Enable undoc; a mode which edits MS Word .doc files.
;;;;http://www.ccs.neu.edu/home/guttman/undoc.el
(autoload 'undoc "~/.emacs.d/undoc.el" "A minor mode which kills MS
Word files dead." t)
(autoload 'undoc-current-buffer "undoc" "" t)
(autoload 'undoc-region-after-mime-decode "undoc" "" t)
