(when window-system 
   ;(set-face-font 'default "6x10")
   (set-frame-font "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
   (set-face-attribute 'default nil :height 90)
   (setq-default line-spacing 2)
   (set-face-font 'tooltip "6x10"))

(require 'cl)

;; El-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)


(if (fboundp 'gnutls-available-p)
    (fmakunbound 'gnutls-available-p))
(setq tls-program '("gnutls-cli --tofu -p %p %h")
      imap-ssl-program '("gnutls-cli --tofu -p %p %s")
      smtpmail-stream-type 'starttls
      starttls-extra-arguments '("--tofu")
      )

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;(add-to-list 'load-path "~/.emacs.d")
(require 'rainbow-delimiters)

(menu-bar-mode -1)
(tool-bar-mode -1)
(transient-mark-mode 1)
(setq inhibit-splash-screen t)
(setq column-number-mode t)

(setq x-select-enable-clipboard t)
;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(setq make-backup-files nil)
(setq auto-save-default nil)

;(setq temporary-file-directory "~/.emacs.d/tmp/")

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
    bpath
  )
)
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

(defun key (desc)
  (or (and window-system (read-kbd-macro desc))
      (or (cdr (assoc desc real-keyboard-keys))
          (read-kbd-macro desc))))

(global-set-key (key "M-<left>") 'windmove-left)          ; move to left windnow
(global-set-key (key "M-<right>") 'windmove-right)        ; move to right window
(global-set-key (key "M-<up>") 'windmove-up)              ; move to upper window
(global-set-key (key "M-<down>") 'windmove-down)          ; move to downer window

(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(setq highlight-symbol-on-navigation-p t)
(global-set-key (kbd "C-*") 'highlight-symbol-next)
(global-set-key (kbd "C-#") 'highlight-symbol-prev)

(setenv "LOCAL_MAPLE" "/home/zv/Desktop/maple2016/bin/maple")
(setenv "PATH" (concat "/home/zv/.lein/bin:"
		       "/home/zv/.local/bin:"
		       "/home/zv/upstream/android-sdk-linux/tools:"
		       "/home/zv/Desktop/maple2016/bin:"
		       "/home/zv/.cabal/bin:"
		       "/home/zv/.cargo/bin:"
		       "/sbin:/usr/sbin:"
		       (getenv "PATH")))

;; Flyspell
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

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

;; Workgroups
(require 'workgroups)
(setq wg-prefix-key (kbd "C-c w"))
(workgroups-mode 1)

;; ESS settings
(autoload 'r-mode "ess-site.el" "ESS" t)
(add-to-list 'auto-mode-alist '("\\.R$" . r-mode))

(defun read-lines (file)
  "Return a list of lines in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string
     (buffer-string) "\n" t)))

(add-hook 'ess-mode-hook
	    '(lambda()
	       (setq ess-my-extra-R-function-keywords
		     (read-lines "~/.emacs.d/R-function-names.txt"))
	       (setq ess-R-mode-font-lock-keywords
		     (append ess-R-mode-font-lock-keywords
			     (list (cons (concat "\\<" (regexp-opt
							ess-my-extra-R-function-keywords 'enc-paren) "\\>")
					 'font-lock-function-name-face))))))

;; Writegood
(add-to-list 'load-path "~/.emacs.d/writegood-mode")
(require 'writegood-mode)
(global-set-key "\C-cg" 'writegood-mode)


(require 'sclang)
(require 'ido)
(ido-mode t)

(scroll-bar-mode nil)
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
    (wg-update-all-workgroups)
    (wg-save "~/.emacs.d/workgroups")
    (if (saved-session)
	(if (y-or-n-p "Overwrite existing desktop? ")
	    (desktop-save-in-desktop-dir)
	  (message "Session not saved."))
      (desktop-save-in-desktop-dir)))

  
  ;; ask user whether to restore desktop at start-up
  (add-hook 'after-init-hook
	    '(lambda ()
	       (if (saved-session)
		   (if (y-or-n-p "Restore desktop? ")
		       (progn
			 (session-restore)
			 (org-agenda-list)
			 (wg-load "~/.emacs.d/workgroups")))))))

;; Dynamic Completion
(load-library "completion")                                                    
(dynamic-completion-mode) 
(global-set-key (kbd "M-<RETURN>") 'complete)

(cua-selection-mode 1) ;; only for rectangles

;; Hide/Show tag
(global-set-key (kbd "M-+") 'hs-toggle-hiding)

(defun make-backup-file-name (file-name)
  "Create the non-numeric backup file name for `file-name`."
  (require `dired)
  (if (file-exists-p "~/.backups")
      (concat (expand-file-name "~/.backups/")
	      (dired-replace-in-string "/" "|" file-name))
    (concat file-name "~")))

(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;; rust
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-to-list 'exec-path "/home/zv/.cargo/bin")
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(require 'python)
(autoload 'python-mode "python-mode" "Mode for editing Python source files")
(add-to-list 'auto-mode-alist '("\\.py" . python-mode))

(setq interpreter-mode-alist
      (cons '("python" . python-mode)
          interpreter-mode-alist)

      python-mode-hook
      '(lambda () (progn
           (set-variable 'py-smart-indentation t)
           (set-variable 'indent-tabs-mode nil) )))

(require 'flymake-easy)
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")

(add-to-list 'load-path "/home/zv/upstream/elisp/zenburn-emacs")
(require 'color-theme-zenburn)
(color-theme-zenburn)

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
(require 'ox-reveal)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-html-postamble nil)

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-ido-switchb)

(setq org-remember-templates
      '(("Tasks" ?t "* TODO %?\n  %i\nAdded: %U\n  %a" "~/notes/orgfiles/organizer.org")
        ("Appointments" ?a "* Appointment: %?\n%^T\n%i\n  %a" "~/notes/orgfiles/organizer.org")))
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(eval-after-load 'remember
  '(add-hook 'remember-mode-hook 'org-remember-apply-template))
(global-set-key (kbd "C-c r") 'remember)                       

(setq org-todo-keywords '((sequence "TODO" "STARTED" "WAITING" "NEXT" "|" "DONE" "DEFERRED")))

(setq org-agenda-custom-commands
    '(("w" todo "WAITING" nil) 
      ("n" todo "NEXT" nil)
      ("c" todo "DONE|DEFERRED|CANCELLED" nil)
      ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT")))))
(setq org-startup-folded nil)
(setq org-log-done t)
(setq org-agenda-include-diary t)   
(setq org-agenda-include-all-todo t)
(setq org-deadline-warning-days 30)
(setq org-agenda-todo-ignore-with-date t)

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
     (define-key org-todo-state-map "s"
       #'(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w"
       #'(lambda nil (interactive) (org-todo "WAITING")))))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-beta")

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
;; (add-to-list 'load-path "/home/zv/.sbcl/site/slime")

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

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;; Markdown
(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)$" . markdown-mode))

(require 'window-number)
(window-number-mode)
(window-number-meta-mode)

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

;;(global-set-key [(control tab)] 'bury-buffer)
(scroll-bar-mode -1)

(defun darkroom-mode ()
        (interactive)

        (set-background-color "black")
        (set-foreground-color "green")

        (set-cursor-color "white")
        (setq left-margin 10)
        (menu-bar-mode -1)
        (tool-bar-mode -1)
        (scroll-bar-mode -1)
        (transient-mark-mode 1)


        (set-face-foreground 'region "black")
        (set-face-background 'region "green")

        (set-face-foreground 'mode-line "gray15")
        (set-face-background 'mode-line "black")

        (move-to-left-margin 0 1)
        (auto-fill-mode)
        (setq text-mode-hook 'darkroom-mode)) 

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(hindent-style "johan-tibell")
 '(muse-project-alist (quote (("WikiPlanner" ("~/plans" :default "index" :major-mode planner-mode :visit-link planner-visit-link)))))
 '(org-agenda-files (quote ("~/notes/orgfiles/work.org" "~/notes/orgfiles/organizer.org")))
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 4)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))

;; Hakaru
(load "/home/zv/.emacs.d/hakaru.elc")

(defun simplify-region (&optional b e) 
  (interactive "r")
  (shell-command-on-region b e "simplify" (current-buffer) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
