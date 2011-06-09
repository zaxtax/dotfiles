;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(when window-system 
  (set-background-color "black")
  (set-foreground-color "green")
  (set-cursor-color "white")
  (set-face-foreground 'region "black")
  (set-face-background 'region "green")
  (set-face-foreground 'mode-line "gray15")
  (set-face-background 'mode-line "black")
  (set-face-font 'default "6x10")
  (set-face-font 'tooltip "6x10"))

(menu-bar-mode -1)
(tool-bar-mode -1)
(transient-mark-mode 1)
(setq inhibit-splash-screen t)

(setq make-backup-files nil)
(setq auto-save-default nil)

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

(defun key (desc)
  (or (and window-system (read-kbd-macro desc))
      (or (cdr (assoc desc real-keyboard-keys))
          (read-kbd-macro desc))))

(global-set-key (key "M-<left>") 'windmove-left)          ; move to left windnow
(global-set-key (key "M-<right>") 'windmove-right)        ; move to right window
(global-set-key (key "M-<up>") 'windmove-up)              ; move to upper window
(global-set-key (key "M-<down>") 'windmove-down)          ; move to downer window

(require 'sclang)
(require 'ido)
(ido-mode t)

;(iswitchb-mode 1)
(scroll-bar-mode nil)
(mouse-wheel-mode)

(when window-system
  ;; Session Saving
  ;; use only one desktop
  (setq desktop-path '("~/.emacs.d/"))
  (setq desktop-dirname "~/.emacs.d/")
  (setq desktop-base-file-name "emacs-desktop")

  ;; remove desktop after it's been read
  (add-hook 'desktop-after-read-hook
	    '(lambda ()
	       ;; desktop-remove clears desktop-dirname
	       (setq desktop-dirname-tmp desktop-dirname)
	       (desktop-remove)
	       (setq desktop-dirname desktop-dirname-tmp)))

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
	    (desktop-save-in-desktop-dir)
	  (message "Session not saved."))
      (desktop-save-in-desktop-dir)))
  
  ;; ask user whether to restore desktop at start-up
  (add-hook 'after-init-hook
	    '(lambda ()
	       (if (saved-session)
		   (if (y-or-n-p "Restore desktop? ")
		       (session-restore))))))
  

;; Dynamic Completion
(load-library "completion")                                                    
(dynamic-completion-mode) 
(global-set-key (kbd "M-<RETURN>") 'complete)

(require 'php-mode)

;; Hide/Show tag
(global-set-key (kbd "M-+") 'hs-toggle-hiding)

(defun make-backup-file-name (file-name)
  "Create the non-numeric backup file name for `file-name`."
  (require `dired)
  (if (file-exists-p "~/.backups")
      (concat (expand-file-name "~/.backups/")
	      (dired-replace-in-string "/" "|" file-name))
    (concat file-name "~")))

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                               auto-mode-alist))

(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

(require 'python)
(autoload 'python-mode "python-mode" "Mode for editing Python source files")
(add-to-list 'auto-mode-alist '("\\.py" . python-mode))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
          interpreter-mode-alist)

      python-mode-hook
      '(lambda () (progn
           (set-variable 'py-indent-offset 4)
           (set-variable 'py-smart-indentation nil)
           (set-variable 'indent-tabs-mode nil) )))

;; Agda2
;;(add-to-list 'load-path "/home/zv/custom_builds/elisp/agda")
;;(require 'agda2)

;; Planner Mode
(add-to-list 'load-path "~/custom_builds/elisp/muse/lisp")
(add-to-list 'load-path "~/custom_builds/elisp/planner")
(add-to-list 'load-path "~/custom_builds/elisp/remember")
(setq planner-project "WikiPlanner")
(setq muse-project-alist
      '(("WikiPlanner"
	 ("~/plans"   ;; Or wherever you want your planner files to be
	  :default "index"
	  :major-mode planner-mode
	  :visit-link planner-visit-link))))
(require 'planner)


(global-font-lock-mode t)
(show-paren-mode t)

;; mouse config
(mouse-wheel-mode)
;;(set-scroll-bar-mode 'right)
(setq scroll-step 1)

(require 'slime)
(eval-after-load "slime"
  '(progn
     ;; fancy slime startup
     (add-to-list 'load-path "~/.sbcl/site/slime/contrib")
     (require 'slime-banner)
     (slime-banner-init)
     ;; fuzzy-completion
     (require 'slime-fuzzy)
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
     (slime-setup)))
(add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")))
(add-hook 'clojure-mode '(lambda () (paredit-mode t)))

;; This is where slime is loaded.
;; (setq inferior-lisp-program "/usr/bin/sbcl")
;; (add-to-list 'load-path "/home/zv/.sbcl/site/slime")


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
(autoload (quote switch-to-haskell) "inf-haskell"
  "Show the inferior-haskell buffer.  Start the process if needed." t nil)

;; arc mode
;; (load "/home/fourier/programming/arc/arc0/arc-mode.el")
;; (setq auto-mode-alist (cons '("\\.arc$" . arc-mode) auto-mode-alist))

(load "/home/zv/.emacs.d/js2.elc")
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Set up matlab-mode to load on .m files
(load "/home/zv/.emacs.d/matlab.el")
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

(global-set-key [(control tab)] 'bury-buffer)

;(server-start)

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
        (setq text-mode-hook 'darkroom-mode)

)
