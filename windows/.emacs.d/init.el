;; ENCODING -------------
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)
(setq visible-bell t)

(setq scroll-step 1)
(setq scroll-margin 8)

(defvar penguin/default-font-size 110)
(defvar penguin/default-variable-font-size 110)
(defvar penguin/frame-transparency '(90 . 90))
(defvar penguin/home "C:\\Users\\es-kyeongsoo" "My home dir")
(defvar penguin/projects (concat penguin/home "\\Projects") "My Projects Directory")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Set frame transpaency
(set-frame-parameter (selected-frame) 'alpha penguin/frame-transparency)
;; (add-to-list 'default-frame-alist '(alpha . ,penguin/frame-transparency))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)

;; Minor modes
(column-number-mode)
(global-display-line-numbers-mode t)
(global-auto-revert-mode 1)

;; Fonts
;; (setq text-scale-mode-step 1.2)
;; (set-frame-font "Hack Nerd Font" nil t)
(set-face-attribute 'default nil :family "Hack Nerd Font" :height penguin/default-font-size)
;; (set-face-attribute 'default nil :family "Hack Nerd Font" :weight 'regular :height 120)

;; Interaction
(setq use-short-answers t)
(setq confirm-kill-emacs 'yes-or-no-p) ;; Confirm to quit

;; Lines
(setq-default truncate-lines t)
(setq-default tab-width 4)
(setq line-move-visual t)

(use-package paren
  ;; highlight matching delimiters
  :ensure nil
  :config
  (setq show-paren-delay 0.1
	show-paren-highlight-openparen t
	show-paren-when-point-inside-paren t
	show-paren-when-point-in-periphery t)
  (show-paren-mode 1))

(setq sentence-end-double-space nil)
(setq bookmark-set-fringe-mark nil)

;; Scrolling
(setq scroll-conservatively 101)

(setq trash-directory (concat penguin/home ".Trash"))
(setq delete-by-moving-to-trash t)

(setq-default display-line-numbers-width 3)
(setq vc-follow-symlinks t)

(setq create-lockfiles nil
      make-backup-files nil
      version-control t
      backup-by-copying t
      delete-old-versions t
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat user-emacs-directory "backup/"))))

(use-package all-the-icons)
(load-theme 'modus-vivendi)

(require 'package)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)
;; (setq package-enable-at-startup nil)
(require 'use-package)
(setq use-package-always-ensure t)
;; (setq use-package-verbose nil)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
				term-mode-hook
				shell-mode-hook
				treemacs-mode-hook
				eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0)))
  (add-hook mode (lambda () (evil-local-mode 0))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; general
(use-package general
  :after evil
  :config
  (general-create-definer penguin/leader-keys
						  :keymaps '(normal insert visual emacs)
						  :prefix "SPC"
						  :global-prefix "C-SPC")

  (penguin/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")))

(general-evil-setup)
(general-imap "j"
  (general-key-dispatch 'self-insert-command
    :timeout 0.25
    "k" 'evil-normal-state)) 

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; (define-key evil-insert-state-map (kbd "jk") 'evil-normal-state) ;; does not work as expected
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (setq-default evil-escape-key-sequence "jk")

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package command-log-mode
  :commands command-log-mode)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
		 :map ivy-minibuffer-map
		 ("TAB" . ivy-alt-done)
		 ("C-l" . ivy-alt-done)
		 ("C-j" . ivy-next-line)
		 ("C-k" . ivy-previous-line)
		 :map ivy-switch-buffer-map
		 ("C-k" . ivy-previous-line)
		 ("C-l" . ivy-done)
		 ("C-d" . ivy-switch-buffer-kill)
		 :map ivy-reverse-i-search-map
		 ("C-k" . ivy-previous-line)
		 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
		 :map minibuffer-local-map
		 ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))


(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))


(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package no-littering)

(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 200)
  (setq recentf-filename-handlers
	(append '(abbreviate-file-name) recentf-filename-handlers))
  (recentf-mode))


(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq openai-api-key (getenv "OPENAI_APIKEY"))
(use-package c3po
  :straight (:host github :repo "d1egoaz/c3po.el")
  :config
  (setq c3po-api-key openai-api-key))

;; projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :ensure t
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :init
  (when (file-directory-p penguin/projects)
	(setq projectile-project-search-path '(penguin/projects)))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))


(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;;(setq dashboard-startup-banner "~/.config/emacs/images/dtmacs-logo.png")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :custom 
  (dashboard-modify-heading-icons '((recents . "file-text")
									(bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))


(defun penguin/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))


(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . penguin/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-l")  ;; Or 'C-c l', 's-l'
  :config
  (lsp-enable-which-key-integration t))


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))


(use-package lsp-treemacs
  :after lsp)


(use-package lsp-ivy
  :after lsp)


(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed
  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))


(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))


(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))


(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))


(use-package company-box
  :hook (company-mode . company-box-mode))

