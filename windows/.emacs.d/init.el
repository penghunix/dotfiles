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

(defvar penguin/default-font-size 100)
(defvar penguin/default-variable-font-size 100)
(defvar penguin/frame-transparency '(90 . 90))
(defvar penguin/home (concat (getenv "HOME") "/") "My home dir")

;; Set frame transpaency
(set-frame-parameter (selected-frame) 'alpha penguin/frame-transparency)
(add-to-list 'default-frame-alist '(alpha . penguin/frame-transparency))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)

;; Minor modes
(column-number-mode)
(global-display-line-numbers-mode t)
(global-auto-revert-mode 1)

;; Fonts
(setq text-scale-mode-step 1.2)
(set-frame-font "0xProto Nerd Font" nil t)
(set-face-attribute 'default nil :family "0xProto Nerd Font" :weight 'regular :height 100)

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

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
				term-mode-hook
				shell-mode-hook
				treemacs-mode-hook
				eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0)))
  (add-hook mode (lambda () (evil-mode 0))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

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

