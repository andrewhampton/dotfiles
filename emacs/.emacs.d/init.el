(setq
  ;; Don't show splash on boot
  inhibit-splash-screen t

  ;; Let tab fix indentation
  evil-want-C-i-jump nil

  ;; Add gnu and melpa package repositories
  package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/"))

  ;; reduce the frequency of garbage collection by making it happen on
  ;; each 50MB of allocated data (the default is on every 0.76MB)
  gc-cons-threshold 50000000

  ;; keep temporary files in the emacs folder
  temporary-file-directory "~/.emacs.d/saves"
  auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
  backup-directory-alist `((".*" . ,temporary-file-directory))

  ;; increase the size of recentf
  recentf-max-menu-items 50
  recentf-max-saved-items 50

  ;; Don't insert magic utf-8 comments for ruby
  ruby-insert-encoding-magic-comment nil

  ;; Use native compilation for .elc files
  ;; https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
  comp-deferred-compilation t

  ;; Bump how much emacs can read from processes so LSP mode will be happy
  read-process-output-max (* 1024 1024) ;; 1mb
  )

;;; disable line wrapping
(set-default 'truncate-lines t)

;;; Turn off menus
(menu-bar-mode -1)

;;; Better query-replace shortcut
(define-key global-map (kbd "C-c r") 'query-replace)

;;; Use emacs 26 line numbers
(global-display-line-numbers-mode)

(recentf-mode 1)
;;; Use counsel-M-x instead of the default
;; (global-set-key (kbd "M-x") 'counsel-M-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 PACKAGE CONFIGURATION                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)

;;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package diminish
  :ensure t)

;;; show-paren-mode
(use-package paren
  :ensure t
  :config
  (setq
    show-paren-delay 0
    show-paren-style 'parenthesis)
  (show-paren-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language Server Protocol ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :diminish
  :config
  (setq
    company-minimum-prefix-length 1
    company-idle-delay 0.0)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package lsp-mode
  :ensure t
  :hook (
          (ruby-mode . lsp)
          (typescript-mode . lsp))
  :commands lsp
  :config
  (setq
    lsp-completion-provider :capf
    lsp-idle-delay 0.15
    lsp-modeline-diagnostics-enable t))

;;      .-"-.            .-"-.            .-"-.
;;    _/_-.-_\_        _/.-.-.\_        _/.-.-.\_
;;   / __} {__ \      /|( o o )|\      ( ( o o ) )
;;  / //  "  \\ \    | //  "  \\ |      |/  "  \|
;; / / \'---'/ \ \  / / \'---'/ \ \      \'/^\'/
;; \ \_/`"""`\_/ /  \ \_/`"""`\_/ /      /`\ /`\
;;  \           /    \           /      /  /|\  \
(use-package evil
  :ensure t
  :diminish (undo-tree-mode eldoc-mode)
  :init
  (setq
    evil-want-integration t
    evil-want-keybinding nil)
  :config
  (evil-mode 1)

  ;; Ensure counsel-projectile C-t isn't overridden
  (dolist (map '(evil-motion-state-map
                  evil-insert-state-map
                  evil-normal-state-map))
    (define-key (eval map) "\C-t" nil))

  (setq
    evil-normal-state-cursor '(box "light blue")
    evil-insert-state-cursor '(bar "medium sea green")
    evil-visual-state-cursor '(hollow "orange"))

  (use-package evil-collection
    :ensure t
    :config
    (evil-collection-init))

  ;; Leader config
  (defvar my-leader-map (make-sparse-keymap)
    "Keymap for \"leader key\" shortcuts.")
  (define-key evil-normal-state-map (kbd "SPC") my-leader-map)
  (define-key my-leader-map "b" 'list-buffers)
  (define-key my-leader-map "tt" 'treemacs)
  (define-key my-leader-map "tf" 'treemacs-find-file)
  (define-key my-leader-map "tp" 'treemacs-switch-workspace)
  (define-key my-leader-map "ff" 'counsel-git)
  (define-key my-leader-map "fs" 'counsel-git-grep)
  (define-key my-leader-map "fr" 'counsel-recentf)
  (define-key my-leader-map "fe" 'lsp-treemacs-errors-list)
  (define-key my-leader-map "fb" 'bookmark-jump)
  (define-key my-leader-map "cn" 'flycheck-next-error)
  (define-key my-leader-map "cp" 'flycheck-previous-error))

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config (load-theme 'sanityinc-tomorrow-eighties t))

(use-package dracula-theme
  :ensure t
  :config (load-theme 'dracula t))

;;;;;;;;;;;;;;;;
;;; File nav ;;;
;;;;;;;;;;;;;;;;
(use-package ivy
  :ensure t
  :diminish
  :bind (("C-x b" . ivy-switch-buffer)
          ("C-c C-r" . ivy-resume))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
    ivy-count-format "(%d/%d) "
    ivy-re-builders-alist '((t . ivy--regex-plus))))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
          ("M-y" . counsel-yank-pop)
          ("C-t" . counsel-git)
          ("C-x f" . counsel-find-file))
  :init
  (counsel-mode 1))

(use-package selectrum
  :ensure t
  :after (prescient)
  :config
  (selectrum-mode +1))

;; Sorting library
(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode +1))

(use-package selectrum-prescient
  :ensure t
  :config
  (selectrum-prescient-mode +1))

(use-package company-prescient
  :ensure t
  :hook (company-mode . company-prescient-mode))

(use-package ivy-prescient
  :ensure t
  :config
  (ivy-prescient-mode +1))

(use-package ctrlf
  :ensure t
  :config
  (ctrlf-mode +1))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

(use-package dumb-jump
  :ensure t
  :init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package deadgrep
  :ensure t
  :after (evil-collection)
  :bind (("C-c C-k" . deadgrep)))

;;;;;;;;;;;;;
;;; Magit ;;;
;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :bind (("C-c C-s" . magit-status)
          ("C-c s" . magit-status))
  :init
  (setq magit-diff-refine-hunk t
    git-commit-summary-max-length 72
    fill-column 72)
  :config
  ; Fill the full frame
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  (use-package forge
    :ensure t))

(use-package evil-magit
  :ensure t
  :after (evil magit))

;;;;;;;;;;;;;;;;;
;;; Languages ;;;
;;;;;;;;;;;;;;;;;

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode))
  :config
  (setq typescript-indent-level 2))

(use-package yaml-mode
  :ensure t)

(use-package haml-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode (("\\.erb\\'" . web-mode)
          ("\\.html\\'" . web-mode)))

(use-package sqlformat
  :ensure t
  :init
  (setq sqlformat-command 'pgformatter))

;; dotenv mode
(use-package dotenv-mode
  :ensure t)

;;; dockerfile
(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

;;;;;;;;;;;;
;;; Misc ;;;
;;;;;;;;;;;;
;;; flycheck
(use-package flycheck
  :ensure t
  :commands (global-flycheck-mode)
  :hook ((markdown-mode text-mode) . flyspell-mode)
  :init
  (global-flycheck-mode 1)
  (setq flycheck-highlighting-mode 'lines
    flycheck-highlighting-style 'level-face))

;;; Integrate with osx clipboard
(use-package osx-clipboard
  :ensure t
  :diminish osx-clipboard-mode
  :config
  (osx-clipboard-mode +1))

;; Use editorconfig to clean up files
(use-package editorconfig
  :ensure t
  :diminish
  :config
  (editorconfig-mode 1))

;; Roam
(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org")
  :bind (:map org-roam-mode-map
          (("C-c n l" . org-roam)
            ("C-c n f" . org-roam-find-file)
            ("C-c n g" . org-roam-graph-show))
          :map org-mode-map
          (("C-c n i" . org-roam-insert))
          (("C-c n I" . org-roam-insert-immediate))))

(use-package markdown-toc
  :ensure t)

;; Move emacs package tracking out of init.el
(setq custom-file "~/.emacs.d/package-selected-packages.el")
(load custom-file)
