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
    company-idle-delay 0.0))

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

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package flx
  :ensure t)

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 nil
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        t
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package dumb-jump
  :ensure t
  ;; :bind (("M-." . dumb-jump-go)
  ;;        ("M-," . dumb-jump-back)
  ;;        ("M-/" . dumb-jump-quick-look))
  :init (dumb-jump-mode)
  :config
  (setq
    dumb-jump-selector 'ivy))

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
          ("\\.html\\'" . web-mode)
          ("\\.js\\'" . web-mode)
          ("\\.jsx\\'" . web-mode)))

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

(use-package smartparens
  :ensure t
  :diminish
  :config
  (smartparens-global-mode))

;; Move emacs package tracking out of init.el
(setq custom-file "~/.emacs.d/package-selected-packages.el")
(load custom-file)
