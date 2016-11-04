(setq js-indent-level 2                                   ;;; javascript-mode
      web-mode-markup-indent-offset 2
      web-mode-code-indent-offset 2
      ruby-deep-indent-paren nil                          ;;; ruby indent mode
      inhibit-splash-screen t
      uniquify-min-dir-content 2
      truncate-partial-width-windows nil
      temporary-file-directory "~/.emacs.d/saves"
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory))
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/"))
      ring-bell-function #'ignore
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      scroll-step 1
      exec-path (append exec-path '("/usr/local/bin"))
      company-minimum-prefix-length 2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(magit-branch-arguments nil)
 '(magit-gitflow-feature-finish-arguments (quote ("--fetch")))
 '(magit-gitflow-feature-start-arguments (quote ("--fetch")))
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(magit-merge-arguments (quote ("--ff-only")))
 '(magit-pull-arguments nil)
 '(magit-rebase-arguments (quote ("--interactive")))
 '(package-selected-packages
   (quote
    (visual-fill-column visual-fill-column-mode chruby multi-eshell shell-switcher exec-path-from-shell yaml-mode yagist web-mode use-package scss-mode rust-mode rspec-mode rainbow-delimiters powerline pbcopy paredit neotree multiple-cursors markdown-toc magit-gitflow lua-mode inf-ruby helm-projectile helm-ag haml-mode gotest go-eldoc git-messenger flx-ido dumb-jump dockerfile-mode cyberpunk-theme company-go color-theme-sanityinc-tomorrow coffee-mode alchemist ag ace-window ace-jump-mode)))
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 PACKAGE CONFIGURATION                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)
(require 'use-package)

;;; paredit
(use-package paredit
  :ensure t
  :commands (enable-paredit-mode)
  :bind ("C-M-u" . backward-up-list+)
  :init (progn
          (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
          (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
          (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
          (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
          (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
          (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
          ;;; Make paredit's backward-up-list handle strings
          (defun backward-up-list+ ()
            "backward-up-list doesn't work when cursor is inside a string"
            (interactive)
            (if (in-string-p)
                (while (in-string-p)
                  (backward-char))
              backward-up-list))))

;;; show-paren-mode
(use-package paren
  :ensure t
  :config (progn
            (setq show-paren-delay 0)
            (show-paren-mode 1)
            (setq show-paren-style 'mixed)))

;;; integrate with the clipboard
(use-package pbcopy
  :ensure t
  :config (turn-on-pbcopy))

;;; projectile
(use-package projectile
  :ensure t
  :commands (projectile-switch-project)
  :bind (("C-x C-f" . projectile-find-file)
	 ("C-c p p" . projectile-switch-project))
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  )

;;; helm
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-c C-f" . helm-imenu)
         ("C-x f" . helm-find-files))
  :init (helm-mode 1)
  :config (progn
            (setq helm-mode-fuzzy-match t
                  helm-completion-in-region-fuzzy-match t
                  helm-autoresize-max-height 15
                  helm-autoresize-min-height 15)
            (helm-autoresize-mode 1)
            (defun pl/helm-alive-p ()
              (if (boundp 'helm-alive-p)
                  (symbol-value 'helm-alive-p)))))

(use-package helm-projectile
  :ensure t
  :bind (
         ("C-t" . helm-projectile-find-file)
         ("C-c p s s" . helm-projectile-ag))
  :init (helm-projectile-on))

;;; flycheck
(use-package flycheck
  :ensure t
  :commands global-flycheck-mode
  :init (progn
          (global-flycheck-mode 1)
          ;; Enable flyspell-prog-mode for programming modes
          (mapcar (lambda (mode-hook) (add-hook mode-hook 'flyspell-prog-mode))
                  '(ruby-mode-hook go-mode-hook coffee-mode-hook web-mode-hook elixir-mode-hook))
          ;; Enable flyspell-mode for text modes
          (mapcar (lambda (mode-hook) (add-hook mode-hook 'flyspell-mode))
                  '(markdown-mode-hook text-mode-hook))))

;;; Themes!
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config (load-theme 'sanityinc-tomorrow-eighties t))

;;; ido-mode/flx/flx-ido
(use-package flx-ido
  :ensure t
  :config (progn
            (ido-mode 1)
            (ido-everywhere 1)
            (flx-ido-mode 1)
            (setq ido-enable-flex-matching t
                  ido-use-faces nil)))

;;; multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (
         ("C-c ." . mc/mark-all-like-this-dwim)
         ("C-c ," . mc/edit-lines)
         ("C-c /" . mc/mark-next-like-this)
         ("C-c C-c ," . mc/mark-previous-like-this)))

;;; rust mode
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

;;; powerline
(use-package powerline
  :ensure t
  :config (powerline-center-theme))

;;; ace-jump
(use-package ace-window
  :ensure t
  :bind (("C-c w" . ace-select-window)
         ("C-c d" . ace-delete-window))
  :config (ace-window-display-mode))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-j" . ace-jump-word-mode)
           ("C-c l" . ace-jump-line-mode)))

;;; yagist
(use-package yagist
  :ensure t
  :config
  (if (file-exists-p "~/.emacs.d/yagist-github.el") (load-file "~/.emacs.d/yagist-github.el")))

;;; magit
(use-package magit
  :ensure t
  :bind (("C-c C-s" . magit-status)
         ("C-c s" . magit-status)))

;;; magit-gitflow
(use-package magit-gitflow
  :ensure t
  :config (add-hook 'magit-mode-hook  #'turn-on-magit-gitflow))

;;; web-mode
(use-package web-mode
  :ensure t
  :mode (("\\.erb\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)))

;; javascript


;;; yaml-mode
(use-package yaml-mode
  :ensure t)

;;; Ruby
(use-package ruby-mode
  :ensure t)

(use-package rspec-mode
  :ensure t)

;; haml-mode
(use-package haml-mode
  :ensure t)

;; coffee-mode
(use-package coffee-mode
  :ensure t)

;;; rainbow-delimeters
(use-package rainbow-delimiters
  :ensure t)

;;;;;;;;;;
;;; Go ;;;
;;;;;;;;;;

;;; go-eldoc
(use-package go-eldoc
  :ensure t)

;;; company
(use-package company
  :ensure t
  :init (progn
          ;; Add company-mode hooks
          (mapcar (lambda (mode-hook) (add-hook mode-hook 'company-mode))
                  '(ruby-mode-hook coffee-mode-hook web-mode-hook elixir-mode-hook))))

;;; company-go

(use-package company-go
  :ensure t
  :config (progn
          (setq company-idle-delay .25)
          (setq company-echo-delay 0)))

;;; go-mode
(use-package go-mode
  :ensure t
  :bind (("C-c r" . go-remove-unused-imports))
  :config (progn
          (add-hook 'before-save-hook  #'gofmt-before-save)
          (add-hook 'go-mode-hook 'go-eldoc-setup)
          (add-hook 'go-mode-hook (lambda ()
                                    (set (make-local-variable 'company-backends) '(company-go))
                                    (company-mode)))))

;;; gotest
(use-package gotest
  :ensure t
  :bind (("C-c C-t" . go-test-current-test)
         ("C-c C-c C-f" . go-test-current-file)
         ("C-c C-p" . go-test-current-project)
         ("C-c C-r" . go-run)))

;;; dockerfile
(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

;;; markdown
(use-package markdown-mode
  :ensure t
  :mode (("\\.text\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)))

(use-package markdown-toc
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package helm-ag
  :ensure t)

(use-package ag
  :ensure t)

(use-package scss-mode
  :ensure t)

(use-package alchemist
  :ensure t
  :config (progn
            (setq alchemist-hooks-test-on-save t)
            (setq alchemist-hooks-compile-on-save t)))

;;      .-"-.            .-"-.            .-"-.
;;    _/_-.-_\_        _/.-.-.\_        _/.-.-.\_
;;   / __} {__ \      /|( o o )|\      ( ( o o ) )
;;  / //  "  \\ \    | //  "  \\ |      |/  "  \|
;; / / \'---'/ \ \  / / \'---'/ \ \      \'/^\'/
;; \ \_/`"""`\_/ /  \ \_/`"""`\_/ /      /`\ /`\
;;  \           /    \           /      /  /|\  \
;; (use-package evil
;;   :ensure t)

(use-package dumb-jump
  :ensure t
  :bind (("M-." . dumb-jump-go)
         ("M-," . dumb-jump-back)
         ("M-/" . dumb-jump-quick-look))
  :init (dumb-jump-mode))

(use-package git-messenger
  :ensure t
  :bind (("C-x v p" . git-messenger:popup-message)))

(use-package neotree
  :ensure t
  :bind (([f5] . neotree-toggle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Other Config ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; emacs quality of life
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil) ;;; use spaces by default
(add-hook 'before-save-hook 'delete-trailing-whitespace)  ;;; Delete trailing whitespace on save
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-auto-revert-mode t)                               ;;; auto-refresh files when they change on disk
(set-default 'truncate-lines t)                           ;;; disable line wrapping
(setq-default fill-column 200)
(tool-bar-mode -1)
(menu-bar-mode -1)
(fset 'html-helper-mode 'html-mode)
(global-hl-line-mode)
(setq tab-width 2)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(blink-cursor-mode 0)

;; don't show line number in the shell
(define-global-minor-mode my-global-linum-mode global-linum-mode
  (lambda ()
    (when (not (memq major-mode
                    (list 'eshell-mode 'ansi-term-mode 'term-mode)))
    (linum-mode 1))))
(my-global-linum-mode t)                                     ;;; show line numbers

;;; Display magit fullscreen
(add-to-list 'display-buffer-alist
             `(,(rx "*magit: ")
               (lunaryorn-display-buffer-fullframe)
               (reusable-frames . nil)))

(defun lunaryorn-display-buffer-fullframe (buffer alist)
  "Display BUFFER in fullscreen.
ALIST is a `display-buffer' ALIST.
Return the new window for BUFFER."
  (let ((window
         (or (display-buffer-use-some-window buffer alist)
             (display-buffer-pop-up-window buffer alist))))
    (when window
      (delete-other-windows window))
    window))
