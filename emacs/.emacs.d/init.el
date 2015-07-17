;((load (expand-file-name "~/p/slime-helper.el"))

(add-to-list 'load-path "~/.emacs.d/elisp")               ;;; ELSIPS
(fset 'html-helper-mode 'html-mode)
(setq-default indent-tabs-mode nil)                       ;;; indentation
(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-linum-mode t)                                     ;;; show line numbers
(add-hook 'before-save-hook 'delete-trailing-whitespace)  ;;; Delete trailing whitespace on save
(global-auto-revert-mode t)                               ;;; auto-refresh files when they change on disk
(set-default 'truncate-lines t)                           ;;; disable line wrapping
(tool-bar-mode -1)
(toggle-scroll-bar -1)

(setq js-indent-level 2                                   ;;; javascript-mode
      ruby-deep-indent-paren nil                          ;;; ruby indent mode
      inhibit-splash-screen t
      uniquify-min-dir-content 2
      truncate-partial-width-windows nil
      temporary-file-directory "~/.emacs.d/saves"
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory))
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/"))
      inferior-lisp-program "sbcl"
      mac-command-modifier 'control
      mac-control-modifier 'meta
      ring-bell-function #'ignore
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      scroll-step 1
      exec-path (append exec-path '("/usr/local/bin"))
      projectile-file-exists-remote-cache-expire (* 10 60)
      projectile-enable-caching t)

;;; If two buffers have the same name, it will append "|<dir" name> instead of "|<counter>"
(custom-set-variables
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(coffee-tab-width 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 PACKAGE CONFIGURATION                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)
(require 'use-package)

;;; disable toolbar and menu bar
(menu-bar-mode -1)

;;; hilight current line
(global-hl-line-mode)

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
  :init (progn
            (setq show-paren-delay 0)
            (show-paren-mode 1)))

;;; integrate with the clipboard
(use-package pbcopy
  :ensure t
  :init (turn-on-pbcopy))

;;; projectile
(use-package projectile
  :ensure t
  :commands (projectile-switch-project)
  :bind (("C-x C-f" . projectile-find-file)
         ("C-t" . projectile-find-file))
  :init (progn (projectile-global-mode)
               (setq projectile-completion-system 'helm)))

;;; helm
(use-package helm
  :ensure t
  :bind ("M-x" . helm-M-x)
  :init (progn (setq helm-locate-fuzzy-match t
                     helm-M-x-fuzzy-match t)))

(use-package helm-projectile
  :ensure t
  :init (helm-projectile-on))

;;; flycheck
(use-package flycheck
  :ensure t
  :commands global-flycheck-mode
  :init (global-flycheck-mode 1))

;;; Themes!
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init (load-theme 'sanityinc-tomorrow-eighties t))

;;; ido-mode/flx/flx-ido
(use-package flx-ido
  :ensure t
  :bind ("C-x f" . ido-find-file)
  :init (progn
          (ido-mode 1)
          (ido-everywhere 1)
          (flx-ido-mode 1)
          (setq ido-enable-flex-matching t
                  ido-use-faces nil)))

;;; multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (
         ("C-c ." . mc/mark-next-like-this)
         ("C-c ," . mc/mark-previous-like-this)
         ("C-c C-c ." . mc/mark-all-like-this)
         ("C-c C-c ," . mc/edit-lines)))

;;; rust mode
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

;;; powerline
(use-package powerline
  :ensure t
  :init (powerline-center-theme))

;;; ace-jump
(use-package ace-jump-mode
  :ensure t
  :bind (("C-j" . ace-jump-word-mode)))

;;; yagist
(use-package yagist
  :ensure t)

;;; magit
(use-package magit
  :ensure t
  :bind (("C-c C-s" . magit-status)
         ("C-c s" . magit-status)))

;;; web-mode
(use-package web-mode
  :ensure t)

;;; yaml-mode
(use-package yaml-mode
  :ensure t)

;;; ruby-mode
(use-package ruby-mode
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

;;; company-go

(use-package company
  :ensure t)

(use-package company-go
  :ensure t
  :init (progn
          (setq company-idle-delay .25)
          (setq company-echo-delay 0)))

;;; go-mode
(use-package go-mode
  :ensure t
  :bind (("C-c C-r" . go-remove-unused-imports)
         ("M-." . godef-jump))
  :init (progn
          (add-hook 'before-save-hook  #'gofmt-before-save)
          (add-hook 'go-mode-hook 'go-eldoc-setup)
          (add-hook 'go-mode-hook (lambda ()
                      (set (make-local-variable 'company-backends) '(company-go))
                      (company-mode)))))
