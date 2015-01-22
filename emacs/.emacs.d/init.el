;((load (expand-file-name "~/p/slime-helper.el"))

(add-to-list 'load-path "~/.emacs.d/elisp")               ;;; ELSIPS
(fset 'html-helper-mode 'html-mode)
(setq-default indent-tabs-mode nil)                       ;;; indentation
(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "C-o") 'other-window)
(global-linum-mode t)                                     ;;; show line numbers
(add-hook 'before-save-hook 'delete-trailing-whitespace)  ;;; Delete trailing whitespace on save
(global-auto-revert-mode t)                               ;;; auto-refresh files when they change on disk
(global-set-key (kbd "C-c C-s") 'magit-status)            ;;; magit keybind
(setq js-indent-level 2                                   ;;; javascript-mode
      ruby-deep-indent-paren nil                          ;;; ruby indent mode
      inhibit-splash-screen t
      temporary-file-directory "~/.emacs.d/saves"
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory))
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/"))
      inferior-lisp-program "sbcl"
      )

;;; If two buffers have the same name, it will append "|<dir" name> instead of "|<counter>"
(custom-set-variables
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 PACKAGE CONFIGURATION                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)
(require 'use-package)

;;; disable toolbar and menu bar
(menu-bar-mode -1)
(tool-bar-mode -1)

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
  :idle (progn
            (setq show-paren-delay 0)
            (show-paren-mode 1)))

;;; integrate with the clipboard
(use-package pbcopy
  :ensure t
  :idle (turn-on-pbcopy))

;;; projectile/helm
(use-package projectile
  :ensure t
  :commands (projectile-switch-project)
  :bind (("C-x C-f" . projectile-find-file)
         ("C-t" . projectile-find-file))
  :init (projectile-global-mode))

;;; flycheck
(use-package
  :ensure t
  :commands global-flycheck-mode
  :idle (global-flycheck-mode 1))

;;; unix eol characters
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;; Themes!
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init (load-theme 'sanityinc-tomorrow-eighties t))

;;; ido-mode
(use-package flx-ido
  :bind ("C-x f" . ido-find-file)
  :idle (progn
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

;;; smex
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :idle (progn
          (setq smex-flex-matching t)
          (smex-initialize)))

;;; powerline
(use-package powerline
  :ensure t
  :init (powerline-default-theme))
