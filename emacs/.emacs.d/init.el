;((load (expand-file-name "~/p/slime-helper.el"))
;(setq inferior-lisp-program "sbcl")

;;; Load packages
(setq package-enable-at-startup nil)
(package-initialize)

;;; ELSIPS
(add-to-list 'load-path "~/.emacs.d/elisp")

;;; paredit
(require 'paredit)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
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
    backward-up-list))
(global-set-key (kbd "C-M-u") 'backward-up-list+)

;;; show-paren-mode
(require 'paren)
(setq show-paren-delay 0)
(show-paren-mode 1)

;;; inf-ruby shortcut
(global-set-key (kbd "C-c r r") 'inf-ruby)

;;; integrate with the clipboard
(require 'pbcopy)
(turn-on-pbcopy)

;;; projectile/helm
(projectile-global-mode)
;(require 'helm-projectile)
;(require 'helm-config)
;(helm-projectile-on)
;(global-set-key (kbd "C-x f") 'helm-projectile)
;(global-set-key (kbd "C-c h") 'helm-mini)
;(helm-mode 1)
(global-set-key (kbd "C-x f") 'projectile-find-file)

;;; recentf-mode
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)

;;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; temp files
(setq temporary-file-directory "~/.emacs.d/saves")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq inhibit-splash-screen t)
(fset 'html-helper-mode 'html-mode)

;;; indentation
(setq-default indent-tabs-mode nil)
(setq ruby-deep-indent-paren nil)

;;; unix eol characters
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;; misc
(dolist (mode '(menu-bar-mode tool-bar-mode))
  (funcall mode -1))

(defalias 'yes-or-no-p 'y-or-n-p)

;;; Themes!
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-eighties t)

;;; Package Repos
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;;; Expand region
(require 'expand-region)
(global-set-key (kbd "M-=") 'er/expand-region)

;;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; auto-refresh files when they change on disk
(global-auto-revert-mode t)

(custom-set-variables
 ;;; If two buffers have the same name, it will append "|<dir" name> instead of "|<counter>"
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

;;; ido-mode
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to `find-file' a recent file"
  (interactive)
  (unless (find-file (ido-completing-read "Find recent file: " recentf-list))
    (message "Aborting")))

;(global-set-key (kbd "C-x f") 'ido-recentf-open)

;;; eshell fun
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-M-e") 'eshell-here)

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

;;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c .")     'mc/mark-next-like-this)
(global-set-key (kbd "C-c ,")     'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-c .") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-c ,") 'mc/edit-lines)

;;; magit
(global-set-key (kbd "C-c s") 'magit-status)


;;; javascript-mode
(setq js-indent-level 2)
