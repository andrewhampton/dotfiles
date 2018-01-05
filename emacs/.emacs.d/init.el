(setq js-indent-level 2                                   ;;; javascript-mode
      web-mode-markup-indent-offset 2
      web-mode-code-indent-offset 2
      ruby-deep-indent-paren nil                          ;;; ruby indent mode
      css-indent-offset 2
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
      ;exec-path (append exec-path '("/usr/local/bin"))
      mac-command-modifier 'control
      mac-control-modifier 'meta
      company-minimum-prefix-length 2
      org-directory "~/org"
      org-default-notes-file (concat org-directory "/notes.org")
      org-agenda-files '("~/org/notes.org"))

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
    (fzf dired-subtree color-theme-oblivion evil elfeed dracula-theme typescript-mode add-node-modules-path browse-at-remote browser-at-remote nyan-mode spaceline-config spaceline counsel-projectile counsel yaml-mode yagist which-key web-mode visual-fill-column use-package swiper shell-switcher scss-mode rust-mode rspec-mode rainbow-delimiters powerline pbcopy paredit neotree multiple-cursors multi-eshell markdown-toc magit-gitflow lua-mode inf-ruby helm-swoop helm-projectile helm-ag haml-mode gotest go-eldoc git-messenger flycheck flx-ido exec-path-from-shell dumb-jump dockerfile-mode cyberpunk-theme company-go color-theme-sanityinc-tomorrow coffee-mode chruby alchemist ag ace-window)))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

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

;;; projectile
(use-package projectile
  :ensure t
  :commands (projectile-switch-project)
  :bind (("C-t" . projectile-find-file)
	 ("C-c p p" . projectile-switch-project))
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

;;; ivy
(use-package ivy
  :ensure t
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer))
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-height 10
        magit-completing-read-function 'ivy-completing-read
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x f" . counsel-find-file)
         ("C-c f" . counsel-imenu))
  :init
  (counsel-mode 1))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package counsel-projectile
  :ensure t
  :bind (("C-c k" . counsel-projectile-ag))
  :init (counsel-projectile-on))


;;; flycheck
(use-package flycheck
  :ensure t
  :commands global-flycheck-mode
  :init
  (global-flycheck-mode 1)
  ;; Enable flyspell-mode for text modes
  (mapcar (lambda (mode-hook) (add-hook mode-hook 'flyspell-mode))
          '(markdown-mode-hook text-mode-hook))
  (setq flycheck-highlighting-mode 'columns)
  (set-face-attribute 'flycheck-error nil :background "pink" :foreground "black"
                      ))

;;; Themes!
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config (load-theme 'sanityinc-tomorrow-eighties t))

;;; ido-mode/flx/flx-ido
;; (use-package flx-ido
;;   :ensure t
;;   :config (progn
;;             (ido-mode 1)
;;             (ido-everywhere 1)
;;             (flx-ido-mode 1)
;;             (setq ido-enable-flex-matching t
;;                   ido-use-faces nil)))

;;; multiple cursors
;; (use-package multiple-cursors
;;   :ensure t
;;   :bind (
;;          ("C-c ." . mc/mark-all-like-this-dwim)
;;          ("C-c ," . mc/edit-lines)
;;          ("C-c /" . mc/mark-next-like-this)
;;          ("C-c C-c ," . mc/mark-previous-like-this)))

;;; rust mode
;; (use-package rust-mode
;;   :ensure t
;;   :mode "\\.rs\\'")

;;; powerline
;; (use-package powerline
;;   :ensure t
;;   ;; :config (powerline-center-theme)
;;   )

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

;;; ace-jump
(use-package ace-window
  :ensure t
  :bind (("C-c w" . ace-select-window)
         ("C-c d" . ace-delete-window))
  :config (ace-window-display-mode))

;;; yagist
(use-package yagist
  :ensure t
  :config
  (if (file-exists-p "~/.emacs.d/yagist-github.el") (load-file "~/.emacs.d/yagist-github.el")))

;;; magit
(use-package magit
  :ensure t
  :bind (("C-c C-s" . magit-status)
         ("C-c s" . magit-status))
  :init
  (setq magit-diff-refine-hunk t
        git-commit-summary-max-length 72
        git-commit-fill-column 72))

;;; magit-gitflow
;; (use-package magit-gitflow
;;   :ensure t
;;   :config (add-hook 'magit-mode-hook  #'turn-on-magit-gitflow))

;;; web-mode
(use-package web-mode
  :ensure t
  :mode (("\\.erb\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)))

;;; yaml-mode
(use-package yaml-mode
  :ensure t)

;;; Ruby
(use-package ruby-mode
  :ensure t
  :config (add-hook 'ruby-mode-hook  #'hs-minor-mode))

(use-package rspec-mode
  :ensure t)

;; haml-mode
(use-package haml-mode
  :ensure t)

;; coffee-mode
(use-package coffee-mode
  :ensure t)

;; ;;; rainbow-delimeters
;; (use-package rainbow-delimiters
;;   :ensure t)

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
;; (use-package gotest
;;   :ensure t
;;   :bind (("C-c C-t" . go-test-current-test)
;;          ("C-c C-c C-f" . go-test-current-file)
;;          ("C-c C-p" . go-test-current-project)
;;          ;("C-c C-r" . go-run)
;;          ))

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

;; (use-package git-messenger
;;   :ensure t
;;   :bind (("C-x v p" . git-messenger:popup-message)))

;; (use-package neotree
;;   :ensure t
;;   :bind (([f5] . neotree-toggle)))

(use-package chruby
  :ensure t
  :init (add-hook 'flycheck-before-syntax-check-hook
                  (lambda ()
                    (if (equal major-mode 'ruby-mode)
                        (chruby-use-corresponding)))))

;; (use-package which-key
;;   :ensure t
;;   :init (progn
;;           (which-key-mode)
;;           (which-key-setup-side-window-bottom)))

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package browse-at-remote
  :ensure t
  :bind ("C-c g g" . browse-at-remote))

;; Javascript

;; add-node-modules-path add's node_modules/.bin to the path when node_modules
;; is present. This is to make sure fly-check knows about the eslint.
(use-package add-node-modules-path
  :ensure t
  :init
  (mapcar (lambda (mode-hook) (add-hook mode-hook 'add-node-modules-path))
                  '(js-mode-hook coffee-mode-hook)))

;; (use-package elfeed
;;   :ensure t
;;   :bind ("C-x w" . elfeed)
;;   :init
;;   (setq
;;    elfeed-feeds '("http://xkcd.com/rss.xml" "http://what-if.xkcd.com/feed.atom" "http://timharford.com/feed/" "http://feeds.feedburner.com/TheWirecutter" "http://9-eyes.com/rss" "http://feeds.feedburner.com/LayeredThoughts" "http://feeds.feedburner.com/thesweethome/NpUt" "http://fortydaysofdating.com/feed/" "https://unsplash.com/rss" "http://www.track-trump.com/feed.rss" "http://www.str.org/podcast/weekly" "https://www.govtrack.us/events/events.rss?feeds=p%3A412485" "https://www.govtrack.us/events/events.rss?feeds=p%3A412582" "https://www.govtrack.us/events/events.rss?feeds=p%3A412321" "http://feeds.feedburner.com/thechangelog" "http://www.joelonsoftware.com/rss.xml" "http://feeds.feedburner.com/IgnoreTheCode" "http://feeds.feedburner.com/JohnResig" "http://prog21.dadgum.com/atom.xml" "http://feeds.feedburner.com/JavascriptJavascript" "http://feeds.feedburner.com/PerfectionKills" "http://feeds.feedburner.com/paul-irish" "http://feeds.feedburner.com/uxmovement" "http://feeds.feedburner.com/codinghorror" "http://www.aaronsw.com/2002/feeds/pgessays.rss" "http://feeds.feedburner.com/Ruby5" "http://martinfowler.com/feed.atom" "http://feeds.feedburner.com/AVc" "http://steveblank.com/feed/" "http://steve-yegge.blogspot.com/feeds/posts/default?alt=rss" "http://web-design-weekly.com/feed/" "http://www.phoboslab.org/log/feed" "http://software.ericsink.com/rss.xml" "http://scribbling.net/feed/" "https://signalvnoise.com/posts.rss" "http://macro.ycombinator.com/feed.xml" "http://feeds.feedburner.com/html5rocks" "http://feeds.feedburner.com/PlataformaBlog" "http://www.masteringemacs.org/feed/" "http://reefpoints.dockyard.com/atom.xml" "http://blog.golang.org/feed.atom" "http://tjake.github.io/atom.xml" "http://feeds.feedburner.com/jquery/" "http://blog.docker.io/feed/" "http://maiagame.com/feed/" "http://www.d3noob.org/feeds/posts/default?alt=rss" "http://endlessparentheses.com/atom.xml" "http://www.planetnodejs.com/feed" "http://www.lunaryorn.com/feed.atom" "https://medium.com/feed/@benbjohnson" "https://www.hashicorp.com/feed.xml" "http://kubernetesio.blogspot.com/feeds/posts/default" "http://nathanleclaire.com/index.xml" "http://rspec.info/blog/feed.xml" "http://pragmaticemacs.com/feed/" "http://marcio.io/index.xml" "http://malisper.me/feed/" "http://kamalmarhubi.com/blog/feed.xml" "https://blog.gopheracademy.com/index.xml" "http://distill.pub/rss.xml" "http://golangweekly.com/rss/2067m4b1" "https://increment.com/feed.xml")
;;    url-queue-timeout 30))

;; (use-package dired-subtree
;;   :ensure t
;;   :config
;;   (bind-keys :map dired-mode-map
;;              ("i" . dired-subtree-insert)
;;              (";" . dired-subtree-remove)))

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
(setq-default fill-column 80)
(tool-bar-mode -1)
(menu-bar-mode -1)
(fset 'html-helper-mode 'html-mode)
(global-hl-line-mode)
(setq tab-width 2)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(blink-cursor-mode 0)

;; don't show line numbers everywhere
(add-hook 'after-change-major-mode-hook
          '(lambda ()
             (linum-mode (if (memq major-mode '(eshell-mode
                                                ansi-term-mode
                                                term-mode
                                                magit-mode
                                                magit-status-mode
                                                magit-cherry-mode
                                                magit-log-select-mode
                                                magit-reflog-mode
                                                magit-refs-mode
                                                magit-revision-mode
                                                magit-stash-mode
                                                magit-stashes-mode
                                                magit-diff-mode
                                                magit-log-mode
                                                text-mode
                                                fundemental-mode)) 0 1))))

(global-set-key (kbd "M-SPC") 'set-mark-command)

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
             (Display-buffer-pop-up-window buffer alist))))
    (when window
      (delete-other-windows window))
    window))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))


;; function to capture a todo
(defun ah/org-capture-todo ()
  (interactive)
  "Capture a TODO item"
  (org-capture nil "t"))
(define-key global-map (kbd "C-x 9") 'ah/org-capture-todo)

(define-key global-map (kbd "C-c r") 'query-replace)

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
                `(ruby-mode
                  ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
                  ,(rx (or "}" "]" "end"))                       ; Block end
                  ,(rx (or "#" "=begin"))                        ; Comment start
                  ruby-forward-sexp nil)))

(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))

(define-key global-map (kbd "C-c m") 'toggle-maximize-buffer)


(global-set-key (kbd "C-c h") 'hs-hide-block)
(global-set-key (kbd "C-c C-h") 'hs-show-block)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
