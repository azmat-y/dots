;; -*- lexical-binding: t -*-

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; (package-initialize)

;; my functions efs, ..
(defun efs/display-startup-time ()
  (interactive)
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(defun my/activate-corfu-terminal ()
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(setq use-package-always-ensure t)

(progn (unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package))

;; (unless package-archive-contents (package-refresh-contents t))
(use-package emacs
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :init
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil)
  (setq warning-minimum-level :emergency)
  (setq gc-cons-threshold 100000000) ; allocates more 20MB for emacs than default 0.76MB so that GC doesn't run as often
  (setq sentence-end-double-space nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb Increase the amount of data which Emacs reads from the process
  (setq create-lockfiles nil)
  (setq backup-directory-alist  '((".*" . "/home/azmat/.emacs_backups")))
  (setq auth-sources '("~/.authinfo.gpg"))
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq isearch-lazy-count t)
  (setq make-backup-files nil)
  (setq package-install-upgrade-built-in t)
  (setq use-package-compute-statistics t)
  (setq compilation-scroll-output t)
  (setq compilation-auto-jump-to-next t)
  (setq compilation-max-output-line-length nil)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  (setq-default dired-listing-switches "-lha")
  (setq-default flymake-mode nil)

  (fset 'yes-or-no-p 'y-or-n-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (global-visual-line-mode 1)
  ;; (set-fringe-mode 5)
  (menu-bar-mode -1)            ; Disable the menu bar
  (electric-pair-mode)	      ; completes delimiters like ({["'"]})
  (show-paren-mode)
  ; (electric-indent-mode 1)
  ; UbuntuMonoNerdFont
  (set-face-attribute 'default nil :font "IosevkaTermNerdFontMono" :height 140)
  (add-to-list 'default-frame-alist '(font . "IosevkaTermNerdFontMono-14"))
  (global-set-key (kbd "M-[") 'universal-argument))

(use-package undo-tree
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/undo")))
  (setq undo-tree-auto-save-history t)
  :config
  (global-undo-tree-mode))


(use-package evil
  :hook (prog-mode . evil-mode)
  :bind (:map evil-insert-state-map
	      ("C-n" . nil)
	      ("C-p" . nil))
  
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-tree)

  :config
  ;; instead of normal mode launch the following in respective modes
  (dolist (p '((Info-mode . emacs)
	     (dired-mode . emacs)
	     (compilation-mode . emacs)
	     (help-mode . emacs)))
  (evil-set-initial-state (car p) (cdr p)))

  ; for using C-g to quit normal mode
  (define-key evil-insert-state-map  (kbd "C-g") #'evil-force-normal-state)
  (define-key evil-replace-state-map (kbd "C-g") #'evil-force-normal-state)
  (define-key evil-visual-state-map  (kbd "C-g") #'evil-force-normal-state)
  (define-key evil-operator-state-map (kbd "C-g") #'evil-force-normal-state))


(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

(use-package general
  :config
  (general-evil-setup t))

(use-package which-key
  :init
  (which-key-mode))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package wgrep
  :bind (:map grep-mode-map
	      ("e" . wgrep-change-to-wgrep-mode)
	      ("C-c C-c" . wgrep-finish-edit)))

(use-package vterm
  :commands (vterm))

(use-package magit
  :commands (magit-status))

;; (use-package transient)

(use-package forge :after magit)

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; The :init section is always executed.
  :init
  ;; Marginalia must be actived in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package consult)
(use-package consult-flycheck)
(use-package imenu-list)

(use-package ef-themes)
(use-package adwaita-dark-theme)
(use-package dracula-theme)

(consult-theme 'dracula)


(nvmap :states '(normal insert visual emacs) :keymaps 'override :prefix "SPC" :global-prefix "M-SPC"
  "s"   '(:ignore t :wk "search")
  "s i" '(consult-imenu :wk "consult-imenu")
  "s r" '(consult-ripgrep :wk "consult-ripgrep")
  "s f" '(consult-flycheck :wk "consult-flycheck")
  "t"   '(:ignore t :wk "toggles")
  "t i" '(imenu-list-smart-toggle :wk "imenu-list-toggle")
  "t r" '(toggle-truncate-lines :wk "toggle-truncate-lines")
  "t t" '(consult-theme :wk "consult-theme")
  "t v" '(vterm :wk "vterm")
  "b "  '(:ignore t :wk "buffer")
  "b i" '(ibuffer :wk "ibuffer")
  "b k" '(kill-this-buffer :wk "kill-buffer")
  "b l" '(previous-buffer :wk "previous-buffer")
  "b n" '(next-buffer :wk "next-buffer")
  "f  " '(:ignore t :wk "files")
  "f f" '(find-file :wk "find-file")
  "f r" '(consult-recent-file :wk "recent-files")
  "e  " '(:ignore t :wk "eval")
  "e r" '(eval-region :wk "eval-region")
  "e b" '(eval-buffer :wk "eval-buffer")
  "g  " '(:ignore t :wk "magit")
  "g s" '(magit-status :wk "magit-status")
  "g b" '(magit-blame :wk "magit-blame"))

(nvmap :states '(normal insert visual emacs) :keymaps 'override :prefix "SPC" :global-prefix "M-SPC"
  "s l" '(consult-line :wk "consult line")
  "l "  '(:ignore t :wk "lsp")
  ;; "l e" '(lsp-treemacs-errors-list :wk "errors")
  ;; "l s" '(consult-lsp-file-symbols :wk "lsp-file-symbols")
  "c c" '(compile :wk "compile")
  "o  " '(:ignore t :wk "org")
  "o c" '(org-capture :wk "org-capture")
  "o f" '(org-open-at-point :wk "org-open-at-point")
  "o a" '(org-agenda :wk "org-agenda")
  "o t" '(org-timer-set-timer :wk "org-timer"))

(use-package projectile
  :init
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-project-search-path '("/home/azmat/Programming/Projects"))
  :general-config
  (:keymap 'projectile-mode-map
	   :prefix "C-c"
	   "p" 'projectile-command-map))


(use-package embark
  :init
  (require 'bind-key)
  (bind-key "C-," #'embark-act))


(use-package  embark-consult)


(use-package surround
  :bind-keymap ("M-n" . surround-keymap))

(use-package ace-window
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (global-set-key (kbd "M-o") #'ace-window))


(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary 'separator)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'first)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 2)        ;; Use scroll margin
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0)
  :bind (:map corfu-map
	      ("M-RET" . corfu-quit)
	      ("TAB" . corfu-insert)
	      ("RET" . corfu-insert)
	      ("C-n" . corfu-next)
	      ("C-p" . corfu-previous))
  :init
  (global-corfu-mode))

;; (use-package corfu-terminal)

(use-package kind-icon
  :ensure t
  :after corfu
  ;:custom
  ; (kind-icon-blend-background t)
  ; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; enable corfu in minibuffer
(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

(use-package doom-modeline
  :init
  (setq doom-modeline-support-imenu t
	doom-modeline-buffer-file-name-style 'file-name)
  :hook (after-init . doom-modeline-mode))


;; setting up tree-sitter
(require 'treesit)
(use-package treesit
  :ensure nil
  :custom
  (treesit-language-source-alist '(;;(cpp "https://github.com/tree-sitter/tree-sitter-cpp") master branch not working for some reason
				   (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.22.0"))
				   (python "https://github.com/tree-sitter/tree-sitter-python")
				   (c "https://github.com/tree-sitter/tree-sitter-c")
				   (java "https://github.com/tree-sitter/tree-sitter-java")
				   (javascript "https://github.com/tree-sitter/tree-sitter-javascript")))
  (major-mode-remap-alist '((c++-mode . c++-ts-mode)
			    (python-mode . python-ts-mode)
			    (c-mode . c-ts-mode)
			    (java-mode . java-ts-mode)
			    (js-mode . js-ts-mode)
			    (javascript-mode . js-ts-mode)))
  (treesit-font-lock-level 4))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package flycheck
  :hook (after-init . global-flycheck-mode))

(use-package hl-todo
  :init
  (setq hl-todo-keyword-faces
	'(("TODO" . "#FF0000")))
  (global-hl-todo-mode))


(use-package eglot
  :ensure nil
  :general-config
  (:keymaps 'eglot-mode-map :prefix "C-c l"
	    "r" 'eglot-rename
	    "a" 'eglot-code-actions
	    "d" 'eglot-find-declaration
	    "h" 'eldoc-doc-buffer
	    "s" 'consult-eglot-symbols
	    "f" 'eglot-format-buffer)
  :hook ((c-ts-mode
	  c++-ts-mode
	  python-ts-mode
	  java-ts-mode)  . eglot-ensure)
  ;; :custom
  ;; (eglot-ignored-server)
  :init
  ;; Option 1: Specify explicitly to use Orderless for Eglot
  (setq completion-category-overrides '((eglot (styles orderless))
					(eglot-capf (styles orderless)))))


;; java setup https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot/
(use-package jarchive
  :after eglot
  :config
  (jarchive-setup))


;; ;; jsonrpc dependency for dape
;; (use-package jsonrpc)
;;

;; debugger setup
(use-package dape
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; :hook
  ;; ;; Save breakpoints on quit
  ;; (kill-emacs . dape-breakpoint-save)
  ;; ;; Load breakpoints on startup
  ;; (after-init . dape-breakpoint-load)

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; Info buffers like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)
  ;; (setq dape-info-hide-mode-line nil)

  ;; Pulse source line (performance hit)
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line) ;

  ;; Showing inlay hints
  (setq dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root))

;; Enable repeat mode for more ergonomic `dape' use
(use-package repeat
  :config
  (repeat-mode))

(use-package recentf
  :ensure nil
  :config
  (recentf-mode))

(use-package org-superstar
  :config
  :hook (org-mode . org-superstar-mode))


(use-package org-mode
  :ensure nil
  :hook (org-mode . ispell-minor-mode)
  :init
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-agenda-span 'week)
  (setq org-directory "~/Org")
  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers nil)
  (setq org-log-done 'time)
  (setq org-export-coding-system 'utf-8)
  (setq org-return-follows-link  t)
  (setq org-default-notes-file (concat org-directory "/agenda.org"))
  (setq org-list-indent-offset 2)
  (setq org-ellipsis " ▼")
  (setq org-startup-indented t)
  (setq org-clock-sound t)
  (setq org-agenda-files '("~/Org/agenda.org"))
  (setq org-todo-keywords
	'((sequence "TODO" "FEEDBACK" "VERIFY" "PROJECT IDEA" "|" "DONE" "SCRAPED")))

  (setq org-capture-templates
      '(("t"              ; hotkey
	 "TODO [Daily] List item" ; name
	 entry            ; type
	 ; heading type and title
	 (file+headline org-default-notes-file "Daily")
	 "* TODO %?\n %i\n")
	("i"              ; hotkey
	   "TODO List item with refrence" ; name
	   entry            ; type
	   (file+headline org-default-notes-file "Tasks")
	 "* TODO %?\n %i\n%a \n")

	("j"
	 "Journal Entry"
	 entry
	 (file+datetree "~/Org/journal.org")
	 "* Entered on %u\n %i%?"))))

;; dashboard
(use-package dashboard
  :init
  (setq dashboard-startup-banner 'logo
	dashboard-week-agenda t
	dashboard-items '((agenda . 5)
			  (bookmarks . 3)
			  (recents . 3)
			  (projects . 3)))
  :config
  (add-hook 'after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))


;; delete trailing whitespace
(use-package ws-butler
  :init
  (ws-butler-global-mode))


(use-package window
  :ensure nil
  :custom
  ; left,  top, right, bottom
  (window-sides-slots '(1 0 1 1))
  (display-buffer-alist
   '(
     (,(rx (| "*xref*"
              "*grep*"
              "*Occur*"))
      display-buffer-reuse-window
      (inhibit-same-window . nil))
     ;; Yes there is a lot of repition here I could not get `(,(rx))
     ;; expression to work for me
     ("\\*vterm\\*"
      display-buffer-in-side-window
      (side . bottom)
      (window . bottom)
      (window-height 0.60))

     ("\\*compilation\\*"
      display-buffer-in-side-window
      (side . bottom)
      (window . root)
      (window-height . 0.45))

     ("\\*lsp-help\\*"
      display-buffer-in-side-window
      (side . right)
      (window . root)
      (window-width . 60))


     ("\\*Occur\\*"
      display-buffer-in-direction
      (direction . bottom)
      (window . root)
      (window-height . 0.35))

     ("\*eldoc\w*"
      display-buffer-in-side-window
      (side . bottom)
      (window . root)
      (window-heigh . 0.80))

     ("\\*Help\\*"
      display-buffer-in-side-window
      (side . bottom)
      (window . root)
      (window-height . 0.60)))))

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-'"   . popper-toggle)
         ("M-'"   . popper-cycle)
         ("C-M-'" . popper-toggle-type))
  :init
  (setq popper-group-function #'popper-group-by-projectile)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
	  "\\*lsp-help\\*" lsp-help-mode
	  "\*eldoc\w*"
	  xref--xref-buffer-mode
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  :config
    (setq popper-display-control nil))

(use-package eglot-booster
  :vc (:fetcher github :repo  "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

;; keep customize edits separate
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)
(add-hook 'emacs-startup-hook #'my/activate-corfu-terminal)
(put 'upcase-region 'disabled nil)
