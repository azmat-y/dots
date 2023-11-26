(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(visual-line-mode 1)


(menu-bar-mode -1)            ; Disable the menu bar
(electric-pair-mode)	      ; completes delimiters like ({["'"]})
(electric-indent-mode 1)

(dolist (mode '(term-mode-hook
		vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq warning-minimum-level :emergency)
;JetBrainsMonoNerdFont
(set-face-attribute 'default nil :font "JetBrainsMonoNerdFont" :height 120)
(setq default-frame-alist '((font  . "JetBrainsMonoNerdFont")))

(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(column-number-mode)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
 (package-refresh-contents t))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package doom-themes)

(use-package nerd-icons)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package evil
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

(use-package general
  :config
  (general-evil-setup t))

(use-package which-key)
(which-key-mode)

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vterm)
(use-package magit)
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
(use-package imenu-list)

; (consult-theme 'doom-solarized-dark-high-contrast)
; (consult-theme 'wombat)
(consult-theme 'doom-monokai-pro)

;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(nvmap :states '(normal insert visual emacs) :keymaps 'override :prefix "SPC" :global-prefix "M-SPC"
  "s"   '(:ignore t :wk "search")
  "s i" '(consult-imenu :wk "consult-imenu")
  "s r" '(consult-ripgrep :wk "consult-ripgrep")
  "s f" '(consult-flymake :wk "consult-flymake")
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
  "f r" '(recentf-open-files :wk "recent-files")
  "e  " '(:ignore t :wk "eval")
  "e r" '(eval-region :wk "eval-region")
  "e b" '(eval-buffer :wk "eval-buffer")
  "g  " '(magit-status :wk "magit-status")
  )

(use-package projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

; for using C-g to quit normal mode

(define-key evil-insert-state-map  (kbd "C-g") #'evil-force-normal-state)
(define-key evil-replace-state-map (kbd "C-g") #'evil-force-normal-state)
(define-key evil-visual-state-map  (kbd "C-g") #'evil-force-normal-state)
(define-key evil-operator-state-map (kbd "C-g") #'evil-force-normal-state)

(define-key evil-insert-state-map (kbd "<return>") 'newline)

(use-package embark)
(use-package embark-consult)

(require 'bind-key)
(bind-key "C-," #'embark-act)

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode))


(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-lens-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-code-actions-segments '(count icon name))
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ((c-mode c++-mode python-mode). lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred)

(setq c-default-style "k&r")
(setq-default c-basic-offset 2)

(use-package lsp-pyright
  :hook (python-mode . (lambda() (require 'lsp-pyright)
			 (lsp))))
(use-package dap-mode
:after lsp-mode
:commands dap-debug
:hook ((python-mode . dap-ui-mode) (python-mode . dap-mode))
:config
(require 'dap-gdb-lldb)
(require 'dap-python)
(setq dap-lldb-debug-program "/usr/bin/lldb-vscode")
(dap-register-debug-template
  "LLDB::Run"
  (list :type "lldb-mi"
        :request "launch"
        :name "LLDB::Run"
        :target nil
        :cwd nil))
(setq dap-python-debugger 'debugpy)
;; (defun dap-python--pyenv-executable-find (command)
;;   (with-venv (executable-find "python")))

(add-hook 'dap-stopped-hook
	  (lambda (arg) (call-interactively #'dap-hydra))))

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.0)
  (setq company-tooltip-offset-display 'lines)
  )
(global-company-mode)

(use-package lsp-treemacs)

(use-package yasnippet)
(use-package yasnippet-snippets)
(yas-global-mode 1)

(use-package consult-lsp)
(define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)

(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(use-package evil-anzu)
(with-eval-after-load 'evil
  (require 'evil-anzu)
 ; (setq anzu-mode 1)
  )
(add-hook 'prog-mode-hook (lambda () (anzu-mode 1)))

(nvmap :states '(normal insert visual emacs) :keymaps 'override :prefix "SPC" :global-prefix "M-SPC"
  "E  " '(embark-act :wk "embark-act")
  "c  " '(:ignroe t :wk "code")
;  "c l" '(lsp-keymap-prefix :wk "lsp")
  "s l" '(consult-line :wk "consult line")


  "l s" '(consult-lsp-file-symbols :wk "lsp-file-symbols")
  "c c" '(compile :wk "compile")
  "o  " '(:ignore t :wk "org")
  "o c" '(org-capture :wk "org-capture")
  "o f" '(org-open-at-point :wk "org-open-at-point")
  "o a" '(org-agenda :wk "org-agenda")
)


(winner-mode 1)
(nvmap :states '(normal insert visual emacs) :keymaps 'override :prefix "SPC" :global-prefix "M-SPC"
       ;; Window SplitS
       "w c"   '(evil-window-delete :which-key "Close window")
       "w n"   '(evil-window-new :which-key "New window")
       "w s"   '(evil-window-split :which-key "Horizontal split window")
       "w v"   '(evil-window-vsplit :which-key "Vertical split window")
       ;; Window MotionS
       "w h"   '(evil-window-left :which-key "Window left")
       "w j"   '(evil-window-down :which-key "Window down")
       "w k"   '(evil-window-up :which-key "Window up")
       "w l"   '(evil-window-right :which-key "Window right")
       "w w"   '(evil-window-next :which-key "Goto next window")
       ;; winner mode
       "w <left>"  '(winner-undo :which-key "Winner undo")
       "w <right>" '(winner-redo :which-key "Winner redo"))


(use-package helpful
  ;; :custom
  ;; (counsel-describe-function-function #'helpful-callable)
  ;; (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package tree-sitter)
(use-package tree-sitter-langs)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode 0))))

(use-package command-log-mode
  :commands command-log-mode)

(setq smerge-command-prefix "\C-cv")

(hl-line-mode)
(setq gc-cons-threshold 20000000) ; allocates more 20MB for emacs than default 0.76MB so that GC doesn't run as often
(setq sentence-end-double-space nil)
(setq-default dired-listing-switches "-alh")
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(setq show-paren-delay 0.0)
(setq visible-bell t)
(setq create-lockfiles nil)
(setq mouse-yank-at-point t)
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;remove trailing whitespace on save
;; (setq tab-always-indent 'complete)
;; (add-to-list 'completion-styles 'initials t)
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0)

(setq org-agenda-span 'week)
(setq org-directory "~/Org")
(setq org-hide-leading-stars t)
(setq org-hide-emphasis-markers t)
(setq org-log-done 'time)
(setq org-export-coding-system 'utf-8)
(setq org-return-follows-link  t)
(global-set-key (kbd "C-c o") 'org-open-at-point)
(global-set-key (kbd "C-c m") 'set-mark-command)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-list-indent-offset 2)
(setq org-ellipsis " ▼")
(setq org-capture-templates
      '(("t"              ; hotkey
	 "TODO List item" ; name
	 entry            ; type
	 ; heading type and title
	 (file+headline org-default-notes-file "Tasks")
	 "* TODO %?\n %i\n %a")
	("j"
	 "Journal Entry"
	 entry
	 (file+datetree "~/Org/journal.org")
	 "* %?\nEntered on %u\n %i\n %a")
;; ("j" "Journal" entry (file+datetree "~/org/journal.org")
;;               "* %?\nEntered on %U\n  %i\n  %a")
	)
      )
(setq org-agenda-custom-commands
      '(("c" "Custom Agenda"
         ((agenda "" nil)
          (alltodo ""
                   ((org-agenda-skip-function
                     '(or
                       (org-agenda-skip-entry-if 'scheduled 'deadline)
                       (org-agenda-skip-entry-if 'todo '("SCHEDULED" "DEADLINE")))))
                   (org-agenda-overriding-header "TODO items without scheduled or deadline"))))))
(put 'upcase-region 'disabled nil)

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   ))
