(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(global-visual-line-mode 1)
(set-fringe-mode 5)

(menu-bar-mode -1)            ; Disable the menu bar
(electric-pair-mode)	      ; completes delimiters like ({["'"]})
(electric-indent-mode 1)

(dolist (mode '(term-mode-hook
		vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq warning-minimum-level :emergency)
;JetBrainsMonoNerdFont
(set-face-attribute 'default nil :font "LiterationMonoNerdFont" :height 120)
(setq default-frame-alist '((font  . "LiterationMonoNerdFont")))

(setq-default display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; package to automatically zooom the focused window

(use-package zoom)
(custom-set-variables
 '(zoom-size '(0.618 . 0.618)))
(global-set-key (kbd "C-x +") 'zoom)
(global-set-key (kbd "<f8>") 'zoom-mode)

(use-package spacious-padding
  :ensure t)

(setq spacious-padding-widths
      '( :internal-border-width 15
         :header-line-width 4
         :mode-line-width 3
         :tab-width 4
         :right-divider-width 20
         :scroll-bar-width 8))
(spacious-padding-mode -1)
;; Initialize package source
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
 (package-refresh-contents t))

;; initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package doom-themes)
(use-package nerd-icons)

(use-package doom-modeline
  :init
  (setq doom-modeline-project-detection 'file-name)
  (doom-modeline-mode 1))

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

(use-package which-key
  :init
  (which-key-mode))

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

(use-package wgrep
  :ensure t
  :bind (:map grep-mode-map
	      ("e" . wgrep-change-to-wgrep-mode)
	      ("C-c C-c" . wgrep-finish-edit)))

(use-package vterm)
(use-package magit)
(use-package forge
  :after magit)
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

(consult-theme 'modus-vivendi)
(global-set-key (kbd "M-s b") #'consult-buffer)

;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(general-auto-unbind-keys)
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
  "g b" '(magit-blame :wk "magit-blame")
  )

(use-package projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p s f") 'flymake-show-project-diagnostics)
(setq projectile-indexing-method 'hybrid)
(setq projectile-project-search-path '("/home/azmat/Programming/Projects"))

; for using C-g to quit normal mode

(define-key evil-insert-state-map  (kbd "C-g") #'evil-force-normal-state)
(define-key evil-replace-state-map (kbd "C-g") #'evil-force-normal-state)
(define-key evil-visual-state-map  (kbd "C-g") #'evil-force-normal-state)
(define-key evil-operator-state-map (kbd "C-g") #'evil-force-normal-state)

(define-key evil-insert-state-map (kbd "<return>") 'newline)
(evil-set-initial-state 'comint-mode 'emacs)
(evil-set-initial-state 'shell-mode 'emacs)
(evil-set-initial-state 'vterm-mode 'emacs)
(evil-set-initial-state 'Info-mode 'emacs)

(use-package embark)
(use-package embark-consult)

(require 'bind-key)
(bind-key "C-," #'embark-act)

(use-package surround
 :bind-keymap ("M-n" . surround-keymap))

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode))

(use-package ace-window
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
(global-set-key (kbd "M-o") #'ace-window)
(global-set-key (kbd "C-c h") #'dap-hydra)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-lens-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-code-actions-segments '(count icon))
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-render-all t)

  (setq lsp-signature-render-documentation nil)
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ((c-ts-mode
	   c++-ts-mode
	   python-ts-mode
	   java-ts-mode
	   js-ts-mode) . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred
  :config
  )
(global-set-key (kbd "M-.") #'xref-find-definitions)
(use-package lsp-java
  :after lsp)

(use-package java-ts-mode
  :ensure nil
  :hook (java-ts-mode . lsp-deferred))

;; for emacs-lsp-booster
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(setq c-default-style "k&r")
(setq-default c-basic-offset 2)

(use-package lsp-pyright
  :hook (python-ts-mode . (lambda() (require 'lsp-pyright)
			 (lsp))))

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind ((:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common)))
  :init
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-minimum 4)
  (setq company-tooltip-flip-when-above t)
  :custom
  (company-icon-size 18)
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.0)
  (company-text-face-extra-attributes
	'(:weight bold :slant italic))
  (company-transformers '(delete-consecutive-dups
                          company-sort-by-occurrence))

  (company-tooltip-offset-display 'lines))
(global-company-mode)

(use-package lsp-treemacs)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)
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

(use-package dap-mode
  :init
  (setq dap-auto-configure-features '(sessions locals tooltip))
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup))

(nvmap :states '(normal insert visual emacs) :keymaps 'override :prefix "SPC" :global-prefix "M-SPC"
  "E  " '(embark-act :wk "embark-act")
  "c  " '(:ignore t :wk "code")
;  "c l" '(lsp-keymap-prefix :wk "lsp")
  "s l" '(consult-line :wk "consult line")

  "l "  '(:ignore t :wk "lsp")
  "l e" '(lsp-treemacs-errors-list :wk "errors")
  "l s" '(consult-lsp-file-symbols :wk "lsp-file-symbols")
  "c c" '(compile :wk "compile")
  "o  " '(:ignore t :wk "org")
  "o c" '(org-capture :wk "org-capture")
  "o f" '(org-open-at-point :wk "org-open-at-point")
  "o a" '(org-agenda :wk "org-agenda")
  "o t" '(org-timer-set-timer :wk "org-timer")
  "d  " '(:ignore t :wk "dap-mode")
  "d d" '(dap-debug :wk "dap-debug")
  "d l" '(dap-debug-last :wk "dap-debug-last")
  "d s" '(dap-delete-session :wk "dap-delete-session")
  "d a" '(dap-breakpoint-add :wk "dap-add-breakpoint")
  "d A" '(dap-breakpoint-delete :wk "dap-breakpoint-add")
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



(require 'treesit)
(use-package treesit
  :ensure nil
  :custom
  (treesit-language-source-alist '((cpp "https://github.com/tree-sitter/tree-sitter-cpp")
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

(global-hl-line-mode 0)
(setq gc-cons-threshold 100000000) ; allocates more 20MB for emacs than default 0.76MB so that GC doesn't run as often
(setq sentence-end-double-space nil)
(setq read-process-output-max (* 1024 1024)) ;; 1mb Increase the amount of data which Emacs reads from the process
(setq lsp-use-plists t)
(setq-default dired-listing-switches "-lh")
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(show-paren-mode t)
(setq show-paren-delay 0.0)
(setq visible-bell nil)
(setq ring-bell-function t)
(setq create-lockfiles nil)
(setq mouse-yank-at-point t)
(advice-add 'ediff-window-display-p :override #'ignore)
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;remove trailing whitespace on save
(setq backup-directory-alist            '((".*" . "/home/azmat/.emacs_backups")));; (setq tab-always-indent 'complete)
;; (add-to-list 'completion-styles 'initials t)
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

(add-hook 'gdb-mode-hook (lambda () (gdb-many-windows)))

(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0)

(setq org-agenda-span 'week)
(setq org-directory "~/Org")
(setq org-hide-leading-stars t)
(setq org-hide-emphasis-markers nil)
(setq org-log-done 'time)
(setq org-export-coding-system 'utf-8)
(setq org-return-follows-link  t)
(global-set-key (kbd "C-c o") 'org-open-at-point)
(global-set-key (kbd "C-c m") 'set-mark-command)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-list-indent-offset 2)
(setq org-ellipsis " ▼")
(setq org-startup-indented t)
(setq org-clock-sound t)
(add-hook 'after-init-hook 'org-agenda-list)
(setq eldoc-echo-area-use-multiline-p nil)

(setq org-todo-keywords
      '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "SCRAPED")))

(setq org-capture-templates
      '(("t"              ; hotkey
	 "TODO List item" ; name
	 entry            ; type
	 ; heading type and title
	 (file+headline org-default-notes-file "Tasks")
	 "* TODO %?\n %i\n%a \n")
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
   (python . t)))

(evil-set-initial-state 'compilation-mode 'emacs)
(use-package compile
  :ensure nil
  :defer t
  :hook ((compilation-filter . ansi-color-compilation-filter))
  :bind (("C-x C-m" . recompile))
  :config
  (setopt compilation-scroll-output t)
  (setopt compilation-ask-about-save nil)
  (require 'ansi-color))

(use-package command-log-mode)
(setq auth-sources '("~/.authinfo.gpg"))

;; So that swithhing to buffer also causes display action rules
(setq switch-to-buffer-obey-display-actions t)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; make tooltip use echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

(eval-when-compile
  (require 'cl))

(defun get-buffer-matching-mode (mode)
  "Return a list of buffers where their major mode is equal to MODE"
  (let ((buffer-mode-matches '()))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq mode major-mode)
	(push buf buffer-mode-matches))))
  buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major-mode"
  ;;; Becomes a little redundant when I use consult-rg and embark-export
  ;;; the buffer
  (interactive)
  (multi-occur
   (get-buffer-matching-mode major-mode)
   (car (occur-read-primary-args))))
(global-set-key (kbd "M-s M-o") #'multi-occur-in-this-mode)

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
      (window . root)
      (window-height . 0.45))

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
      (side . right)
      (window-width . 60))

     ("\\*Help\\*"
      display-buffer-in-side-window
      (window-width . 60)
      (side . right)))))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(defun open-project-files (project-directory)
  "Open all files in the specified PROJECT-DIRECTORY in separate buffers."
  (interactive "DProject directory: ")
  (if (file-directory-p project-directory)
      (let ((files (directory-files-recursively
		    project-directory
		    ".*\\.\\(el\\|py\\|java\\|cpp\\|h\\|txt\\)$")))
        (dolist (file files)
          (find-file file)))
    (message "Invalid project directory")))
(global-set-key (kbd "C-c i") #'open-project-files)

(use-package clang-format
  :init
  (setq clang-format-style "k&r"))

(use-package java-ts-mode
  :ensure nil
  :custom
  (java-ts-mode-indent-offset 2))

(unless (package-installed-p 'keycast)
  (package-vc-install
   "https://github.com/tarsius/keycast.git"))
(keycast-header-line-mode)

(use-package flycheck
  :init
  (global-flycheck-mode))

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
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  :config
    (setq popper-display-control nil))   ; For echo area hints
