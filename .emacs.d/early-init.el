(setq use-package-enable-imenu-support t)
(setq package-enable-at-startup t)
(setq use-package-compute-statistics t)
(setenv "LSP_USE_PLISTS" "true")
(dolist (dir '("/home/azmat/.local/bin"
               "/home/azmat/.cargo/bin"))
  (add-to-list 'exec-path dir))
(setenv "PATH" (concat (getenv "PATH") ":/home/azmat/.local/bin"))
(setenv "PATH" (concat (getenv "PATH")  ":/home/azmat/.cargo/bin"))
