;;; -*- lexical-binding: t; -*-

(defun az-org-timer ()
  (interactive)
  (require 'org-timer)
  (call-interactively 'org-timer-set-timer))
