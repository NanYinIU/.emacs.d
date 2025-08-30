;;; setup-terminal.el --- Terminal and shell configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure terminal emulators and shell integration

;;; Code:

(require 'init-const)
(require 'init-funcs)

;; For vterm - Superior terminal emulator
(use-package vterm
  :ensure t
  :defer t
  :diminish
  :bind (("C-c t" . vterm))
  :config
  ;; Terminal configuration
  (setq vterm-shell "zsh"
        vterm-buffer-name-string "vterm:%s"
        vterm-kill-buffer-on-exit t)

  ;; vterm mode keybindings
  (define-key vterm-mode-map (kbd "C-c C-k") 'vterm-send-C-c)
  (define-key vterm-mode-map (kbd "<f1>") 'vterm-other-window)
  (define-key vterm-mode-map (kbd "C-c c") 'vterm-copy-mode)
  (define-key vterm-mode-map (kbd "C-c y") 'vterm-yank)

  ;; Integration with projectile for project-specific terminals
  (with-eval-after-load 'projectile
    (defun my-projectile-vterm ()
      "Open vterm in the current projectile project root."
      (interactive)
      (let ((project-root (projectile-project-root)))
        (if project-root
            (vterm project-root)
          (vterm))))
    (define-key projectile-mode-map (kbd "C-c p t") #'my-projectile-vterm))

  ;; Configure process coding for ripgrep integration
  (add-to-list 'process-coding-system-alist
               '("[rR][gG]" . (utf-8 . utf-8))))

(provide 'setup-terminal)
;;; setup-terminal.el ends here