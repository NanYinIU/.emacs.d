;;; setup-python.el --- Enhanced Python configuration for pyenv -*- lexical-binding: t; -*-

(use-package pyenv-mode
  :ensure t
  :hook (python-ts-mode . pyenv-mode)
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions")
)

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python3" . python-ts-mode)
  :config
  (setq python-shell-interpreter "~/.virtualenvs/nvim_py3_venv/bin/python")
  ;; Keybindings for python-ts-mode
  (define-key python-ts-mode-map (kbd "C-c C-r") 'python-shell-send-region)
  (define-key python-ts-mode-map (kbd "C-c C-c") 'python-shell-send-buffer)
  (define-key python-ts-mode-map (kbd "C-c C-p") 'run-python))

;; Configure format-on-save for python files
(use-package apheleia
  :ensure t
  :config
  (setf (alist-get 'python-ts-mode apheleia-formatters)
        '(("ruff" "format" "-")
          ("black" "-q" "-")))
  (setf (alist-get 'python-mode apheleia-formatters)
        '(("ruff" "format" "-")
          ("black" "-q" "-"))))

(provide 'setup-python)
