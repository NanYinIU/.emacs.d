;;; setup-python.el --- Enhanced Python configuration for pyenv -*- lexical-binding: t; -*-

(use-package pyenv-mode
  :ensure t
  :hook (python-ts-mode . pyenv-mode)
  :config
  ;; Set the desired pyenv virtual environment
  (setq pyenv-mode-virtualenv "nvim_py3_venv")
  
  ;; Ensure pyenv-mode sets the interpreter for other packages to use
  (setq python-shell-interpreter (concat (pyenv-root) "/versions/" pyenv-mode-virtualenv "/bin/python")))

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python3" . python-ts-mode)
  :hook (python-ts-mode . eglot-ensure)
  :config
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