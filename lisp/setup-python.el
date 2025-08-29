;;; setup-python.el --- Enhanced Python configuration for pyenv -*- lexical-binding: t -*-

;;; Commentary:
;; Python language configuration with pyenv support

;;; Code:

(require 'init-const)
(require 'init-custom)

;; For pyenv-mode
(use-package pyenv-mode
  :ensure t
  :defer t
  :hook (python-ts-mode . pyenv-mode)
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

;; For python
(use-package python
  :ensure t
  :defer t
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python3" . python-ts-mode)
  :config
  (setq python-shell-interpreter python-virtualenv-path)
  :bind (:map python-ts-mode-map
              ("C-c C-r" . python-shell-send-region)
              ("C-c C-c" . python-shell-send-buffer)
              ("C-c C-p" . run-python)))

(provide 'setup-python)
;;; setup-python.el ends here
