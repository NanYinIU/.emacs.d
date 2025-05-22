;;; init-rust.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package rust-mode
  :hook (rust-mode . my/rust-compile)
  :config
  (define-key rust-mode-map (kbd "C-c C-c") nil)
  (setq rust-format-on-save nil)
;;  (global-leader
;;    :major-modes
;;    '(rust-mode t)
;;    ;;and the keymaps:
;;    :keymaps
;;    '(rust-mode-map)
;;    "=" 'rust-format-buffer
;;    "c" 'rust-compile
;;    "r" 'rust-run
;;    "t" 'rust-test)
 ;; (define-key rust-mode-map (kbd "RET") 'av/auto-indent-method-maybe)
  (defun my/rust-compile ()
    (setq-local compile-command "cargo check --color never --tests")))


(use-package cargo
  :hook ((rust-mode . cargo-minor-mode))
  :config
  (setq shell-command-switch "-ic")
  (defun my/cargo-test-current ()
    (interactive)
    (setenv "RUST_LOG" "debug")
    (cargo-process-current-test))
  (define-key cargo-mode-map (kbd "C-c C-c") nil)
  :bind (:map rust-mode-map
              (("C-c C-t" . my/cargo-test-current)))
  :custom ((cargo-process--command-current-test "test --color never")
           (cargo-process--enable-rust-backtrace t)))

(use-package rust-playground
  :hook ((rust-mode . rust-playground-mode))
  :custom (rust-playground-run-command "cargo run --color never")
  :commands (rust-playground-get-snippet-basedir)
  :config
  (add-hook 'conf-toml-mode 'rust-playground-mode)
  (setq rust-playground-basedir (expand-file-name "~/Develop/rust/playground")))

(use-package conf-toml-mode
  :ensure nil
  :hook ((conf-toml-mode . rust-playground-mode)))


(provide 'init-rust)
