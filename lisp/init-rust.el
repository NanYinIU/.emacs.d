;;; init-rust.el --- Rust language support -*- lexical-binding: t -*-

;;; Commentary:
;; Rust language configuration and tools

;;; Code:

(require 'init-const)
(require 'init-custom)

;; For rust-mode
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook ((rust-mode . eglot-ensure)
         (rust-mode . my/rust-compile))
  :config
  (define-key rust-mode-map (kbd "C-c C-c") nil)
  (setq rust-format-on-save nil)
  (defun my/rust-compile ()
    "Set compile command for Rust."
    (setq-local compile-command "cargo check --color never --tests")))

;; For cargo - Rust package manager integration
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode)
  :config
  (setq shell-command-switch "-ic")
  (defun my/cargo-test-current ()
    "Run cargo test for the current test with debug logging."
    (interactive)
    (setenv "RUST_LOG" "debug")
    (cargo-process-current-test))
  (define-key cargo-mode-map (kbd "C-c C-c") nil)
  :bind (:map rust-mode-map
              (("C-c C-t" . my/cargo-test-current)))
  :custom ((cargo-process--command-current-test "test --color never")
           (cargo-process--enable-rust-backtrace t)))

;; For rust-playground - Quick Rust code experimentation
(use-package rust-playground
  :ensure t
  :after rust-mode
  :hook ((rust-mode . rust-playground-mode)
         (conf-toml-mode . rust-playground-mode))
  :custom (rust-playground-run-command "cargo run --color never")
  :config
  (setq rust-playground-basedir (expand-file-name rust-playground-dir)))

;; Configure TOML mode for Rust
(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

(provide 'init-rust)
;;; init-rust.el ends here
