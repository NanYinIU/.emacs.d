;;; setup-programming.el --- Programming language support and tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure programming language support and development tools

;;; Code:

(require 'init-const)
(require 'init-funcs)

;; For apheleia - Formatter
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

;; For eglot - LSP client
(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure))
  :config
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-autoreconnect 3)
  (setq eglot-connect-timeout 30)

  ;; Server configurations
  (add-to-list 'eglot-server-programs
               '(rust-ts-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs
               '(rust-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))

  ;; Java JDT LS configuration
  (let* (
         (jdtls-install-dir (expand-file-name "~/.emacs.d/servers/jdtls/"))
         (jdtls-launcher-jar (expand-file-name "plugins/org.eclipse.equinox.launcher_1.6.400.v20210924-0641.jar" jdtls-install-dir))
         (lombok-jar (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.32/lombok-1.18.32.jar"))
        )
    (if (not (file-exists-p jdtls-launcher-jar))
        (warn (format "JDT LS launcher JAR not found: %s. Please verify 'jdtls-install-dir' in your Eglot Java configuration." jdtls-launcher-jar))
      (add-to-list 'eglot-server-programs
                   (cons 'java-mode
                         (lambda (project-root)
                           (let* (
                                  (os-config-dir-name (cond ((string-equal system-type "windows-nt") "config_win")
                                                            ((string-equal system-type "darwin") "config_mac")
                                                            (t "config_linux")))
                                  (jdtls-configuration-dir (expand-file-name os-config-dir-name jdtls-install-dir))
                                  (project-data-suffix
                                   (if project-root
                                       (md5 (expand-file-name project-root))
                                     "default_non_project_workspace"))
                                  (project-specific-data-dir
                                   (concat (file-name-as-directory (expand-file-name "~/.emacs.d/eglot_jdtls_data/"))
                                           project-data-suffix))
                                  (command-args (list
                                                 "-Declipse.application=org.eclipse.jdt.ls.core.id1"
                                                 "-Dosgi.bundles.defaultStartLevel=4"
                                                 "-Declipse.product=org.eclipse.jdt.ls.core.product"
                                                 "-Dlog.protocol=true"
                                                 "-Dlog.level=INFO"
                                                 "-Xms2g"
                                                 "-Xmx2G"
                                                 "--add-modules=ALL-SYSTEM"
                                                 "--add-opens" "java.base/java.util=ALL-UNNAMED"
                                                 "--add-opens" "java.base/java.lang=ALL-UNNAMED"
                                                 "-jar" jdtls-launcher-jar
                                                 "-configuration" jdtls-configuration-dir
                                                 "-data" project-specific-data-dir
                                                 ))
                                  )
                             (if (and lombok-jar (file-exists-p lombok-jar) (string-match-p "\\.jar$" lombok-jar))
                                 (setq command-args (append (list (format "-javaagent:%s" lombok-jar)
                                                                  (format "-Xbootclasspath/a:%s" lombok-jar))
                                                            command-args)))
                             (cons "java" command-args)))))))
)

;; For flymake - Syntax checking
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (("C-c f" . flymake-show-buffer-diagnostics))
  :init 
  (setq flymake-no-changes-timeout nil 
        flymake-fringe-indicator-position 'right-fringe)
  :config
  (setq flymake-no-changes-timeout 0.5))

;; For flyspell - Spell checking
(use-package flyspell
  :ensure nil
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :init
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")))

;; For eldoc - Documentation display
(use-package eldoc
  :ensure nil
  :diminish
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.2))

;; For json-mode
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

;; For markdown-mode
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"
        markdown-fontify-code-blocks-natively t))

;; For prog-mode general hooks
(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . prettify-symbols-mode)
         (prog-mode . hs-minor-mode)
         (prog-mode . display-line-numbers-mode))
  :init
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; For protobuf-mode
(use-package protobuf-mode
  :ensure t
  :hook (protobuf-mode . (lambda ()
                           "Set up Protobuf's imenu generic expressions."
                           (setq imenu-generic-expression
                                 '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

;; For python
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; For quickrun - Run code snippets
(use-package quickrun
  :ensure t
  :bind (("C-<f5>" . quickrun)
         ("C-c X"  . quickrun)))

;; For rust-mode and related tools
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook (rust-mode . eglot-ensure))

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
  :custom (rust-playground-run-command "cargo run --color never")
  :config
  (setq rust-playground-basedir (expand-file-name "~/Develop/rust/playground"))
  (add-hook 'conf-toml-mode-hook 'rust-playground-mode))

;; Configure TOML mode for Rust
(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

;; For treesit-auto - Tree-sitter support
(use-package treesit-auto
  :ensure t
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (setq treesit-font-lock-level 4)
  (global-treesit-auto-mode)
  (add-to-list 'treesit-extra-load-path
               (expand-file-name "tree-sitter" user-emacs-directory)))

;; For yaml-mode
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

;; For yasnippet - Template system
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

;; For yasnippet-snippets - Collection of snippets
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(provide 'setup-programming)
;;; setup-programming.el ends here