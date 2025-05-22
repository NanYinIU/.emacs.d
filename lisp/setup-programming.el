;;; setup-programming.el --- Programming language support and tools -*- lexical-binding: t; -*-

;; For apheleia
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1) ; 全局启用，它会根据项目和文件类型自动选择格式化器
  )

;; For eglot
(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         ;; Add other modes from init-complete.el if desired, e.g.:
         ;; (c-mode . eglot-ensure)
         ;; (c++-mode . eglot-ensure)
         ;; (typescript-mode . eglot-ensure)
         ;; (js-mode . eglot-ensure)
         )
  :config
  (setq eglot-confirm-server-initiated-edits nil) ; Preserved from zz-migrated
  ;; (setq eglot-autoreconnect 3) ; From init-complete, optional
  ;; (setq eglot-connect-timeout 30) ; From init-complete, optional

  ;; Server configurations from init-complete.el
  (add-to-list 'eglot-server-programs
               '(rust-ts-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs ; This effectively replaces the one from init-check for rust-mode
               '(rust-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs ; Prefer this python config from init-complete
                '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  ;; (add-to-list 'eglot-server-programs
  ;;              '((python-mode python-ts-mode) . ("pylsp"))) ; Alternative python from init-complete

  ;; Java JDT LS configuration from init-check.el (MUST BE PRESERVED)
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
  ;; -- Java 配置结束 --
)

;; For flymake (built-in)
(use-package flymake
  :ensure nil ;; Built-in
  :hook (prog-mode . flymake-mode)
  :bind ("C-c f" . flymake-show-buffer-diagnostics)
  :config
  (setq flymake-no-changes-timeout 0.5)
  :init 
  (setq flymake-no-changes-timeout nil 
        flymake-fringe-indicator-position 'right-fringe))

;; For idlwave
(use-package idlwave
  :ensure t)

;; For json-mode
(use-package json-mode
  :ensure t)

;; For log4e
(use-package log4e
  :ensure t)

;; For prog-mode general hooks
(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . prettify-symbols-mode)
         (prog-mode . hs-minor-mode))
  :init
  ;;(setq-default prettify-symbols-alist centaur-prettify-symbols-alist) ; This refers to a custom var, keep commented if it's not defined yet or defined in init-custom.el
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
  :ensure t)

;; For quickrun
(use-package quickrun
  :ensure t
  :bind (("C-<f5>" . quickrun)
         ("C-c X"  . quickrun)))

;; For rust-mode
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

;; For treesit-auto
(use-package treesit-auto
  :ensure t ; ensure t, as it's a package to be installed
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (setq treesit-font-lock-level 4)
  (global-treesit-auto-mode)
  (add-to-list 'treesit-extra-load-path
               (expand-file-name "tree-sitter" user-emacs-directory)))

;; For verilog-mode
(use-package verilog-mode
  :ensure t)

(provide 'setup-programming)
