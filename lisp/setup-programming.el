;;; setup-programming.el --- Programming language support and tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure programming language support and development tools

;;; Code:

(require 'init-const)
(require 'init-funcs)

;; For apheleia - Code formatter
(use-package apheleia
  :ensure t
  :defer t
  :diminish
  :config
  (apheleia-global-mode +1)
  ;; Configure format-on-save for python files
  (setf (alist-get 'python-ts-mode apheleia-formatters)
        '(("ruff" "format" "-")
          ("black" "-q" "-")))
  (setf (alist-get 'python-mode apheleia-formatters)
        '(("ruff" "format" "-")
          ("black" "-q" "-"))))

;; For eglot - LSP client
(use-package eglot
  :ensure t
  :defer t
  :diminish
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
  (setq eglot-confirm-server-initiated-edits nil
        eglot-autoreconnect 3
        eglot-connect-timeout 30)

  ;; Server configurations
  (add-to-list 'eglot-server-programs
               `(python-ts-mode . (,python-virtualenv-path "-m" "pylsp")))
  (add-to-list 'eglot-server-programs
               '(rust-ts-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs
               '(rust-mode . ("rust-analyzer")))
  (setq-default eglot-workspace-configuration
    `((:pylsp .
              (:plugins
               (:jedi_completion (:include_params t
                                  :fuzzy t)
                 :jedi (:environment python-virtualenv-dir)
                 :black (:enabled t)
                 :rope_autoimport (:enabled t
                                   :python_path python-virtualenv-dir))))))

  ;; Java JDT LS configuration
  (let* ((jdtls-install-dir (expand-file-name "~/.emacs.d/servers/jdtls/"))
         (jdtls-launcher-jar (expand-file-name "plugins/org.eclipse.equinox.launcher_1.6.400.v20210924-0641.jar" jdtls-install-dir))
         (lombok-jar (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.32/lombok-1.18.32.jar")))
    (if (not (file-exists-p jdtls-launcher-jar))
        (warn (format "JDT LS launcher JAR not found: %s. Please verify 'jdtls-install-dir' in your Eglot Java configuration." jdtls-launcher-jar))
      (add-to-list 'eglot-server-programs
                   (cons 'java-mode
                         (lambda (project-root)
                           (let* ((os-config-dir-name (cond ((string-equal system-type "windows-nt") "config_win")
                                                             ((string-equal system-type "darwin") "config_mac")
                                                             (t "config_linux")))
                                  (jdtls-configuration-dir (expand-file-name os-config-dir-name jdtls-install-dir))
                                  (project-data-suffix
                                   (if (and project-root (stringp project-root))
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
                                                 "-data" project-specific-data-dir)))
                             (if (and lombok-jar (file-exists-p lombok-jar) (string-match-p "\\.jar$" lombok-jar))
                                 (setq command-args (append (list (format "-javaagent:%s" lombok-jar)
                                                                  (format "-Xbootclasspath/a:%s" lombok-jar))
                                                            command-args)))
                             (cons "java" command-args))))))))

;; For flymake - Syntax checking
(use-package flymake
  :ensure nil
  :defer t
  :diminish
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
  :defer t
  :diminish
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :init
  ;; Use full path to aspell to avoid PATH issues
  (setq ispell-program-name "/opt/homebrew/bin/aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
  :config
  ;; Ensure proper encoding for aspell
  (setq ispell-encoding8-command t)
  (setq ispell-dictionary "en_US"))

;; For eldoc - Documentation display
(use-package eldoc
  :ensure nil
  :defer t
  :diminish
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.2))

;; For prog-mode general hooks
(use-package prog-mode
  :ensure nil
  :defer t
  :diminish
  :hook ((prog-mode . prettify-symbols-mode)
         (prog-mode . hs-minor-mode)
         (prog-mode . display-line-numbers-mode))
  :init
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; For quickrun - Run code snippets
(use-package quickrun
  :ensure t
  :defer t
  :diminish
  :bind (("C-<f5>" . quickrun)
         ("C-c X"  . quickrun)))

;; For treesit-auto - Tree-sitter support
(use-package treesit-auto
  :ensure t
  :demand t
  :diminish
  :config
  (setq treesit-auto-install 'prompt)
  (setq treesit-font-lock-level 4)
  (global-treesit-auto-mode)
  (add-to-list 'treesit-extra-load-path
               (expand-file-name "tree-sitter" user-emacs-directory)))

(provide 'setup-programming)
;;; setup-programming.el ends here