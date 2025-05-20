;; init-check.el --- Define functions.	-*- lexical-binding: t -*-

;; Example of how you might structure with use-package
;; (my/ensure-package-installed 'use-package) ;; Make sure use-package itself is installed
;; (require 'use-package)

 (use-package flymake
   :ensure nil ;; Built-in
   :hook (prog-mode . flymake-mode)
   :bind ("C-c f" . flymake-show-buffer-diagnostics)
   :config
   (setq flymake-no-changes-timeout 0.5)
   :init (setq flymake-no-changes-timeout nil
              flymake-fringe-indicator-position 'right-fringe)
   )

(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (java-mode . eglot-ensure) ; <--- 确保此 hook 存在且正确
         )
  :config
  (setq eglot-confirm-server-initiated-edits nil) ; 个人偏好

  ;; 为其他非 Java 语言配置服务器 (如果需要)
  (add-to-list 'eglot-server-programs
               '(rust-mode . ("rust-analyzer")))
  ;; (add-to-list 'eglot-server-programs
  ;;              '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))) ; 您的 Python 配置

  ;; -----------------------------------------------------
  ;; Java (Eclipse JDT LS 1.8.0) 配置 for Eglot
  ;; -----------------------------------------------------
  ;; 将下面整个 let* 块和 add-to-list 一起复制
  (let* (
         ;; !<--- 请务必修改为您 JDT LS 1.8.0 解压的【根目录】路径 ---!>
         (jdtls-install-dir (expand-file-name "~/.emacs.d/servers/jdtls/"))
         (jdtls-launcher-jar (expand-file-name "plugins/org.eclipse.equinox.launcher_1.6.400.v20210924-0641.jar" jdtls-install-dir))
         ;; !<--- 如果不使用 Lombok，可以将此行设置为 (lombok-jar nil) 或者完全移除下面 if 块中 lombok-jar 的相关逻辑 ---!>
         (lombok-jar (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.32/lombok-1.18.32.jar"))
        )

    ;; 检查 launcher JAR 是否存在
    (if (not (file-exists-p jdtls-launcher-jar))
        (warn (format "JDT LS launcher JAR not found: %s. Please verify 'jdtls-install-dir' in your Eglot Java configuration." jdtls-launcher-jar))
      ;; 将 Java 语言服务器配置添加到 eglot-server-programs
      (add-to-list 'eglot-server-programs
                   (cons 'java-mode ; 关联到 java-mode
                         ;; 下面是启动服务器的 lambda 函数
                         (lambda (project-root) ; project-root 由 Eglot 传入，可能为 nil
                           (let* ( ;; -- 内部 let* 开始，定义启动命令所需的各种变量 --
                                  (os-config-dir-name (cond ((string-equal system-type "windows-nt") "config_win")
                                                            ((string-equal system-type "darwin") "config_mac")
                                                            (t "config_linux")))
                                  (jdtls-configuration-dir (expand-file-name os-config-dir-name jdtls-install-dir))

                                  ;; vvv --- 这是处理 project-root 为 nil 的关键逻辑 --- vvv
                                  (project-data-suffix
                                   (if project-root
                                       (md5 (expand-file-name project-root)) ; 为有效项目根创建 MD5 后缀
                                     "default_non_project_workspace")) ; project-root 为 nil 时的默认后缀
                                  (project-specific-data-dir
                                   (concat (file-name-as-directory (expand-file-name "~/.emacs.d/eglot_jdtls_data/"))
                                           project-data-suffix))
                                  ;; ^^^ --- project-root 处理结束 --- ^^^

                                  (command-args (list
                                                 "-Declipse.application=org.eclipse.jdt.ls.core.id1"
                                                 "-Dosgi.bundles.defaultStartLevel=4"
                                                 "-Declipse.product=org.eclipse.jdt.ls.core.product"
                                                 "-Dlog.protocol=true"
                                                 "-Dlog.level=INFO" ; 日志级别，可调整为 WARN, ERROR, 或 ALL
                                                 "-Xms2g"           ; JVM 初始内存
                                                 "-Xmx2G"           ; JVM 最大内存 (根据您的系统和项目调整)
                                                 "--add-modules=ALL-SYSTEM"
                                                 "--add-opens" "java.base/java.util=ALL-UNNAMED"
                                                 "--add-opens" "java.base/java.lang=ALL-UNNAMED"
                                                 "-jar" jdtls-launcher-jar ; 使用外部 let* 捕获的 launcher jar 路径
                                                 "-configuration" jdtls-configuration-dir
                                                 "-data" project-specific-data-dir
                                                 ))
                                  ) ;; -- 内部 let* 结束 --

                             ;; 如果使用 Lombok，并且 lombok-jar 路径有效，则添加 javaagent 参数
                             (if (and lombok-jar (file-exists-p lombok-jar) (string-match-p "\\.jar$" lombok-jar))
                                 (setq command-args (append (list (format "-javaagent:%s" lombok-jar)
                                                                  (format "-Xbootclasspath/a:%s" lombok-jar))
                                                            command-args)))
                             ;; 返回最终的服务器启动命令列表 ("java" "arg1" "arg2" ...)
                             (cons "java" command-args)))))))
  ;; -- Java 配置结束 --
)

(use-package rust-mode ;; If you manage rust-mode with use-package
  :ensure t
  :mode "\\.rs\\'")

(provide 'init-check)
