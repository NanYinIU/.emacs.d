;;; setup-org.el --- Org mode and its ecosystem -*- lexical-binding: t; -*-

;;============ Org Mode Group Start ===============
;; For org (Core Org mode configuration)
(use-package org
  :ensure t
  :init
  ;; Define centaur-org-directory if it's not already defined (e.g. from init-custom.el)
  ;; This is a placeholder; actual value should be set in init-custom.el or similar
  (unless (boundp 'centaur-org-directory)
    (defvar centaur-org-directory "~/org/"
      "Default directory for Org files if not set in init-custom.el."))
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)
  (defconst load-language-alist
    '((emacs-lisp . t)
      ;; (perl       . t)
      (python     . t)
      ;; (rust       . t)
      (js         . t)
      (css        . t)
      ;; 需要使用 conf (properties . t)
      ;;  需要使用 js-json (json       . t)
      (java       . t)
      (shell      . t)
      (plantuml   . t))
    "Alist of org ob languages.")
  (setq inbox-dir (expand-file-name "INBOX.org" centaur-org-directory))
  (setq todo-dir (expand-file-name "TODOs.org" centaur-org-directory))
  (setq book-dir (expand-file-name "TODOs.org" centaur-org-directory)) ; Assuming book-dir is same as todo-dir based on init-org.el
  (setq
   org-startup-with-inline-images t
   org-hierarchical-todo- t
   ;; org mode 默认展示层级
   ;;org-startup-folded 'show2levels
   ;; from https://emacs-china.org/t/org-startup-show2levels/16499
   ;; org-startup-with-inline-images t ; Duplicate, already set above
   org-todo-keywords
   '((sequence "TODO(t)" "PROJ(p)" "NEXT(n)" " WAIT(w)" "FIXME(f)" "|" "DONE(d!)" "CANCELLED(c!)")
     (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
     )
   org-statisticstodo-keyword-faces '(
                                      ("NEXT" . (doom-color 'green)) ; Assuming doom-color is defined
                                      ("FIXME" . (doom-color 'green))
                                      ("TODO" . (doom-color 'yellow))
                                      ("PROJ" . (doom-color 'yellow))
                                      ("WAIT" . (doom-color 'teal))
                                      ("MAYBE" . (doom-color 'base5))
                                      ("[ ]" . (doom-color 'green))
                                      ("[-]" . (doom-color 'yellow))
                                      ("[?]" . (doom-color 'red)))
   )
  :config
  (setq org-capture-templates nil)
  (add-to-list 'org-capture-templates
               '("i" "INBOX" entry
                 (file+headline inbox-dir "REFILE")
                 "\n* TODO %^{内容}\n%u\n"))

  (add-to-list 'org-capture-templates '("t" "Tasks"))
  (add-to-list 'org-capture-templates
               '("tw" "Work Task" entry
                 (file+headline todo-dir "Work")
                 "\n* TODO %^{任务名} :work: \n%u"))
  (add-to-list 'org-capture-templates
               '("tp" "Personal Task" entry
                 (file+headline todo-dir "Personal")
                 "\n* TODO %^{任务名} :personal: \n%u\n"))
  (setq org-agenda-files (list
                          (expand-file-name "TODOs.org" centaur-org-directory)
                          (expand-file-name "INBOX.org" centaur-org-directory)
                          (expand-file-name "ARCHIVED.org" centaur-org-directory)
                          (expand-file-name "daily" centaur-org-directory)
                          ))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 4) ; 扫描 agenda 文件中的标题，最深4级
                           (nil :maxlevel . 3)))          ; 也允许归档到当前文件的标题
  ;;  (setq org-agenda-files (list centaur-org-directory))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-include-deadlines t)
  (setq org-agenda-include-diary t)
  (setq org-agenda-block-separator nil)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-log-mode-items '(closed clock state))
  (setq org-agenda-use-time-grid t)
  ;; (add-hook 'org-agenda-mode-hook '(lambda() (hl-line-mode 1)))

  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                (done ("WAITING"))
                ("TODO" ("WAITING") ("CANCELLED"))
                ("NEXT" ("WAITING") ("CANCELLED"))
                ("DONE" ("WAITING") ("CANCELLED"))
                )))

  (setq
   org-agenda-prefix-format '((agenda    . "  %i %-12:c%?-12t% s ")
                              (timeline  . "  %-6t %6e ")
                              (todo      . "  %-6t %6e ")
                              (tags      . "  %i %-12:c ")
                              (search    . "%l")
                              )
   org-agenda-tags-column 80
   ;; org-agenda-start-on-weekday 1 ; 从今天开始
   org-agenda-include-deadlines t
   org-agenda-block-separator nil
   org-agenda-compact-blocks t
   org-agenda-breadcrumbs-separator " ❱ "
   org-agenda-current-time-string " ┈┈┈┈┈┈┈┈┈┈┈ now"
   org-agenda-sorting-strategy
   '((agenda habit-down time-up effort-up priority-down category-keep)
     (todo   priority-up effort-up todo-state-up category-keep)
     (tags   priority-down category-keep)
     (search category-keep))
   )
  ;; Prettify time grid
  (setq org-agenda-time-grid (quote ((daily today require-timed)
                                     (300
                                      600
                                      900
                                      1200
                                      1500
                                      1800
                                      2100
                                      2400)
                                     "......"
                                     "-----------------------------------------------------"
                                     )))
  (org-babel-do-load-languages 'org-babel-load-languages load-language-alist)
   (define-key org-mode-map (kbd "RET")
                'my-org/org-return)
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; HACK: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil)))
         (org-mode-hook . valign-mode)
         (org-mode-hook . org-ident-mode)
         )
  :bind (
         ("C-c [" . org-mark-ring-goto)
         ("C-c o a" . org-agenda))
  )

;; For org-appear
(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks t)
  )

;; For org-download
(use-package org-download
  :ensure t
  :commands (org-download-yank org-download-clipboard org-download-screenshot org-download-file org-download-enable)
  :custom
  (org-download-display-inline-images 'posframe)
  (org-download-method 'attach)
  ;; not work，use custom.el config
  ;; (org-download-image-dir (expand-file-name "img/" (file-truename centaur-org-directory))) ; centaur-org-directory needs to be defined
  (org-download-heading-lvl nil)
  (org-download-image-attr-list
   '("#+ATTR_HTML: :width 65% :align center"))
  :config
  (when (boundp 'centaur-org-directory) ; Set image dir only if centaur-org-directory is defined
    (setq org-download-image-dir (expand-file-name "img/" (file-truename centaur-org-directory))))
  (add-hook 'dired-mode-hook #'org-download-enable)
  )

;; For org-fancy-priorities
(use-package org-fancy-priorities
  :ensure t
  :diminish
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list
        '("🅰" "🅱" "🅲" "🅳" "🅴")))

;; For org-modern
(use-package org-modern
  :ensure t ;; 如果你还没有安装 org-modern，请确保 t
  :hook ((org-mode . org-modern-mode)                ;; 在打开 org 文件时启用 org-modern-mode
         (org-mode . global-org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)   ;; 美化 agenda 视图
         (org-modern-mode . (lambda ()
                              "为 org-modern-mode 进行额外调整"
                              ;; 禁用 Emacs 内建的 prettify-symbols-mode，避免与 org-modern 冲突
                              ;; 这是你配置中已有的好做法
                              (setq prettify-symbols-alist nil)
                              (prettify-symbols-mode -1)

                              ;; ----------------------------------------------------
                              ;; 使用不同的 Unicode 字符作为标题星号
                              (setq org-modern-star '("◉" "○" "✸" "✿"))
                              ;; 或者使用更简约的实心点 (需要兼容的字体)
                              ;;(setq org-modern-star '("●" "○" "◆" "◇"))
                              ;; 如果想完全隐藏星号，只通过缩进区分层级
                              ;; (setq org-modern-hide-stars t)


                              ;; --- 标签样式 ---
                              ;; 使用更柔和的标签背景色，或者只改变前景色
                              ;; (setq org-modern-tag-faces '((:foreground "dim gray" :weight 'semi-bold)))
                              ;; 或者给标签加上边框/药丸形状 (需要主题或额外 face 定义支持良好)
                              ;; (setq org-modern-tag-faces '((:box (:line-width (-1 . -1) :color "gray" :style nil) :foreground "dim gray")))

                              ;; --- 列表项目符号 ---
                              ;; 自定义无序列表的项目符号
                              (setq org-modern-list '((43 . "· ")  ; + (plus)
                                                      (45 . "– ")  ; - (hyphen)
                                                      (42 . "• "))) ; * (asterisk)

                              (setq org-modern-horizontal-rule "┈┈┈┈┈┈") ;; 虚线

                              ;; --- 表格 ---
                              ;; 启用更现代的表格渲染 (使用 box-drawing 字符)
                              (setq org-modern-table 'modern)

                              ;; --- 元数据行 (如 #+TITLE, #+AUTHOR) ---
                              (setq org-modern-keyword-foreground "DarkGoldenrod") ;; 改变元数据关键字的颜色
                              (setq org-modern-meta-line-padding 1) ;; 增加元数据行上方的填充

                              ;; --- 日程和时钟 ---
                              (setq org-modern-agenda-time-grid-custom-colors t) ;; 允许agenda时间网格使用自定义颜色

                              )))
  :config
  ;; 默认情况下，org-modern 已经有很多不错的设置了。
  ;; :config 块可以留空，或者放一些不适合在 hook lambda 中设置的变量（比如只设置一次的）
  ;; 例如，如果你想全局修改一些 org-modern 自身提供的 face (虽然通常由主题处理)
  ;; (custom-set-faces
  ;;  '(org-modern-tag ((t (:foreground "blue" :weight 'bold)))))
  (setq org-modern-todo t) ; 启用 TODO 关键字的现代化显示
  (setq org-modern-priority t) ; 启用优先级的现代化显示 (例如 [#A] -> Ⓐ)
  )

;; For org-roam
(use-package org-roam
  :ensure t
  :after org
  :commands
  (org-roam-mode
   org-roam-capture
   org-roam-node-find)
  :init
  ;;  (setq org-id-extra-files (org-roam--list-files org-roam-directory))
  ;; Ensure centaur-org-directory is defined before use
  (unless (boundp 'centaur-org-directory)
    (defvar centaur-org-directory "~/org/"
      "Default directory for Org files if not set in init-custom.el."))
  (setq
   org-roam-directory (file-truename centaur-org-directory)
   org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
   )
  :config
  ;;(org-roam-db-autosync-enable)
  (org-roam-setup)
  ;;(setq org-ioam-mode-sections
  ;;      (list #'org-roam-backlinks-insert-section
  ;;            #'org-roam-reflinks-insert-section
  ;;            #'org-roam-unlinked-references-insert-section))
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain "%?"
           :target (file+head "${slug}-%<%Y%m%d%H>.org"
                              "#+title:${title}\n#+filetags:\n\n")
           :unnarrowed t)

          ("p" "Persional Note" plain "%?"
           :target (file+head "note/${slug}-%<%Y%m%d%H%M%S>.org"
                              "#+title:${title}\n#+filetags: :note:\n\n")
           :unnarrowed t)

          ("w" "Work Document" plain "%?"
           :target (file+head "work/${slug}-%<%Y%m%d%H%M%S>.org"
                              "#+title:${title}\n#+filetags: :work:\n\n")
           :unnarrowed t)

          ("l" "LeetCode")
          ("la" "Arrays" entry "\n** ${title} \n:PROPERTIES:\n:ID: %(org-id-new)\n:END:\n*** Reference\n%^{reference}\n*** Questtion\n \n*** Solution \n "
           :target (file+olp "Leetcode.org" ("Arrays"))
           :unnarrowed t)
          ("ll" "List" entry "\n** ${title} \n:PROPERTIES:\n:ID: %(org-id-new)\n:END:\n*** Reference\n%^{reference}\n*** Questtion\n \n*** Solution \n "
           :target (file+olp "Leetcode.org" ("List"))
           :unnarrowed t)
          )
        )
  (setq org-roam-capture-ref-templates
        '(
          ("a" "Annotation" plain
           "%U ${body}\n"
           :target (file+head "reference/${slug}.org" "#+title: ${title}\n#+roam_ref: ${ref}\n#+filetags:\n\n")
           ;; :immediate-finish t
           :unnarrowed t
           )
          ("r" "ref"
           entry
           "* TODO %<%I:%M %p>: %:annotation :Bookmark: \n SCHEDULED: %T \n  "
           :target (file+olp (format "%s/%s" "INBOX.org") ("Bookmarks")) ; INBOX.org path needs to be correct
           :immediate-finish t)
          ))
  (setq org-roam-completion-everywhere t)
   (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)
                ))

  (defun my-doom-modeline-roam-aware-buffer-file-name (orig-fun &rest args)
  "Display Org Roam filenames: 'YYYY-MM-DD' for dailies, and ' Title (YYYY-MM-DD)' or ' (YYYY-MM-DD) Title' for others."
  ;; 基本条件检查：是否为 Org Roam 目录下的 .org 文件
  (if (and (boundp 'org-roam-directory)
           org-roam-directory
           (stringp buffer-file-name)
           (require 's nil 'noerror) ; 确保 s.el 字符串处理库可用
           ;; 使用 file-truename 处理符号链接等情况，确保路径比较的准确性
           (s-starts-with-p (file-truename org-roam-directory) (file-truename buffer-file-name))
           (s-ends-with-p ".org" buffer-file-name t)) ; t 表示忽略后缀的大小写

      ;; 提取文件名（不含路径和后缀），并将下划线替换为空格
      (let* ((filename (file-name-nondirectory buffer-file-name))
             (name-part (file-name-sans-extension filename))
             (name-part-spaced (subst-char-in-string ?_ ?\s name-part))
             (icon " ") ; 定义图标，方便复用
             (display-string nil)) ; 用于存储最终显示的字符串

        (cond
         ;; ---------------------------------------------------------------------
         ;; 模式1: "日志" 文件 (纯日期文件名，如 "2023-01-01" 或 "20230101")
         ;; 期望输出: "2023-01-01" (不带图标)
         ;; ---------------------------------------------------------------------
         ((string-match "^\\([0-9]\\{4\\}\\)[-_]?\\([0-9]\\{2\\}\\)[-_]?\\([0-9]\\{2\\}\\)$" name-part-spaced)
          (let ((year (match-string 1 name-part-spaced))
                (month (match-string 2 name-part-spaced))
                (day (match-string 3 name-part-spaced)))
            ;; 可选: 如果希望日志文件必须在特定的 "daily/" 子目录下，可以在这里添加检查
            ;; (if (s-contains-p "/daily/" buffer-file-name t) ... )
            (setq display-string (format "%s%s-%s-%s" icon year month day))))

         ;; ---------------------------------------------------------------------
         ;; 模式2: "其他文件" - 标题在前，日期/时间戳在后 (如 "我的笔记-20230101120000")
         ;; 期望输出: " 我的笔记 (2023-01-01)"
         ;; ---------------------------------------------------------------------
         ((string-match "^\\(.*\\)-\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*$" name-part-spaced)
          (let ((title-part (s-trim (match-string 1 name-part-spaced))) ; 捕获标题并去除首尾空格
                (year  (match-string 2 name-part-spaced))
                (month (match-string 3 name-part-spaced))
                (day   (match-string 4 name-part-spaced)))
            (if (not (s-blank? title-part)) ; 确保标题部分不为空
                (setq display-string (format "%s(%s%s-%s-%s)" title-part icon year month day)))))

         ;; ---------------------------------------------------------------------
         ;; 模式3: "其他文件" - 日期/时间戳在前，标题在后 (如 "20230101120000-我的笔记")
         ;; 期望输出: " (2023-01-01) 我的笔记"
         ;; ---------------------------------------------------------------------
         ((string-match "^\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-\\(.*\\)$" name-part-spaced)
          (let ((year (match-string 1 name-part-spaced))
                (month (match-string 2 name-part-spaced))
                (day (match-string 3 name-part-spaced))
                (title-part (s-trim (match-string 4 name-part-spaced))))
            (if (not (s-blank? title-part)) ; 如果有标题部分
                (setq display-string (format "%s(%s-%s-%s) %s" icon year month day title-part))
              ;; 如果标题部分为空 (例如文件名是 "202301011200-")，也显示带图标的日期
              (setq display-string (format "%s(%s-%s-%s)" icon year month day))))))
        ;; --- End of cond ---

        ;; 如果以上任何自定义格式规则匹配成功，则使用 display-string
        (if display-string
            display-string
          ;; 如果所有自定义规则都未匹配，则调用原始函数处理。
          ;; 这确保了如果文件名不符合上述任何一种 Roam 特定格式，
          ;; 或者您希望原始函数有其他处理逻辑，它仍然会被执行。
          (apply orig-fun args)))

    ;; 如果不是 Org Roam 目录下的 .org 文件，则直接调用原始函数
    (apply orig-fun args)))
  (with-eval-after-load 'doom-modeline
    (advice-add 'doom-modeline-buffer-file-name
                :around #'my-doom-modeline-roam-aware-buffer-file-name))
  :bind (("C-c o l" . org-roam-buffer-toggle)
         ("C-c o f" . org-roam-node-find)
         ("C-c o g" . org-roam-graph)
         ("C-c o i" . org-roam-node-insert)
         ("C-c o c" . org-roam-capture)
         ("C-c o j" . org-roam-dailies-capture-today)
         ("C-c o t" . org-roam-dailies-goto-today)
         )
  )

;; For org-roam-ui
(use-package org-roam-ui
  :ensure t
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;; :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme nil
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; For toc-org
(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-mode))

;;============ Org Mode Group End ===============

(provide 'setup-org)
