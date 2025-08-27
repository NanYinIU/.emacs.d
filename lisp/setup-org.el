;;; setup-org.el --- Org mode and its ecosystem -*- lexical-binding: t; -*-
(require 'org-protocol)

;;============ Org Mode Group Start ===============
;; For org (Core Org mode configuration)
(use-package org
  :ensure t
  :init
  ;; Define org-directory if it's not already defined (e.g. from init-custom.el)
  ;; This is a placeholder; actual value should be set in init-custom.el or similar
  (unless (boundp 'org-directory)
    (defvar org-directory "~/org/"
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
      (mermaid    . t)
      (plantuml   . t))
    "Alist of org ob languages.")
  (setq inbox-dir (expand-file-name "INBOX.org" org-directory))
  (setq todo-dir (expand-file-name "TODOs.org" org-directory))
  (setq book-dir (expand-file-name "TODOs.org" org-directory)) ; Assuming book-dir is same as todo-dir based on init-org.el
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
   org-log-done 'time
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
  (setq org-startup-indented t)
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
  (add-to-list 'org-capture-templates
               '("w" "Web Capture" entry
                 (file+headline inbox-dir "Web")
                 "* %a\n%?%i"))
;;  (setq org-agenda-files (list
;;                          (expand-file-name "TODOs.org" org-directory)
;;                          (expand-file-name "INBOX.org" org-directory)
;;                          (expand-file-name "ARCHIVED.org" org-directory)
;;                          (expand-file-name "daily" org-directory)
;;                          ))
;;  (setq org-agenda-files
;;        (directory-files-recursively org-directory "\\.org$"))

  (setq org-refile-targets '((org-agenda-files :maxlevel . 4) ; 扫描 agenda 文件中的标题，最深4级
                           (nil :maxlevel . 3)))          ; 也允许归档到当前文件的标题
  ;;  (setq org-agenda-files (list org-directory))
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
  ;;(message "Configuring Org Babel. load-language-alist is: %S" load-language-alist) ; For verification
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
        ;; (org-mode-hook . valign-mode)
         (org-mode-hook . org-indent-mode)
         )
  :bind (
         ("C-c [" . org-mark-ring-goto)
         ("C-c a" . org-agenda-list)
         ;;         ("C-c z" . org-capture)
         )
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
  ;; (org-download-image-dir (expand-file-name "img/" (file-truename org-directory))) ; org-directory needs to be defined
  (org-download-heading-lvl nil)
  (org-download-image-attr-list
   '("#+ATTR_HTML: :width 65% :align center"))
  :config
  (when (boundp 'org-directory) ; Set image dir only if org-directory is defined
    (setq org-download-image-dir (expand-file-name "img/" (file-truename org-directory))))
  (add-hook 'dired-mode-hook #'org-download-enable)
  )


;; For org-modern
(use-package org-modern
  :ensure t ;; 如果你还没有安装 org-modern，请确保 t
  :hook ((org-mode . org-modern-mode)                ;; 在打开 org 文件时启用 org-modern-mode
         (org-mode . global-org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)   ;; 美化 agenda 视图
         (org-modern-mode . (lambda ()
                              "为 org-modern-mode 进行额外调整"
                              ;; 禁用 Emacs 内建的 prettify-symbols-mode，避免与 org-modern 冲突
                              (setq prettify-symbols-alist nil)
                              (prettify-symbols-mode -1)

                              ;; ----------------------------------------------------
                              ;; ----------------------------------------------------
                              ;; 使用不同的 Unicode 字符作为标题星号
                              (setq org-modern-star '("●" "-" "·"))
                              ;; 或者使用更简约的实心点 (需要兼容的字体)
                              ;;(setq org-modern-star '("●" "○" "◆" "◇"))
                              ;; 如果想完全隐藏星号，只通过缩进区分层级
                              ;; (setq org-modern-hide-stars t)


                              ;; --- 标签样式 ---
                              ;; 使用更柔和的标签背景色，或者只改变前景色
                              (setq org-modern-tag-faces '((:foreground "dim gray" :weight 'semi-bold)))

                              (setq org-pretty-entities t)
                              (setq org-modern-horizontal-rule "┈┈┈┈┈┈") ;; 虚线

                                ;; Set custom checkbox styles
                              (setq org-modern-checkbox
                                    '(("☐" . "○")
                                      ("☑" . "●")
                                      ("[-]" . "◐")))
                              ;; --- 表格 ---
                              ;; 启用更现代的表格渲染 (使用 box-drawing 字符)
                              (setq org-modern-table t)

                              ;; --- 元数据行 (如 #+TITLE, #+AUTHOR) ---
                              ;;(setq org-modern-keyword-foreground "DarkGoldenrod") ;; 改变元数据关键字的颜色
                              ;;(setq org-modern-meta-line-padding 1) ;; 增加元数据行上方的填充

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

(use-package org-modern-indent
  ;;:after org-modern
  :load-path "site-lisp/org-modern-indent"
  :commands org-modern-indent-mode
  :hook (org-mode . org-modern-indent-mode)
  ;;:config ; add late to hook
  ;;(add-hook 'org-mode-hook #'org-modern-indent-mode 90)
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
  ;; Ensure org-directory is defined before use
  (unless (boundp 'org-directory)
    (defvar org-directory "~/org/"
      "Default directory for Org files if not set in init-custom.el."))
  (setq
   org-roam-directory (file-truename org-directory)
   org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
   )
  :config
  ;;(org-roam-db-autosync-enable)
  (setq org-roam-database-connector 'sqlite-builtin)
  (org-roam-setup)
  ;;(setq org-ioam-mode-sections
  ;;      (list #'org-roam-backlinks-insert-section
  ;;            #'org-roam-reflinks-insert-section
  ;;            #'org-roam-unlinked-references-insert-section))
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain "%?"
           :target (file+head "${slug}-%<%Y%m%d%H>.org"
                              "#+title:${title}\n#+filetags: :note:\n#+SETUPFILE: ~/.emacs.d/white_clean.theme\n\n")
           :unnarrowed t)

          ("p" "Project Note" entry "* ${slug}\n:PROPERTIES:\n:Status: Active\n:Deadline:\n:END:"
           :target (file+head "note/${slug}-%<%Y%m%d%H%M%S>.org"
                              "#+title: P: ${title}\n#+category: Project\n#+filetags: :note:\n#+SETUPFILE: ~/.emacs.d/white_clean.theme\n\n")
           :unnarrowed t)

          ("w" "Work Document" entry "* ${slug}\n:PROPERTIES:\n:Status: Active\n:END:\n\n%^{Link}\n\n%? "
           :target (file+head "work/${slug}-%<%Y%m%d%H%M%S>.org"
                              "#+title: W: ${title}\n#+category: Project\n#+filetags: :vault:\n#+SETUPFILE: ~/.emacs.d/white_clean.theme\n\n")
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






  ;; Custom modeline format using the roam-aware function
  (defun my-custom-modeline-buffer-identification ()
    "Custom buffer identification for modeline with Org Roam awareness."
    (propertize (my-roam-aware-buffer-name)
                'face 'mode-line-buffer-id
                'help-echo "Buffer name
\
mouse-1: Previous buffer\nmouse-3: Next buffer"
                'mouse-face 'mode-line-highlight
                'local-map (let ((map (make-sparse-keymap)))
                            (define-key map [mode-line mouse-1] 'mode-line-previous-buffer)
                            (define-key map [mode-line mouse-3] 'mode-line-next-buffer)
                            map)))

  ;; Set the custom modeline format
  (setq-default mode-line-buffer-identification
                '(:eval (my-custom-modeline-buffer-identification)))
  (add-to-list 'display-buffer-alist
                   '("\\*org-roam\\*"
                     (display-buffer-in-side-window)
                     (side . right)
                     (slot . 0)
                     (window-width . 0.25)
                     (window-parameters . ((no-other-window . t)
                                           (no-delete-other-windows . t)))))
  ;; Codes blow are used to general a hierachy for title nodes that under a file
  (cl-defmethod org-roam-node-doom-filetitle ((node org-roam-node))
        "Return the value of \"#+title:\" (if any) from file that NODE resides in.
      If there's no file-level title in the file, return empty string."
        (or (if (= (org-roam-node-level node) 0)
                (org-roam-node-title node)
              (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
            ""))
      (cl-defmethod org-roam-node-doom-hierarchy ((node org-roam-node))
        "Return hierarchy for NODE, constructed of its file title, OLP and direct title.
        If some elements are missing, they will be stripped out."
        (let ((title     (org-roam-node-title node))
              (olp       (org-roam-node-olp   node))
              (level     (org-roam-node-level node))
              (filetitle (org-roam-node-doom-filetitle node))
              (separator (propertize " > " 'face 'shadow)))
          (cl-case level
            ;; node is a top-level file
            (0 filetitle)
            ;; node is a level 1 heading
            (1 (concat (propertize filetitle 'face '(shadow italic))
                       separator title))
            ;; node is a heading with an arbitrary outline path
            (t (concat (propertize filetitle 'face '(shadow italic))
                       separator (propertize (string-join olp " > ") 'face '(shadow italic))
                       separator title)))))

  (setq org-roam-node-display-template (concat "${type:15} ${doom-hierarchy:80} " (propertize "${tags:*}" 'face 'org-tag)))

  (cl-defmethod org-roam-node-type ((node org-roam-node))
        "Return the TYPE of NODE."
        (condition-case nil
            (file-name-nondirectory
             (directory-file-name
              (file-name-directory
               (file-relative-name (org-roam-node-file node) org-roam-directory))))
          (error "")))

  ;; https://gist.github.com/d12frosted/a60e8ccb9aceba031af243dff0d19b2e
  (defun vulpea-project-p ()
    "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (seq-find                                 ; (3)
     (lambda (type)
       (eq type 'todo))
     (org-element-map                         ; (2)
         (org-element-parse-buffer 'headline) ; (1)
         'headline
       (lambda (h)
         (org-element-property :todo-type h)))))

  (defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-project-p)
              (setq tags (cons "project" tags))
            (setq tags (remove "project" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (eq major-mode 'org-mode)
         (string-suffix-p "org" buffer-file-name)
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
                :from tags
                :left-join nodes
                :on (= tags:node-id nodes:id)
                :where (like tag (quote "%\"project\"%"))]))))

  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (vulpea-project-files)))

  (add-hook 'find-file-hook #'vulpea-project-update-tag)
  (add-hook 'before-save-hook #'vulpea-project-update-tag)

  (advice-add 'org-agenda-list :before #'vulpea-agenda-files-update)
  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
  (advice-add 'org-todo-list :before #'vulpea-agenda-files-update)

  ;; functions borrowed from `vulpea' library
  ;; https://github.com/d12frosted/vulpea/blob/6a735c34f1f64e1f70da77989e9ce8da7864e5ff/vulpea-buffer.el

  (defun vulpea-buffer-tags-get ()
    "Return filetags value in current buffer."
    (vulpea-buffer-prop-get-list "filetags" "[ :]"))

  (defun vulpea-buffer-tags-set (&rest tags)
    "Set TAGS in current buffer.

If filetags value is already set, replace it."
    (if tags
        (vulpea-buffer-prop-set
         "filetags" (concat ":" (string-join tags ":") ":"))
      (vulpea-buffer-prop-remove "filetags")))

  (defun vulpea-buffer-tags-add (tag)
    "Add a TAG to filetags in current buffer."
    (let* ((tags (vulpea-buffer-tags-get))
           (tags (append tags (list tag))))
      (apply #'vulpea-buffer-tags-set tags)))

  (defun vulpea-buffer-tags-remove (tag)
    "Remove a TAG from filetags in current buffer."
    (let* ((tags (vulpea-buffer-tags-get))
           (tags (delete tag tags)))
      (apply #'vulpea-buffer-tags-set tags)))

  (defun vulpea-buffer-prop-set (name value)
    "Set a file property called NAME to VALUE in buffer file.
If the property is already set, replace its value."
    (setq name (downcase name))
    (org-with-point-at 1
      (let ((case-fold-search t))
        (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                               (point-max) t)
            (replace-match (concat "#+" name ": " value) 'fixedcase)
          (while (and (not (eobp))
                      (looking-at "^[#:]"))
            (if (save-excursion (end-of-line) (eobp))
                (progn
                  (end-of-line)
                  (insert "\n"))
              (forward-line)
              (beginning-of-line)))
          (insert "#+" name ": " value "\n")))))

  (defun vulpea-buffer-prop-set-list (name values &optional separators)
    "Set a file property called NAME to VALUES in current buffer.
VALUES are quoted and combined into single string using
`combine-and-quote-strings'.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t.
If the property is already set, replace its value."
    (vulpea-buffer-prop-set
     name (combine-and-quote-strings values separators)))

  (defun vulpea-buffer-prop-get (name)
    "Get a buffer property called NAME as a string."
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                               (point-max) t)
        (buffer-substring-no-properties
         (match-beginning 1)
         (match-end 1)))))

  (defun vulpea-buffer-prop-get-list (name &optional separators)
    "Get a buffer property NAME as a list using SEPARATORS.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
    (let ((value (vulpea-buffer-prop-get name)))
      (when (and value (not (string-empty-p value)))
        (split-string-and-unquote value separators))))

  (defun vulpea-buffer-prop-remove (name)
    "Remove a buffer property called NAME."
  (org-with-point-at 1
    (when (re-search-forward (concat "\\(^#\\+" name ":.*\n?\\)")
                             (point-max) t)
      (replace-match ""))))
  ;; org dailies capture

  (setq org-roam-dailies-capture-templates
      '(("d" "Journal" entry "* %? %T\n"
         :if-new (file+head+olp "%<%Y-%m-%d>.org"
	  	  	        "#+title: %<%Y-%m-%d>\n#+filetags: %<:%Y:%B:>\n"
		  	        ("Journal")))
        ("m" "Most Important Thing" entry "* TODO [#A] %? :mit: \n SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))"
         :if-new (file+head+olp "%<%Y-%m-%d>.org"
			        "#+title: %<%Y-%m-%d>\n#+filetags: %<:%Y:%B:>\n"
			        ("Most Important Thing(s)")))))
  :bind (("C-c o b" . org-roam-buffer-toggle)
         ("C-c o f" . org-roam-node-find)
         ("C-c o g" . org-roam-graph)
         ("C-c o i" . org-roam-node-insert)
         ("C-c o t" . org-roam-dailies-goto-date)
         ("C-c t" . org-roam-dailies-goto-today)
         ("C-c C" . org-roam-capture)
         ("C-c c" . org-roam-dailies-capture-today)
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

(use-package ob-mermaid
  :ensure t ; Ensure it's installed if not already
  :config
  (setq ob-mermaid-cli-path "/opt/homebrew/bin/mmdc")
  )

(use-package org-web-tools
  :ensure t)

(use-package org-transclusion
  :after org
  :ensure t
  :config
  (setq org-transclusion-enable-async-http t)
  (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
  :bind(("C-c o l" . org-transclusion-add)
        )
  )

(use-package org-transclusion-http
  :after org-transclusion
  :ensure t
  :config
  (add-to-list 'org-transclusion-extensions 'org-transclusion-http)
  )

(use-package org-ql
  :ensure t
  :after org-roam
  :config
  ;; 定义 org-ql 的搜索范围，与 agenda-files 保持一致
;;  (setq org-ql-search-directories (file-expand-wildcards org-agenda-files))
  (setq org-ql-search-directories-files (list org-directory))


  ;; 定义我们想要在 Sidebar 中看到的"视图"
  (setq org-ql-views
        '(
           ("📅 Today's Agenda"
           :query (or (deadline :on today) (scheduled :on today))
           :buffers-files  org-roam-list-files
           :sort (deadline)
           )

           ("🔥 Overdue Items"
           :query (and (not (done))
                     (or (scheduled :to -1)  ; relative date: yesterday
                         (deadline :to -1)))  ; relative date: yesterday
           :buffers-files  org-roam-list-files
           :sort (deadline scheduled))

           ("⚡ Upcoming Agenda (7-Day)"
           :query (and (todo)
                       (or (scheduled :from today)
                           (deadline :from today)))
           :buffers-files  org-roam-list-files
           )

          ("✅ Completed This Week"
           :query (and (done) (closed :from -7))
           :buffers-files  org-roam-list-files
           :sort (closed))

          ("🤔 Waiting For"
           :query (tags "waiting")
           :buffers-files  org-roam-list-files
           :sort (priority))

          ("🚧 Blocked"
           :query (tags "blocked")
           :buffers-files  org-roam-list-files
           )

          ("📂 Active Projects"
           :query (and (level 1) (property "CATEGORY" "Project")
                       (property "Status" "Active"))
           :buffers-files  org-roam-list-files
           )
          )
  )
)

(use-package org-sidebar
  :ensure t
  :config
  ;; 创建兼容的侧边栏函数
  (defun my-minimal-sidebar-today (source-buffer)
    "今日议程"
    (let ((display-buffer
           (generate-new-buffer (format "org-sidebar<%s>" (buffer-name source-buffer))))
          (title "📅 Today's Agenda")
         (files (org-roam-list-files)))
      (with-current-buffer display-buffer
        (setf org-sidebar-source-buffer source-buffer))
      (save-window-excursion
        (org-ql-search (mapcar #'find-file-noselect files)
          '(or (deadline :on today) (scheduled :on today))
          :narrow t :sort '(deadline)
          :buffer display-buffer
          :title title))
      display-buffer))

  (defun my-minimal-sidebar-overdue (source-buffer)
    "进行中项目"
    (let* ((display-buffer
            (generate-new-buffer (format "org-sidebar<%s>" (buffer-name source-buffer))))
           (title "🔥 Overdue Items")
           (files (org-roam-list-files)))
      (with-current-buffer display-buffer
        (setf org-sidebar-source-buffer source-buffer))
      (save-window-excursion
        (org-ql-search (mapcar #'find-file-noselect files)
          `(and (not (done))
                (or (scheduled :to -1)
                    (deadline :to -1)))
          :narrow t :sort '(deadline scheduled)
          :buffer display-buffer
          :title title))
      display-buffer))

   (defun my-minimal-sidebar-project (source-buffer)
    "当前项目"
    (let* ((display-buffer
            (generate-new-buffer (format "org-sidebar<%s>" (buffer-name source-buffer))))
           (title "📂 Active Projects")
           (files (org-roam-list-files)))
      (with-current-buffer display-buffer
        (setf org-sidebar-source-buffer source-buffer))
      (save-window-excursion
        (org-ql-search (mapcar #'find-file-noselect files)
          `(and (level 1) (property "CATEGORY" "Project")
                (property "Status" "Active"))
          :narrow t
          :sort '(priority)
          :buffer display-buffer
          :title title))
      display-buffer))


  ;; 设置默认函数
  (setq org-sidebar-default-fns
        '(my-minimal-sidebar-today
          my-minimal-sidebar-overdue
          my-minimal-sidebar-project))

  ;; 绑定一个方便的快捷键来开关侧边栏，F8 是个不错的选择
  (global-set-key (kbd "<f9>") #'org-sidebar-toggle)

  )
;;============ Org Mode Group End ===============

(provide 'setup-org)
