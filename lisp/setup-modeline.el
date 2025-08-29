;;; setup-modeline --- A minimal mode line -*- lexical-binding: t; -*-

;; Standalone function for default modeline
  (defun my-roam-aware-buffer-name ()
    "Return a formatted buffer name for Org Roam files, or default buffer name for others."
    (if (and (boundp 'org-roam-directory)
             org-roam-directory
             (stringp buffer-file-name)
             (require 's nil 'noerror)
             (s-starts-with-p (file-truename org-roam-directory) (file-truename buffer-file-name))
             (s-ends-with-p ".org" buffer-file-name t))

        ;; Process Org Roam files
        (let* ((filename (file-name-nondirectory buffer-file-name))
               (name-part (file-name-sans-extension filename))
               (name-part-spaced (subst-char-in-string ?_ ?\s name-part))
               (icon "")
               (display-string nil))

          (cond
           ;; Daily files: "2023-01-01" or "20230101"
           ((string-match "^\\([0-9]\\{4\\}\\)[-_]?\\([0-9]\\{2\\}\\)[-_]?\\([0-9]\\{2\\}\\)$" name-part-spaced)
            (let ((year (match-string 1 name-part-spaced))
                  (month (match-string 2 name-part-spaced))
                  (day (match-string 3 name-part-spaced)))
              (setq display-string (format "%s%s-%s-%s" icon year month day))))

           ;; Title-Date format: "我的笔记-20230101120000"
           ((string-match "^\\(.*\\)-\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*$" name-part-spaced)
            (let ((title-part (s-trim (match-string 1 name-part-spaced)))
                  (year  (match-string 2 name-part-spaced))
                  (month (match-string 3 name-part-spaced))
                  (day   (match-string 4 name-part-spaced)))
              (if (not (s-blank? title-part))
                  (setq display-string (format "%s(%s%s-%s-%s)" title-part icon year month day)))))

           ;; Date-Title format: "20230101120000-我的笔记"
           ((string-match "^\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-\\(.*\\)$" name-part-spaced)
            (let ((year (match-string 1 name-part-spaced))
                  (month (match-string 2 name-part-spaced))
                  (day (match-string 3 name-part-spaced))
                  (title-part (s-trim (match-string 4 name-part-spaced))))
              (if (not (s-blank? title-part))
                  (setq display-string (format "%s(%s-%s-%s) %s" icon year month day title-part))
                (setq display-string (format "%s(%s-%s-%s)" icon year month day))))))

          ;; Return formatted string or fallback to filename
          (or display-string name-part-spaced))

      ;; For non-Roam files, return buffer name
      (buffer-name)))

(defvar lsp-modeline--code-actions-string nil)
 (setq-default mode-line-format
   '("%e"
 	(:propertize " " display (raise +0.3)) ;; Top padding
 	(:propertize " " display (raise -0.3)) ;; Bottom padding

 	(:propertize "  " face font-lock-comment-face)
 	mode-line-frame-identification
 	mode-line-buffer-identification

 	;; Version control info
 	(:eval (when-let (vc vc-mode)
 			 ;; Use a pretty branch symbol in front of the branch name
 			 (list (propertize "   " 'face 'font-lock-comment-face)
                    ;; Truncate branch name to 50 characters
 				   (propertize (truncate-string-to-width
                                 (substring vc 5) 50)
 							   'face 'font-lock-comment-face))))

 	;; Add space to align to the right
 	(:eval (propertize
 			 " " 'display
 			 `((space :align-to
 					  (-  (+ right right-fringe right-margin)
 						 ,(+ 3
                              (string-width (or lsp-modeline--code-actions-string ""))
                              (string-width "%4l:3%c")))))))

     ;; LSP code actions
     (:eval (or lsp-modeline--code-actions-string ""))

 	 ;; Line and column numbers
	 (:propertize "%4l:%c" face mode-line-buffer-id)))



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
         ((string-match "^\\([0-9]\\{4\\}\\)[-_]?\\([0-9]\\{2\\}\\)[-_]?\\([0-9]\\{2\\}\\)$" name-part-spaced)
          (let ((year (match-string 1 name-part-spaced))
                (month (match-string 2 name-part-spaced))
                (day (match-string 3 name-part-spaced)))
            ;; 可选: 如果希望日志文件必须在特定的 "daily/" 子目录下，可以在这里添加检查
            ;; (if (s-contains-p "/daily/" buffer-file-name t) ... )
            (setq display-string (format "%s%s-%s-%s" icon year month day))))

         ((string-match "^\\(.*\\)-\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*$" name-part-spaced)
          (let ((title-part (s-trim (match-string 1 name-part-spaced))) ; 捕获标题并去除首尾空格
                (year  (match-string 2 name-part-spaced))
                (month (match-string 3 name-part-spaced))
                (day   (match-string 4 name-part-spaced)))
            (if (not (s-blank? title-part)) ; 确保标题部分不为空
                (setq display-string (format "%s(%s%s-%s-%s)" title-part icon year month day)))))

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

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 24)
  (setq doom-modeline-bar-width 4)
  (setq doom-modeline-hud nil)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-time-icon nil)
  (setq doom-modeline-buffer-name t)
  (setq doom-modeline-vcs-icon t)
  (setq doom-modeline-modal-modern-icon t)
  )

  ;; Doom-modeline advice (commented out since you're using default modeline)
  (with-eval-after-load 'doom-modeline
    (advice-add 'doom-modeline-buffer-file-name
                :around #'my-doom-modeline-roam-aware-buffer-file-name))

(provide 'setup-modeline)
;;; setup-modeline.el ends here
