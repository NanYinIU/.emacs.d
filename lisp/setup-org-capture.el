;;; setup-org-capture.el --- Org capture templates and workflows -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure Org capture templates and workflow management functions

;;; Code:

(require 'init-const)
(require 'init-funcs)

;; Basic capture templates setup
(use-package org
  :ensure nil  ; Already loaded in setup-org-core
  :config
  ;; Initialize capture templates
  (setq org-capture-templates nil)
  
  ;; Basic capture templates
  (add-to-list 'org-capture-templates
               '("i" "INBOX" entry
                 (file+headline inbox-dir "REFILE")
                 "\n* TODO %^{内容}\n%u"))
  
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
                 "* %a\n%?%i")))

;; Project management functions for Org Roam
(defun vulpea-project-p ()
  "Return non-nil if current buffer has any todo entry."
  (seq-find
   (lambda (type)
     (eq type 'todo))
   (org-element-map
       (org-element-parse-buffer 'headline)
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
        (setq tags (seq-uniq tags))
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
  "Return a list of note files containing 'project' tag."
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

;; Buffer property management functions
(defun vulpea-buffer-tags-get ()
  "Return filetags value in current buffer."
  (vulpea-buffer-prop-get-list "filetags" "[ :]"))

(defun vulpea-buffer-tags-set (&rest tags)
  "Set TAGS in current buffer."
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
  "Set a file property called NAME to VALUE in buffer file."
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
  "Set a file property called NAME to VALUES in current buffer."
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
  "Get a buffer property NAME as a list using SEPARATORS."
  (let ((value (vulpea-buffer-prop-get name)))
    (when (and value (not (string-empty-p value)))
      (split-string-and-unquote value separators))))

(defun vulpea-buffer-prop-remove (name)
  "Remove a buffer property called NAME."
  (org-with-point-at 1
    (when (re-search-forward (concat "\\(^#\\+" name ":.*\n?\\)")
                             (point-max) t)
      (replace-match ""))))

;; Org Roam dailies configuration
(use-package org-roam
  :ensure nil  ; Already loaded in setup-org-roam
  :config
  ;; Dailies capture templates
  (setq org-roam-dailies-capture-templates
        '(("d" "Journal" entry "* %? %T\n"
           :if-new (file+head+olp "%<%Y-%m-%d>.org"
                                  "#+title: %<%Y-%m-%d>\n#+filetags: %<:%Y:%B:>\n"
                                  ("Journal")))
          ("m" "Most Important Thing" entry "* TODO [#A] %? :mit: \n SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))"
           :if-new (file+head+olp "%<%Y-%m-%d>.org"
                                  "#+title: %<%Y-%m-%d>\n#+filetags: %<:%Y:%B:>\n"
                                  ("Most Important Thing(s)"))))))

;; Setup hooks for project management
(add-hook 'find-file-hook #'vulpea-project-update-tag)
(add-hook 'before-save-hook #'vulpea-project-update-tag)
(advice-add 'org-agenda-list :before #'vulpea-agenda-files-update)
(advice-add 'org-agenda :before #'vulpea-agenda-files-update)
(advice-add 'org-todo-list :before #'vulpea-agenda-files-update)

(provide 'setup-org-capture)
;;; setup-org-capture.el ends here