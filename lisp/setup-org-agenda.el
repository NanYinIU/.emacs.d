;;; setup-org-agenda.el --- Org agenda and scheduling configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure Org agenda, scheduling, and time management

;;; Code:

(require 'init-const)
(require 'init-funcs)

;; Org agenda configuration
(use-package org
  :ensure nil  ; Already loaded in setup-org-core
  :config
  ;; Basic agenda settings
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-include-diary t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-start-with-log-mode t
        org-agenda-log-mode-items '(closed clock state)
        org-agenda-use-time-grid t)

  ;; Agenda prefix format and display settings
  (setq org-agenda-prefix-format
        '((agenda    . "  %i %-12:c%?-12t% s ")
          (timeline  . "  %-6t %6e ")
          (todo      . "  %-6t %6e ")
          (tags      . "  %i %-12:c ")
          (search    . "%l"))
        org-agenda-tags-column 80
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-breadcrumbs-separator " ❱ "
        org-agenda-current-time-string " ┈┈┈┈┈┈┈┈┈┈┈ now")

  ;; Agenda sorting strategy
  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up effort-up priority-down category-keep)
          (todo   priority-up effort-up todo-state-up category-keep)
          (tags   priority-down category-keep)
          (search category-keep)))

  ;; Time grid configuration
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (300 600 900 1200 1500 1800 2100 2400)
          "......"
          "-----------------------------------------------------")))

;; Org QL - Advanced query language for Org files
(use-package org-ql
  :ensure t
  :defer t
  :diminish
  :after org-roam
  :config
  ;; Define search directories
  (setq org-ql-search-directories-files (list org-directory))

  ;; Define agenda views for Org Sidebar
  (setq org-ql-views
        '(("📅 Today's Agenda"
           :query (or (deadline :on today) (scheduled :on today))
           :buffers-files org-roam-list-files
           :sort (deadline))

          ("🔥 Overdue Items"
           :query (and (not (done))
                     (or (scheduled :to -1)
                         (deadline :to -1)))
           :buffers-files org-roam-list-files
           :sort (deadline scheduled))

          ("⚡ Upcoming Agenda (7-Day)"
           :query (and (todo)
                       (or (scheduled :from today)
                           (deadline :from today)))
           :buffers-files org-roam-list-files)

          ("✅ Completed This Week"
           :query (and (done) (closed :from -7))
           :buffers-files org-roam-list-files
           :sort (closed))

          ("🤔 Waiting For"
           :query (tags "waiting")
           :buffers-files org-roam-list-files
           :sort (priority))

          ("🚧 Blocked"
           :query (tags "blocked")
           :buffers-files org-roam-list-files)

          ("📂 Active Projects"
           :query (and (level 1) (property "CATEGORY" "Project")
                       (property "Status" "Active"))
           :buffers-files org-roam-list-files))))

;; Org Sidebar - Visual agenda interface
(use-package org-sidebar
  :ensure t
  :defer t
  :diminish
  :config
  ;; Custom header line format for Org QL views
  (defun my-org-ql-view-header-line-format (orig-fun &rest args)
    "Remove 'View:' prefix from org-ql view header."
    (let ((result (apply orig-fun args)))
      (when (stringp result)
        (setq result (replace-regexp-in-string "View:" "" result)))
      result))

  (advice-add 'org-ql-view--header-line-format :around #'my-org-ql-view-header-line-format)

  ;; Custom sidebar functions
  (defun my-minimal-sidebar-today (source-buffer)
    "Show today's agenda in sidebar."
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
          :super-groups '((:auto-planning))
          :buffer display-buffer
          :title title))
      display-buffer))

  (defun my-minimal-sidebar-overdue (source-buffer)
    "Show overdue items in sidebar."
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
          :super-groups '((:auto-planning))
          :buffer display-buffer
          :title title))
      display-buffer))

  (defun my-minimal-sidebar-project (source-buffer)
    "Show active projects in sidebar."
    (let* ((display-buffer
            (generate-new-buffer (format "org-sidebar<%s>" (buffer-name source-buffer))))
           (title "📂 Active Projects")
           (files (sort (org-roam-list-files)
                       (lambda (a b)
                         (time-less-p (file-attribute-modification-time
                                      (file-attributes b))
                                     (file-attribute-modification-time
                                      (file-attributes a)))))))
      (with-current-buffer display-buffer
        (setf org-sidebar-source-buffer source-buffer))
      (save-window-excursion
        (org-ql-search (mapcar #'find-file-noselect files)
          `(and (level 1) (property "CATEGORY" "Project")
                (property "Status" "Active"))
          :narrow t
          :buffer display-buffer
          :title title))
      display-buffer))

  ;; Set default sidebar functions
  (setq org-sidebar-default-fns
        '(my-minimal-sidebar-today
          my-minimal-sidebar-overdue
          my-minimal-sidebar-project))

  ;; Bind toggle function to F9
  (global-set-key (kbd "<f9>") #'org-sidebar-toggle))

(provide 'setup-org-agenda)
;;; setup-org-agenda.el ends here