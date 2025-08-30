;;; setup-org-core.el --- Core Org mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure core Org mode functionality and basic settings

;;; Code:

(require 'init-const)
(require 'init-funcs)

;; Core Org mode configuration
(use-package org
  :ensure t
  :demand t
  :init
  ;; Ensure org-directory is defined
  (unless (boundp 'org-directory)
    (defvar org-directory "~/org/"
      "Default directory for Org files if not set in init-custom.el."))
  
  ;; Basic Org mode settings
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-startup-with-inline-images t
        org-hierarchical-todo- t
        org-startup-indented t)
  
  ;; Define directory paths
  (setq inbox-dir (expand-file-name "INBOX.org" org-directory))
  (setq todo-dir (expand-file-name "TODOs.org" org-directory))
  (setq book-dir (expand-file-name "TODOs.org" org-directory))
  
  ;; Babel language support
  (defconst load-language-alist
    '((emacs-lisp . t)
      (python     . t)
      (js         . t)
      (css        . t)
      (java       . t)
      (shell      . t)
      (mermaid    . t)
      (plantuml   . t))
    "Alist of org ob languages.")
  
  ;; TODO keywords and faces
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "NEXT(n)" "WAIT(w)" "FIXME(f)" "|" "DONE(d!)" "CANCELLED(c!)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)"))
        org-log-done 'time
        org-todo-keyword-faces
        '(("NEXT" . "#98C379")
          ("FIXME" . "#98C379")
          ("TODO" . "#E5C07B")
          ("PROJ" . "#E5C07B")
          ("WAIT" . "#56B6C2")
          ("MAYBE" . "#ABB2BF")
          ("[ ]" . "#98C379")
          ("[-]" . "#E5C07B")
          ("[?]" . "#E06C75")))
  
  ;; TODO state triggers
  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          (done ("WAITING"))
          ("TODO" ("WAITING") ("CANCELLED"))
          ("NEXT" ("WAITING") ("CANCELLED"))
          ("DONE" ("WAITING") ("CANCELLED"))))
  
  :config
  ;; Load babel languages
  (org-babel-do-load-languages 'org-babel-load-languages load-language-alist)
  
  ;; Refile targets
  (setq org-refile-targets '((org-agenda-files :maxlevel . 4)
                             (nil :maxlevel . 3)))
  
  ;; Key bindings
  (define-key org-mode-map (kbd "RET") 'my-org/org-return)
  
  ;; Hooks
  (add-hook 'org-indent-mode 
            (lambda ()
              (diminish 'org-indent-mode)
              (make-variable-buffer-local 'show-paren-mode)
              (setq show-paren-mode nil)))
  
  :bind (("C-c [" . org-mark-ring-goto)
         ("C-c a" . org-agenda-list))
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images)
         (org-mode . org-indent-mode)))

(provide 'setup-org-core)
;;; setup-org-core.el ends here