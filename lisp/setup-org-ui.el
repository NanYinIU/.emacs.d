;;; setup-org-ui.el --- Org mode visual enhancements -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure Org mode visual enhancements and UI improvements

;;; Code:

(require 'init-const)
(require 'init-funcs)

;; Org Appear - Auto-reveal markup characters
(use-package org-appear
  :ensure t
  :defer t
  :diminish
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks t))

;; Org Download - Drag and drop images
(use-package org-download
  :ensure t
  :defer t
  :diminish
  :commands (org-download-yank org-download-clipboard org-download-screenshot 
                           org-download-file org-download-enable)
  :custom
  (org-download-display-inline-images 'posframe)
  (org-download-method 'attach)
  (org-download-heading-lvl nil)
  (org-download-image-attr-list
   '("#+ATTR_HTML: :width 65% :align center"))
  :config
  ;; Set image directory if org-directory is defined
  (when (boundp 'org-directory)
    (setq org-download-image-dir (expand-file-name "img/" (file-truename org-directory))))
  (add-hook 'dired-mode-hook #'org-download-enable))

;; Org Modern - Modern styling for Org documents
(use-package org-modern
  :ensure t
  :defer t
  :diminish
  :hook ((org-mode . org-modern-mode)
         (org-mode . global-org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)
         (org-modern-mode . (lambda ()
                              "Additional adjustments for org-modern-mode"
                              ;; Disable built-in prettify-symbols-mode
                              (setq prettify-symbols-alist nil)
                              (prettify-symbols-mode -1)
                              
                              ;; Configure heading stars
                              (setq org-modern-star '("●" "-" "·"))
                              
                              ;; Tag styling
                              (setq org-modern-tag-faces 
                                    '((:foreground "dim gray" :weight 'semi-bold)))
                              
                              ;; Entity and horizontal rule settings
                              (setq org-pretty-entities t)
                              (setq org-modern-horizontal-rule "┈┈┈┈┈┈")
                              
                              ;; Custom checkbox styles
                              (setq org-modern-checkbox
                                    '(("☐" . "○")
                                      ("☑" . "●")
                                      ("[-]" . "◐")))
                              
                              ;; Enable modern table rendering
                              (setq org-modern-table t)
                              
                              ;; Agenda time grid colors
                              (setq org-modern-agenda-time-grid-custom-colors t))))
  :config
  (setq org-modern-todo t
        org-modern-priority t))

;; Org Modern Indent - Enhanced indentation for Org mode
(use-package org-modern-indent
  :defer t
  :diminish
  :load-path "site-lisp/org-modern-indent"
  :commands org-modern-indent-mode
  :hook (org-mode . org-modern-indent-mode))

;; TOC Org - Automatic table of contents
(use-package toc-org
  :ensure t
  :defer t
  :diminish
  :hook (org-mode . toc-org-mode))

;; Org Web Tools - Web integration for Org mode
(use-package org-web-tools
  :ensure t
  :defer t
  :diminish)

;; Org Transclusion - Content inclusion from other files
(use-package org-transclusion
  :after org
  :ensure t
  :defer t
  :diminish
  :config
  (setq org-transclusion-enable-async-http t)
  (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
  :bind (("C-c o l" . org-transclusion-add)))

;; Org Transclusion HTTP - HTTP support for transclusion
(use-package org-transclusion-http
  :after org-transclusion
  :ensure t
  :defer t
  :diminish
  :config
  (add-to-list 'org-transclusion-extensions 'org-transclusion-http))

;; Org Protocol - Capture from external applications
(use-package org-protocol
  :ensure nil  ; Built-in package
  :defer t)

;; Babel Mermaid support
(use-package ob-mermaid
  :ensure t
  :defer t
  :diminish
  :config
  (setq ob-mermaid-cli-path "/opt/homebrew/bin/mmdc"))

(provide 'setup-org-ui)
;;; setup-org-ui.el ends here