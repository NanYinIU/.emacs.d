;;; setup-org-roam.el --- Org Roam configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure Org Roam for note-taking and knowledge management

;;; Code:

(require 'init-const)
(require 'init-funcs)

;; Org Roam - Zettelkasten note-taking system
(use-package org-roam
  :ensure t
  :defer t
  :diminish
  :after org
  :commands (org-roam-mode org-roam-capture org-roam-node-find)
  :init
  ;; Ensure org-directory is defined
  (unless (boundp 'org-directory)
    (defvar org-directory "~/org/"
      "Default directory for Org files if not set in init-custom.el."))
  
  ;; Basic Org Roam configuration
  (setq org-roam-directory (file-truename org-directory)
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
        org-roam-completion-everywhere t)
  
  :config
  ;; Database setup
  (setq org-roam-database-connector 'sqlite-builtin)
  (org-roam-setup)
  
  ;; Capture templates for different note types
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
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
          ("la" "Arrays" entry "\n** ${title} \n:PROPERTIES:\n:ID: %(org-id-new)\n:END:\n*** Reference\n%^{reference}\n*** Question\n \n*** Solution \n "
           :target (file+olp "Leetcode.org" ("Arrays"))
           :unnarrowed t)
          ("ll" "List" entry "\n** ${title} \n:PROPERTIES:\n:ID: %(org-id-new)\n:END:\n*** Reference\n%^{reference}\n*** Question\n \n*** Solution \n "
           :target (file+olp "Leetcode.org" ("List"))
           :unnarrowed t)))
  
  ;; Reference capture templates
  (setq org-roam-capture-ref-templates
        '(("a" "Annotation" plain
           "%U ${body}\n"
           :target (file+head "reference/${slug}.org" "#+title: ${title}\n#+roam_ref: ${ref}\n#+filetags:\n\n")
           :unnarrowed t)
          ("r" "ref" entry
           "* TODO %<%I:%M %p>: %:annotation :Bookmark: \n SCHEDULED: %T \n  "
           :target (file+olp (format "%s/%s" "INBOX.org") ("Bookmarks"))
           :immediate-finish t)))
  
  ;; Enhanced node display template
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  
  ;; Display configuration
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.25)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  
  ;; Node hierarchy and type methods
  (cl-defmethod org-roam-node-doom-filetitle ((node org-roam-node))
    "Return the value of \"#+title:\" from file that NODE resides in."
    (or (if (= (org-roam-node-level node) 0)
            (org-roam-node-title node)
          (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
        ""))
  
  (cl-defmethod org-roam-node-doom-hierarchy ((node org-roam-node))
    "Return hierarchy for NODE."
    (let ((title (org-roam-node-title node))
          (olp (org-roam-node-olp node))
          (level (org-roam-node-level node))
          (filetitle (org-roam-node-doom-filetitle node))
          (separator (propertize " > " 'face 'shadow)))
      (cl-case level
        (0 filetitle)
        (1 (concat (propertize filetitle 'face '(shadow italic))
                   separator title))
        (t (concat (propertize filetitle 'face '(shadow italic))
                   separator (propertize (string-join olp " > ") 'face '(shadow italic))
                   separator title)))))
  
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  
  ;; Set enhanced display template
  (setq org-roam-node-display-template 
        (concat "${type:15} ${doom-hierarchy:80} " 
                (propertize "${tags:*}" 'face 'org-tag)))
  
  :bind (("C-c o b" . org-roam-buffer-toggle)
         ("C-c o f" . org-roam-node-find)
         ("C-c o g" . org-roam-graph)
         ("C-c o i" . org-roam-node-insert)
         ("C-c o t" . org-roam-dailies-goto-date)
         ("C-c t" . org-roam-dailies-goto-today)
         ("C-c C" . org-roam-capture)
         ("C-c c" . org-roam-dailies-capture-today)))

;; Org Roam UI - Visual interface for Org Roam
(use-package org-roam-ui
  :ensure t
  :defer t
  :diminish
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme nil
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(provide 'setup-org-roam)
;;; setup-org-roam.el ends here