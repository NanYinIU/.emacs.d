;;; init-funcs.el --- Define functions.	-*- lexical-binding: t -*-

;;; Commentary:
;; Utility functions used throughout the configuration

;;; Code:

(require 'cl-lib)

;; Suppress warnings
(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

;; File and buffer
(defun revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer")
  (text-scale-set 0)
  (widen)
  (revert-buffer t t))

(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied '%s'" filename))
      (warn "Current buffer is not attached to a file!"))))

(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

(defun reload-init-file ()
  "Reload init file."
  (interactive)
  (load-file user-init-file))

;; Selection functions
(defun selected-region-or-symbol-at-point ()
  "Return the selected region, otherwise return the symbol at point."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol t)))

;; Feature and display checks
(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and (featurep 'nerd-icons)
       (require 'nerd-icons nil t)))

(defun childframe-workable-p ()
  "Whether childframe is workable."
  (and (>= emacs-major-version 26)
       (not noninteractive)
       (not emacs-basic-display)
       (or (display-graphic-p)
           (featurep 'tty-child-frames))
       (eq (frame-parameter (selected-frame) 'minibuffer) 't)))

(defun childframe-completion-workable-p ()
  "Whether childframe completion is workable."
  (and (boundp 'centaur-completion-style)
       (eq centaur-completion-style 'childframe)
       (childframe-workable-p)))

;; Configuration reload
(defun my-reload-config-and-custom-lisp (&optional custom-lisp-dir)
  "Reload user-init-file and all .el/.elc files in CUSTOM-LISP-DIR."
  (interactive
   (list (read-directory-name "Path to your custom Lisp directory: "
                              (expand-file-name "lisp" user-emacs-directory)
                              nil t)))
  ;; 1. Reload main configuration file
  (if (file-exists-p user-init-file)
      (progn
        (message "Reloading %s..." user-init-file)
        (load-file user-init-file)
        (message "Reloaded %s." user-init-file))
    (message "User init file %s not found." user-init-file))

  ;; 2. Reload all .el or .elc files in the specified directory
  (if (file-directory-p custom-lisp-dir)
      (let ((load-suffixes '(".elc" ".el")) ; Prefer .elc files
            (reloaded-files 0)
            (failed-files 0))
        (message "Reloading Lisp files from %s..." custom-lisp-dir)
        (dolist (file (directory-files custom-lisp-dir t "\\.elc?\\'" nil))
          (condition-case err
              (progn
                (message "  Reloading %s..." (file-name-nondirectory file))
                (load-file file)
                (setq reloaded-files (1+ reloaded-files)))
            (error
             (message "  Failed to reload %s: %s" (file-name-nondirectory file) err)
             (setq failed-files (1+ failed-files)))))
        (message "Finished reloading from %s: %d file(s) reloaded, %d failed."
                 custom-lisp-dir reloaded-files failed-files))
    (message "Custom Lisp directory %s not found or is not a directory." custom-lisp-dir)))

;; Process list enhancement
(with-no-warnings
  (defun my-list-processes--prettify ()
    "Prettify process list."
    (when-let* ((entries tabulated-list-entries))
      (setq tabulated-list-entries nil)
      (dolist (p (process-list))
        (when-let* ((val (cadr (assoc p entries)))
                    (name (aref val 0))
                    (pid (aref val 1))
                    (status (aref val 2))
                    (status (list status
                                  'face
                                  (if (memq status '(stop exit closed failed))
                                      'error
                                    'success)))
                    (buf-label (aref val 3))
                    (tty (list (aref val 4) 'face 'font-lock-doc-face))
                    (thread (list (aref val 5) 'face 'font-lock-doc-face))
                    (cmd (list (aref val 6) 'face 'completions-annotations)))
          (push (list p (vector name pid status buf-label tty thread cmd))
                tabulated-list-entries)))))
  (advice-add #'list-processes--refresh :after #'my-list-processes--prettify))

 (defun my-org/org-return (&optional indent)
      "Goto next table row or insert a newline.
Calls `org-table-next-row' or `newline', depending on context.
When optional INDENT argument is non-nil, call
`newline-and-indent' instead of `newline'.
When `org-return-follows-link' is non-nil and point is on
a timestamp or a link, call `org-open-at-point'.  However, it
will not happen if point is in a table or on a \"dead\"
object (e.g., within a comment).  In these case, you need to use
`org-open-at-point' directly."
      (interactive)
      (let ((context (if org-return-follows-link (org-element-context)
                       (org-element-at-point))))
        (cond
         ;; In a table, call `org-table-next-row'.  However, before first
         ;; column or after last one, split the table.
         ((or (and (eq 'table (org-element-type context))
                   (not (eq 'table.el (org-element-property :type context)))
                   (>= (point) (org-element-property :contents-begin context))
                   (< (point) (org-element-property :contents-end context)))
              (org-element-lineage context '(table-row table-cell) t))
          (if (or (looking-at-p "[ \t]*$")
                  (save-excursion (skip-chars-backward " \t") (bolp)))
              (insert "\n")
            (org-table-justify-field-maybe)
            (call-interactively #'org-table-next-row)))
         ;; On a link or a timestamp, call `org-open-at-point' if
         ;; `org-return-follows-link' allows it.  Tolerate fuzzy
         ;; locations, e.g., in a comment, as `org-open-at-point'.
         ((and org-return-follows-link
               (or (and (eq 'link (org-element-type context))
                        ;; Ensure point is not on the white spaces after
                        ;; the link.
                        (let ((origin (point)))
                          (org-with-point-at (org-element-property :end context)
                            (skip-chars-backward " \t")
                            (> (point) origin))))
                   (org-in-regexp org-ts-regexp-both nil t)
                   (org-in-regexp org-tsr-regexp-both nil t)
                   (org-in-regexp org-any-link-re nil t)))
          (call-interactively #'org-open-at-point))
         ;; Insert newline in heading, but preserve tags.
         ((and (not (bolp))
               (let ((case-fold-search nil))
                 (org-match-line org-complex-heading-regexp)))
          ;; At headline.  Split line.  However, if point is on keyword,
          ;; priority cookie or tags, do not break any of them: add
          ;; a newline after the headline instead.
          (let ((tags-column (and (match-beginning 5)
                                  (save-excursion (goto-char (match-beginning 5))
                                                  (current-column))))
                (string
                 (when (and (match-end 4) (org-point-in-group (point) 4))
                   (delete-and-extract-region (point) (match-end 4)))))
            ;; Adjust tag alignment.
            (cond
             ((not (and tags-column string)))
             (org-auto-align-tags (org-align-tags))
             (t (org--align-tags-here tags-column))) ;preserve tags column
            (end-of-line)
            (org-show-entry)
            (if indent (newline-and-indent) (newline))
            (when string (save-excursion (insert (org-trim string))))))
         ;; In a list, make sure indenting keeps trailing text within.
         ((and indent
               (not (eolp))
               (org-element-lineage context '(item)))
          (let ((trailing-data
                 (delete-and-extract-region (point) (line-end-position))))
            (newline-and-indent)
            (save-excursion (insert trailing-data))))
         ((and (eolp) (org-at-item-p))
          (end-of-visible-line)
          (org-insert-item (org-at-item-checkbox-p)))
         (t
          ;; Do not auto-fill when point is in an Org property drawer.
          (let ((auto-fill-function (and (not (org-at-property-p))
                                         auto-fill-function)))
            (if indent
                (newline-and-indent)
              (newline)))))))


;;    (define-key org-mode-map (kbd "RET")
;;                'my-org/org-return)


(defun copy-file-name-to-clipboard (do-not-strip-prefix)
  "Copy the current buffer file name to the clipboard. The path will be relative to the project's root directory, if set. Invoking with a prefix argument copies the full path."
  (interactive "P")
  (letrec
      ((fullname (if (equal major-mode 'dired-mode) default-directory (buffer-file-name)))
       (root (project-root (project-current)))
       (relname (file-relative-name fullname root))
       (should-strip (and root (not do-not-strip-prefix)))
       (filename (if should-strip relname fullname)))
    (kill-new filename)
    (message "Copied buffer file name '%s' to the clipboard." filename)))

(provide 'init-funcs)
;;; init-funcs.el ends here
