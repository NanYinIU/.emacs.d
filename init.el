;; packages
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))


;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Better defaults
;; (setq initial-scratch-message nil)
(setq inhibit-splash-screen t)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
;; (setq kill-whole-line t)                   ; Kill line including '\n'

(setq-default major-mode 'text-mode)
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; gargage message
;;(setq garbage-collection-messages t)

;; UI
;;(load-theme 'wombat t)
(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; 显示行号
(global-hl-line-mode)
(global-display-line-numbers-mode)
;; Basic modes
(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)


(setq auto-save-default nil)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
;; 更改光标的样式
;;(setq cursor-type 'bar)
(icomplete-mode 1)

(require 'package)



;; Key Modifiers
(cond
 ((eq system-type 'windows-nt)
  ;; make PC keyboard's Win key or other to type Super or Hyper
  ;; (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super     ; Left Windows key
        w32-apps-modifier 'hyper)       ; Menu/App key
  (w32-register-hot-key [s-t]))
 ((eq window-system 'mac)
  ;; Compatible with Emacs Mac port
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super)
  (global-set-key [(super a)] #'mark-whole-buffer)
  (global-set-key [(super v)] #'yank)
  (global-set-key [(super c)] #'kill-ring-save)
  (global-set-key [(super s)] #'save-buffer)
  (global-set-key [(super l)] #'goto-line)
  (global-set-key [(super w)] #'delete-frame)
  (global-set-key [(super z)] #'undo)))
;; Keybindings
(global-set-key (kbd "<C-return>") #'rectangle-mark-mode)

(defun revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer")
  (text-scale-set 0)
  (widen)
  (revert-buffer t t))
(global-set-key (kbd "<f5>") #'revert-current-buffer)

;; Add "lisp" and "site-lisp" to the beginning of `load-path`
(defun update-load-path (&rest _)
  ;; "Update the `load-path` to prioritize personal configurations."
  (dolist (dir '("lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))
;; Add subdirectories inside "site-lisp" to `load-path`

;;(defun add-subdirs-to-load-path (&rest _)
;;  "Recursively add subdirectories in `site-lisp` to `load-path`.

;; Avoid placing large files like EAF in `site-lisp` to prevent slow startup."
;;  (let ((default-directory (expand-file-name "lisp" user-emacs-directory)))
;;    (normal-top-level-add-subdirs-to-load-path)))

;; Ensure these functions are called after `package-initialize`
(advice-add #'package-initialize :after #'update-load-path)
;;(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

;; Initialize load paths explicitly
(update-load-path)

;;防止反复调用 package-refresh-contents 会影响加载速度
;;(when (not package-archive-contents)
;;  (package-refresh-contents))

;; Requisites

(require 'init-const)
(require 'init-package)
(require 'init-custom)
(require 'init-funcs)
(require 'init-base)
(require 'init-hydra)
(require 'init-ui)
(require 'init-edit)
(require 'init-prog)
(require 'init-check)
(require 'init-proj)
(require 'init-complete)
(require 'init-org)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )




;;; init.el ends here
