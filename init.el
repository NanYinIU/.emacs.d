;; packages
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

;; Command-line switch definitions (moved from site-lisp/init-nano.el)
(add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
(add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
(add-to-list 'command-switch-alist '("-compact" . (lambda (args))))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Set charset and language environment (moved from init-base.el)
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-next-selection-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq system-time-locale "C")
(if sys/win32p
    (add-to-list 'process-coding-system-alist
                 '("cmdproxy" utf-8 . gbk))
  (set-selection-coding-system 'utf-8))

;; Better defaults
;; (setq initial-scratch-message nil)
(setq inhibit-splash-screen t)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")     ; For adaptive-fill-mode
(setq adaptive-fill-first-line-regexp "^* *$")                ; For adaptive-fill-mode
(setq delete-by-moving-to-trash t)                             ; Deleting files go to OS's trash folder
(setq make-backup-files nil)                                   ; Forbid to make backup files
(setq auto-save-default nil)                                   ; Disable auto save
(setq set-mark-command-repeat-pop t)                           ; Repeating C-SPC after popping mark pops it again
;; (setq kill-whole-line t)                                   ; Kill line including '\n'
(setq visible-bell t)                                          ; Use visible bell instead of sound
(setq inhibit-compacting-font-caches t)                      ; Don’t compact font caches during GC
(setq word-wrap-by-category t)                                 ; Optimize word wrapping for CJK text
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*") ; Define sentence end for CJK
(setq sentence-end-double-space nil)                           ; No double space after sentence end

(setq-default major-mode 'text-mode)

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

;; (setq auto-save-default nil) ; This is already set in "Better defaults"
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
;; 更改光标的样式
;;(setq cursor-type 'bar)

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
(global-set-key (kbd "<s-return>") #'rectangle-mark-mode)
(bind-keys ("s-r"     . revert-this-buffer)
           ("C-x K"   . delete-this-file)
           ("C-c C-l" . reload-init-file))

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
  (dolist (dir '("lisp" "site-lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))
;; Add subdirectories inside "site-lisp" to `load-path`


;; Ensure these functions are called after `package-initialize`
(advice-add #'package-initialize :after #'update-load-path)
;;(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

;; Initialize load paths explicitly
(update-load-path)

;;防止反复调用 package-refresh-contents 会影响加载速度
;;(when (not package-archive-contents)
;;  (package-refresh-contents))

;; Requisites (New Order)
(require 'init-const)       ; System constants
(require 'init-funcs)       ; Custom general functions
(require 'init-package)     ; Package system bootstrap (use-package)
(require 'init-custom)      ; User defcustoms and early overrides

;; Load new topical modules
(require 'setup-core)
;; (require 'init-base)        ; Remaining non-package settings from original init-base -- DELETED as init-base.el is deleted
(require 'setup-ui)
;; (require 'init-ui)          ; Remaining non-package settings from original init-ui -- DELETED as init-ui.el is deleted
(require 'setup-hydra)
(require 'setup-completion)
(require 'setup-editing)
(require 'setup-org)
(require 'setup-programming)
(require 'setup-project)
(require 'setup-misc)

;; Load site-lisp configurations if any are still needed directly
;; (require 'init-nano) ; NANO theme is now primarily via setup-ui's nano-local-system.
                        ; init-nano.el still has command-line switches and load-path.
                        ; It should be loaded if those are still desired.
                        ; For now, keep it commented as in the original state.




;;; init.el ends here
