;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file loads the complete Emacs configuration

;;; Code:

;; Package repositories
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

;; Add "lisp" and "site-lisp" to load-path
(defun update-load-path (&rest _)
  "Update the `load-path` to prioritize personal configurations."
  (dolist (dir '("lisp" "site-lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

;; Initialize load paths explicitly
(update-load-path)

;; Command-line switch definitions
(add-to-list 'command-switch-alist '("-dark"      . (lambda (args))))
(add-to-list 'command-switch-alist '("-light"     . (lambda (args))))
(add-to-list 'command-switch-alist '("-default"   . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-help"   . (lambda (args))))
(add-to-list 'command-switch-alist '("-compact"   . (lambda (args))))

;; Encoding and language settings
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
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

;; Better defaults
(setq inhibit-splash-screen t)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")     ; For adaptive-fill-mode
(setq adaptive-fill-first-line-regexp "^* *$")                ; For adaptive-fill-mode
(setq delete-by-moving-to-trash t)                            ; Deleting files go to OS's trash folder
(setq make-backup-files nil)                                  ; Forbid to make backup files
(setq auto-save-default nil)                                  ; Disable auto save
(setq set-mark-command-repeat-pop t)                          ; Repeating C-SPC after popping mark pops it again
(setq visible-bell t)                                         ; Use visible bell instead of sound
(setq inhibit-compacting-font-caches t)                       ; Don't compact font caches during GC
(setq word-wrap-by-category t)                                ; Optimize word wrapping for CJK text
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*") ; Define sentence end for CJK
(setq ispell-dictionary "en_US")
(setq sentence-end-double-space nil)                          ; No double space after sentence end
(setq-default major-mode 'text-mode)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

;; Tab and Space
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; UI settings
(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; Basic modes
(global-hl-line-mode)
(global-display-line-numbers-mode)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)

;; Initialize package system
(require 'package)

;; Ensure these functions are called after `package-initialize`
(advice-add #'package-initialize :after #'update-load-path)

;; Key Modifiers
(cond
 ((eq system-type 'windows-nt)
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

;; Set up keybindings for utility functions
(global-set-key (kbd "<f5>") #'revert-current-buffer)
(bind-keys ("s-r"     . revert-this-buffer)
           ("C-x K"   . delete-this-file)
           ("C-c C-l" . reload-init-file))

;; Load core configuration modules
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; Native compilation settings
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  ;; Silence compiler warnings as they can be noisy
  (setq native-comp-async-report-warnings-errors nil)

  ;; Set the right directory to store the cache
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache/" user-emacs-directory))

  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t)

  ;; Limit the number of compilation jobs
  (setq native-comp-async-jobs-number 2)

  ;; Don't show compilation warnings in echo area
  (setq native-comp-warning-on-missing-source nil))

;; step 1: Start the Emacs Server
(server-start)
;; step 2: use alias e='emacsclient -c -n' to alias emacsclient open files

;; Load modules in proper order
(require 'init-const)       ; System constants
(require 'init-funcs)       ; Custom general functions
(require 'init-package)     ; Package system bootstrap (use-package)
(require 'init-custom)      ; User defcustoms and early overrides

;; Load functional modules
(require 'setup-core)       ; Core packages and settings
(require 'setup-ui)         ; UI related settings and packages
(require 'setup-completion) ; Completion frameworks
(require 'setup-editing)    ; Editing tools and enhancements
(require 'setup-org)        ; Org-mode configuration
(require 'setup-programming) ; Programming language support
(require 'setup-project)    ; Project management
(require 'setup-misc)       ; Miscellaneous settings
(require 'setup-ai)

;; Variables configured via the custom UI go in custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
