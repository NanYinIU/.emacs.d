;;; init-const.el --- Define constants -*- lexical-binding: t -*-

;;; Commentary:
;; Define constants used across the configuration

;;; Code:

;; System-specific detection
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/mac-ns-p
  (eq window-system 'ns)
  "Are we running on a GNUstep or Macintosh Cocoa display?")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst sys/mac-port-p
  (eq window-system 'mac)
  "Are we running a macport build on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

;; Emacs version checks
(defconst emacs/>=29p (>= emacs-major-version 29)
  "Emacs version is 29 or above.")

(defconst emacs/>=28p (>= emacs-major-version 28)
  "Emacs version is 28 or above.")

(defconst emacs/>=27p (>= emacs-major-version 27)
  "Emacs version is 27 or above.")

;; Refer to https://emacs.stackexchange.com/questions/28736/emacs-tech-support-people-say-they-need-my-system-configuration-what-is-that
(defconst sys/is-a-macOS
  (if (eq system-type 'darwin) t nil)
  "Is this machine macOS?")

(defconst sys/is-a-linux
  (if (eq system-type 'gnu/linux) t nil)
  "Is this machine Linux?")

(defconst sys/is-a-windows
  (if (eq system-type 'windows-nt) t nil)
  "Is this machine Windows?")

;; Handle Windows-specific settings when using Emacs via WSL
(defconst sys/wsl
  (and sys/is-a-linux
       (not (null (string-match "Linux.*Microsoft.*Linux"
                                (shell-command-to-string "uname -a"))))
       t)
  "Is this WSL?")

;; Process encoding
(if sys/win32p
    (add-to-list 'process-coding-system-alist
                 '("cmdproxy" utf-8 . gbk))
  (set-selection-coding-system 'utf-8))

(provide 'init-const)
;;; init-const.el ends here