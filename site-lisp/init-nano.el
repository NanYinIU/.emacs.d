;; Path to nano emacs modules (mandatory)
(add-to-list 'load-path (expand-file-name "nano-emacs" (expand-file-name "site-lisp" user-emacs-directory)))

;;(require 'nano-layout)
;;(add-to-list 'load-path (expand-file-name "site-lisp/nano-emacs" user-emacs-directory))
;; Theming Command line options (this will cancel warning messages)
(add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
(add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
(add-to-list 'command-switch-alist '("-compact" . (lambda (args))))

;; Theme
(require 'nano-base-colors)
(require 'nano-faces)
(require 'nano-theme)
(require 'nano-theme-dark)
(require 'nano-theme-light)


;;(cond
;; ((member "-default" command-line-args) t)
;; ((member "-dark" command-line-args) (nano-theme-set-dark))
;; (t (nano-theme-set-light)))
;;(call-interactively 'nano-refresh-theme)


;; Nano default settings (optional)
;;(require 'nano-defaults)

;; Nano header & mode lines (optional)
(require 'nano-modeline)

;; Compact layout (need to be loaded after nano-modeline)
(when (member "-compact" command-line-args)
  (require 'nano-compact))


(require 'nano-splash)
;; Welcome message (optional)
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O edition")
  (message (format "Initialization time: %s" (emacs-init-time))))
;; minibuffer  needs mini frame
(use-package mini-frame)
(require 'nano-colors)
;;(require 'nano-minibuffer)
(require 'nano-command)
;;(require 'nano-agenda)


;; custom content

(setq nano-font-family-monospaced "Maple Mono NL")
(setq nano-font-size 13)
;;(setq nano-font-family-proportional (face-attribute 'variable-width :family))

(nano-theme-set-dark)
(call-interactively 'nano-refresh-theme)


(provide 'init-nano)

;;; init-nano.el ends here
