;;; setup-languages.el --- Language-specific configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure support for specific programming languages and file types

;;; Code:

(require 'init-const)
(require 'init-funcs)

;; For json-mode - JSON file support
(use-package json-mode
  :ensure t
  :defer t
  :diminish
  :mode "\\.json\\'")

;; For markdown-mode - Markdown and GFM support
(use-package markdown-mode
  :ensure t
  :defer t
  :diminish
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"
        markdown-fontify-code-blocks-natively t))

;; For protobuf-mode - Protocol Buffers support
(use-package protobuf-mode
  :ensure t
  :defer t
  :diminish
  :hook (protobuf-mode . (lambda ()
                           "Set up Protobuf's imenu generic expressions."
                           (setq imenu-generic-expression
                                 '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

;; For yaml-mode - YAML file support
(use-package yaml-mode
  :ensure t
  :defer t
  :diminish
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(provide 'setup-languages)
;;; setup-languages.el ends here