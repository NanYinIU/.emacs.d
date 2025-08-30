;;; setup-themes.el --- Theme configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure theme packages and visual appearance settings

;;; Code:

(require 'init-const)
(require 'init-funcs)

;; For ef-theme
(use-package ef-themes
  :ensure t
  :defer t
  :diminish
  :config
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  (setq org-modern-timestamp nil))

;; For moe-theme
(use-package moe-theme
  :ensure t
  :defer t
  :diminish
  :config
  (setq moe-theme-highlight-buffer-id t)
  (setq moe-theme-modeline-color 'cyan))

;; For modus-themes
(use-package modus-themes
  :ensure t
  :defer t
  :diminish
  :config
  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t)
  (setq modus-themes-prompts '(bold italic))
  (setq modus-themes-completions
      '((matches . (extrabold underline))
        (selection . (semibold italic)))))

;; For kanagawa-themes (default theme)
(use-package kanagawa-themes
  :ensure t
  :demand t
  :init
  (setq kanagawa-themes-org-height nil
        kanagawa-themes-org-bold nil)
  (load-theme 'kanagawa-wave t))

(provide 'setup-themes)
;;; setup-themes.el ends here