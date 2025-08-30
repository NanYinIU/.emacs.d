;;; setup-org.el --- Org mode and its ecosystem -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is now a reference to the split Org mode modules.
;; The actual functionality has been moved to specialized modules:
;; - setup-org-core.el    : Core Org mode functionality
;; - setup-org-roam.el    : Org Roam configuration
;; - setup-org-capture.el : Capture templates and workflows
;; - setup-org-agenda.el  : Agenda and scheduling
;; - setup-org-ui.el      : Visual enhancements

;;; Code:

;; This file is kept for compatibility but is no longer needed.
;; All Org functionality has been moved to specialized modules.
;; Please remove the (require 'setup-org) line from init.el
;; and ensure the new Org modules are loaded instead.

(provide 'setup-org)
;;; setup-org.el ends here