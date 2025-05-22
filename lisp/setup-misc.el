;;; setup-misc.el --- Miscellaneous packages -*- lexical-binding: t; -*-

;; For esup
(use-package esup
  :ensure t)

;; For faceup
(use-package faceup
  :ensure t)

;; For gntp
(use-package gntp
  :ensure t)

;; For youdao-dictionary (Commented out as per init-edit.el)
;;(use-package youdao-dictionary
;;  :ensure t
;;  :after popup
;;  :init
;;  (setq youdao-dictionary-app-key "5f99312ee6462e1e")
;;  (setq youdao-dictionary-secret-key "g6u7hTZCpv4vYJ9PD5U0Rnr46N8kGZjl"))

(provide 'setup-misc)
