;;; setup-fonts.el --- Font and typography configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure fonts and typography settings for better display

;;; Code:

(require 'init-const)
(require 'init-funcs)

;;; Font settings
(defun +my/better-font()
  "Set up fonts for UI."
  (interactive)
  (if (display-graphic-p)
      (progn
        ;; Set default font for Latin characters
        (set-face-attribute 'default nil :font "Fantasque Sans Mono 14")
        ;; Set CJK font for Chinese/Japanese/Korean characters
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "LXGW WenKai Mono" :size 13))))))

(defun +my|init-font(frame)
  "Initialize font for FRAME."
  (with-selected-frame frame
    (if (display-graphic-p)
        (+my/better-font))))

;; Initialize fonts based on Emacs mode
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions #'+my|init-font)
  (+my/better-font))

(provide 'setup-fonts)
;;; setup-fonts.el ends here