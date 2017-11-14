(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fd2c2d45aca20ba76b25c86beed11a35970ef4ed96d73a4a69d4e427d9a6597d" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defalias 'perl-mode 'cperl-mode)

;; Load custom theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/theme/")
(load-theme 'dracula)

;; Setup font
;;(add-to-list 'default-frame-alist '(font . "Consolas-10"))
;;(set-face-attribute 'default nil :font "Consolas")
(set-default-font "Consolas-11")

;; Set default window size
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 120) ; chars
              (height . 50) ; lines
              ;;
              ))

      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 120)
              (height . 50)
              ;;
              )))
  (progn
    (setq initial-frame-alist
          '(
            (tool-bar-lines . 0)))
    (setq default-frame-alist
          '(
            (tool-bar-lines . 0)))))
;;


