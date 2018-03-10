;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;; MELPA Stable
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; enhanced Ruby mode
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))

;; Web Mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)) ; eRuby

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
;; binding the ENTER key to `newline-and-indent`
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Global config
(setq make-backup-files nil) ;no backup files
(setq column-number-mode t) ; display column number in the mode line
(global-auto-revert-mode t) ; auto refresh files

;; Setup font
(cond ((string-equal system-type "windows-nt") ; Windows
       (set-default-font "Consolas-11"))
      ((string-equal system-type "darwin") ; Mac
       (set-default-font "Inconsolata-20"))
      ;;      ((String-equal system-type "gnu/linux") ; Linux
      ;;     (set-default-font "Inconsolata-16"))
      )

;; Set default window size
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 90) ; chars
              (height . 30) ; lines
;;	      (background-color . "white smoke")
              ))

      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 90)
              (height . 30)
;;   	      (background-color . "white smoke")
              ;;
              )))
  )

;; Set cursor type
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))

;; turn on highlighting current line
(global-hl-line-mode 1)

;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (enh-ruby-mode yaml-mode web-mode markdown-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Check if time is between 6 PM and 6 AM, aka night time
(defun nightp ()
  (set 'hour-str 
	(car (split-string
	      (nth 3
		   (split-string (current-time-string) " "))
	      ":")))
  (set 'hour (string-to-number hour-str))
  (or (> hour 18) (< hour 6)))

;; use dark theme for night and light theme for day
(if (nightp)
    (progn
      (message "It's night. Loading dark theme")  
      (load-theme 'misterioso))
  (progn
    (message "It's day. Loading light theme")  
    (load-theme 'leuven)))
