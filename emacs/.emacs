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

;; xah fly keys (default layout is dvorak)
(require 'xah-fly-keys)
(xah-fly-keys 1)

;; Ruby mode
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

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
       (set-default-font "Inconsolata-16"))
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
 '(custom-safe-themes
   (quote
    ("f5516b1e20aa2c9edc3d4ed151702a48a574d8aab91ab38721714e1d4a25688d" "1473f4ea26c61b23bc0931088503ba339d4ae0d9ba126cd9001df208f1dfa85e" "f0c817a3706ac81717ac86feb8c17c649fae70a6d49cb639aebfb28409772eb3" "1e918d6e3fb6ecf1f5860a84f30028a5352700e640ea40553c8f32d0d4b7a91f" "94a84f52916d89bcfc8df7900a2fbb9ea6f555ced94edfc6af3f43adfbd830e3" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(package-selected-packages
   (quote
    (xah-fly-keys magit dracula-theme yaml-mode web-mode markdown-mode))))
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
		   (split-string (current-time-string)))
	      ":")))
  (set 'hour (string-to-number hour-str))
  (message hour-str)
  (or (> hour 17) (< hour 6)))

;; use dark theme for night and light theme for day
(if (nightp)
    (progn
      (message "It's night. Loading dark theme")  
      (load-theme 'dracula))
  (progn
    (message "It's day. Loading light theme")  
    (load-theme 'leuven)))
(put 'downcase-region 'disabled nil)
