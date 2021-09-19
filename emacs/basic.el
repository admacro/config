;; basic.el
;; basic settings for emacs

(set-language-environment "UTF-8") ; UTF-8 as default encoding
(setq default-directory "~/prog")

;; frame parameters
(setq default-frame-alist
      '((ns-appearance . 'light)       ; display title bar using system's light theme (dark title color)
        (ns-transparent-titlebar . t)  ; make titlebar same color as Emacs background
        (width . 90)
        (height . 60)))
(setq initial-frame-alist               ; this inherits default-frame-alist
      '((width . 100)
        (left . 850)))

;; appearance
(setq inhibit-startup-message t) ; no startup message
(column-number-mode t) ; display column number in the mode line
(tool-bar-mode -1) ; no tool bar
(scroll-bar-mode -1) ; no scroll bar
(blink-cursor-mode 0) ; no cursor blinking (0 is blinking time interval)
(global-display-line-numbers-mode); dispaly line numbers everywhere

;; editing
(setq show-paren-delay 0)       ; highlight matching paren immediately
(show-paren-mode 1)             ; highlight matching parens
(electric-pair-mode 1)          ; auto insert matching paren
(electric-indent-mode 1)        ; make return key also do indent, globally
(setq-default tab-width 4)      ; default tab width is 4 spaces
(setq-default indent-tabs-mode nil) ; always indent by space, no mixing tab and space

;; file
(global-auto-revert-mode t)   ; auto refresh files
(recentf-mode 1)              ; turn on recent file feature
(setq desktop-restore-frame nil) ; only restore opened files, no restore frame or window
(desktop-save-mode 1)         ; save/restore opened files and windows config
(setq bookmark-save-flag 1)   ; automatically save bookmark when it's changed
(setq auto-save-default nil)  ; stop creating #autosave# files
(setq make-backup-files nil)  ; stop creating backup~ files

;; convenience
(fset 'yes-or-no-p 'y-or-n-p)	      ; y for yes, n for no

;; startup commands
(dired "~/prog")
(recentf-open-files)

;; Close *scratch* buffer
(kill-buffer "*scratch*")
