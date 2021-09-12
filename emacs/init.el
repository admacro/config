;; global valiables
(set-language-environment "UTF-8") ; UTF-8 as default encoding
(setq gc-con-threshold-value (* 1024 1024)) ; 1MB

;; font Variables
(setq proportional-font-family "sf pro text")
(setq monospace-font-family "sf mono")
(setq default-font-size 14)

(load "~/prog/config/emacs/fun.el")
(load "~/prog/config/emacs/pkg/pkg.el")
(load "~/prog/config/emacs/theme/theme.el")
(load "~/prog/config/emacs/abbrevs.el")
(load "~/prog/config/emacs/org/org.el")

;; theme and font
(set-font-size default-font-size)

;; set font for chinese characters
(set-fontset-font
 t
 'han
 (if (member "Songti SC" (font-family-list)) "Songti SC"))

;; Global config
;; appearance
(setq default-frame-alist
	  '(
		(ns-transparent-titlebar . t) ;make titlebar same color as Emacs background
		))

(setq inhibit-startup-message t) ; no startup message
(blink-cursor-mode 0)		; disable cursor
(global-display-line-numbers-mode 1); dispaly line numbers everywhere
(setq column-number-mode t) ; display column number in the mode line
;; (setq frame-title-format nil)	; no frame title
(scroll-bar-mode -1) ;no scrool bar
(tool-bar-mode -1) ; no tool bar

;; editing
(setq-default tab-width 4)		; default tab width is 4 spaces
(setq show-paren-delay 0)		; highlight matching paren immediately
(show-paren-mode 1)				; highlight matching parens
(electric-pair-mode 1)			; auto insert matching paren
(delete-selection-mode t)		; text selection can be deleted or replaced by typing
(put 'downcase-region 'disabled nil)	; enable downcase command
(setq-default indent-tabs-mode nil)		; indent by space, or emacs will mix tab and space

;; file
(recentf-mode 1)		      ; turn on recent file feature
(desktop-save-mode 1)		  ; save/restore opened files and windows config
(setq bookmark-save-flag 1)   ; automatically save bookmark when it's changed
(global-auto-revert-mode t)	  ; auto refresh files
(setq auto-save-default nil)  ; stop creating #autosave# files
(setq make-backup-files nil)  ; stop creating backup~ files

;; convenience
(fset 'yes-or-no-p 'y-or-n-p)	      ; y for yes, n for no
;; (setq scroll-conservatively 15) ; center cursor if a moving step is more than 15 lines, otherwise scroll just enough lines

;; startup commands
(recentf-open-files)
(dired "~/prog")

;; Close *scratch* buffer
(kill-buffer "*scratch*")
