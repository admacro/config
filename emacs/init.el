;; global valiables
(set-language-environment "UTF-8") ; UTF-8 as default encoding
(setq gc-con-threshold-value (* 1024 1024)) ; 1MB

;; set font for chinese characters
(set-fontset-font
 t
 '(#x4e00 . #x9fff)
 (if (member "Songti SC" (font-family-list)) "Songti SC"))

;; font
(setq proportional-font-family "roboto")
(setq monospace-font-family "roboto mono")
(setq default-font-size 15)

;; org projects
(setq org-project-notes-name "notes")
(setq org-project-notes-path "~/prog/notes")
(setq org-project-www-name "www")
(setq org-project-www-path "~/prog/admacro.github.io")

(load "~/prog/config/emacs/fun.el")
(load "~/prog/config/emacs/pkg/pkg.el")
(load "~/prog/config/emacs/theme/theme.el")

;; theme and font
(load-adm-theme)
(set-font-size default-font-size)

;; Global config
;; appearance
(setq default-frame-alist
      '((fullscreen . maximized)      ;start emacs with window maximized
        (ns-transparent-titlebar . t) ;make titlebar same color as Emacs background
        ))
(setq inhibit-startup-message t) ; no startup message
(blink-cursor-mode 0)       ; disable cursor
(global-display-line-numbers-mode 1); dispaly line numbers everywhere
(setq column-number-mode t) ; display column number in the mode line
(setq frame-title-format nil)   ; no frame title
(scroll-bar-mode -1) ;no scrool bar
(tool-bar-mode -1) ; no tool bar
;; behavior
(fset 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(global-auto-revert-mode t) ; auto refresh files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq make-backup-files nil) ; stop creating backup~ files
(setq-default indent-tabs-mode nil) ; indent by space
(setq-default tab-width 4) ; default tab width is 4 spaces
(setq scroll-conservatively 15) ; center cursor if a moving step is more than 15 lines, otherwise scroll just enough lines
(delete-selection-mode t) ; text selection can be deleted or replaced by typing
(electric-pair-mode 1) ; auto insert matching paren
(recentf-mode 1) ;; turn on recent file feature

;; show matching paren immediately
(setq show-paren-delay 0)
(show-paren-mode 1)

;; enable downcase command
(put 'downcase-region 'disabled nil)

;; big minibuffer height, for ido to show choices vertically
;; (setq max-mini-window-height 0.25)

;; startup commands
(recentf-open-files)
(treemacs)

;; Close *scratch* buffer
(kill-buffer "*scratch*")
