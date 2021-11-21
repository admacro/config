;; basic.el
;; basic settings for emacs

;; UTF-8 as default encoding
(set-language-environment "UTF-8")      ; for emacs environment
(set-default-coding-systems 'utf-8-unix); for newly created buffers, files and file names

(setq default-directory "~/prog")

;; frame and window parameters
(progn
  (setq default-frame-alist
        '((ns-appearance . light)     ; display title bar using system's light theme (dark title color)
          (ns-transparent-titlebar . t); make titlebar same color as Emacs background
          (width . 150)
          (height . 100)))
  ;; initial-frame-alist inherits default-frame-alist
  (if (> (display-pixel-width) 1600)
      (setq initial-frame-alist         ; for external big monitor 4k
            '((left . 600)))
    (setq initial-frame-alist         ; for laptop builtin screen 1440x900
          '((left . 50))))

  ;; new frame title bar theme is not set to light somehow, add a hook to set it manually
  (defun set-ns-appearance-light (frame)
    (set-frame-parameter frame 'ns-appearance 'light))
  (add-hook 'after-make-frame-functions 'set-ns-appearance-light)
  )

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

;; recommanded by xah fly key
(delete-selection-mode 1)           ; make typing delete/overwrites selected text
(setq shift-select-mode nil)        ; disable shift select

;; file
(global-auto-revert-mode t)   ; auto refresh files

(progn
  (require 'recentf)
  (recentf-mode 1)              ; turn on recent file feature
  )

(progn
  (desktop-save-mode 1)         ; save/restore opened files and windows config
  (setq desktop-restore-frames nil) ; no saving or restoring frame and window configuration
  )

(setq bookmark-save-flag 1)   ; auto save bookmark when it's changed
(setq auto-save-default nil)  ; no auto creating #autosave# files
(setq make-backup-files nil)  ; no auto creating backup~ files
(setq create-lockfiles nil)   ; no auto creating lock files, refresh files immediately

;; convenience
(fset 'yes-or-no-p 'y-or-n-p)	      ; y for yes, n for no
(setq scroll-conservatively most-positive-fixnum) ; never center point when scroll

;; minibuffer
(setq max-mini-window-height 0.5)

;; startup commands
(dired "~/prog")
(recentf-open-files)

;; Close *scratch* buffer
(kill-buffer "*scratch*")
