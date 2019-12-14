(load "~/prog/config/emacs/pkg.el")
(load "~/prog/config/emacs/fun.el")

;;_UTF-8_as_default_encoding
(set-language-environment "UTF-8")

;; Emacs GC config for flx-ido 
;; https://github.com/lewang/flx
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 100000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; load custom themes
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Global config
(delete-selection-mode t)		; text selection can be deleted or replaced by typing
(electric-pair-mode 1)			; auto insert matching paren
(fset 'yes-or-no-p 'y-or-n-p)		; y or n is enough
(global-auto-revert-mode t) ; auto refresh files
(global-display-line-numbers-mode 1)	; dispaly line numbers everywhere
(global-hl-line-mode 1)		     ; highlight current line where cursor is
(global-visual-line-mode 1) ; 1 for on, 0 for off
(modify-all-frames-parameters (list (cons 'cursor-type 'bar))) ; display cursor as a vertical bar
(scroll-bar-mode -1)			; no scrool bar
(setq auto-save-default nil) ; stop creating #autosave# files
(setq column-number-mode t) ; display column number in the mode line
(setq inhibit-startup-message t)	; no startup message
(setq make-backup-files nil) ; stop creating backup~ files
(setq-default indent-tabs-mode nil)	; indent by space
(setq-default tab-width 2)  ; default tab width is 2 spaces
(tool-bar-mode -1)			; no tool bar

;; show matching paren immediately
(setq show-paren-delay 0)
(show-paren-mode 1)

;; (set-default-font "Inconsolata-20")
;; (set-default-font "Menlo-16") ;; Menlo is same as DejaVu Sans Mono
(set-default-font "PT Mono-18")

(if (display-graphic-p)
    (setq initial-frame-alist
          '(
            (fullscreen . maximized)      ;start emacs with window maximized
            (ns-transparent-titlebar . t) ;make titlebar same color as Emacs background
            (background-color . "#F4EEE6")  ;set background color (other options: F1EAE2/F8F3EB, EBE3E1/F8F5F4)
            )))
(setq default-frame-alist initial-frame-alist)

(progn
  ;; make buffer switch command do suggestions, also for find-file command
  (require 'ido)
  (ido-mode 1)
  ;; show choices vertically
  (if (version< emacs-version "25")
      (progn
        (make-local-variable 'ido-separator)
        (setq ido-separator "\n"))
    (progn
      (make-local-variable 'ido-decorations)
      (setf (nth 2 ido-decorations) "\n")))

  ;; enable ido everywhere
  (setq ido-everywhere t)
  ;; show any name that has the chars you typed
  (setq ido-enable-flex-matching t)
  ;; use current pane for newly opened file
  (setq ido-default-file-method 'selected-window)
  ;; use current pane for newly switched buffer
  (setq ido-default-buffer-method 'selected-window)
  ;; stop ido from suggesting when naming new file
  (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)
  )

;; big minibuffer height, for ido to show choices vertically
(setq max-mini-window-height 0.5)

;; enable downcase command
(put 'downcase-region 'disabled nil)

;; auto wrap line in text mode (default maximum line width is 70)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq user-home (getenv "HOME"))
(setq start-dir (concat user-home "/prog"))
(setq project-home (concat user-home "/prog/bobcat"))
(if (file-directory-p project-home)
    (progn (setq start-dir project-home)))
(dired start-dir)
(xah-fly-command-mode-activate)
