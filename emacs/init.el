(load "~/prog/config/emacs/pkg-init.el")
(load "~/prog/config/emacs/pkg-config.el")
(load "~/prog/config/emacs/theme/adm-light-theme.el")
(load "~/prog/config/emacs/fun.el")

;;_UTF-8_as_default_encoding
(set-language-environment "UTF-8")


;; ibuffer
;; Filter buffers into groups
;; more at https://www.emacswiki.org/emacs/IbufferMode
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Programming" (and (not (name . "^magit"))
                             (not (name . "COMMIT_EDITMSG"))
                             (filename . "prog")))
         ("Magit" (or (name . "^magit")
                      (name . "COMMIT_EDITMSG")))
         ("Misc" (name . "^\\*")))))
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1) ; auto refresh ibuffer list
             (ibuffer-switch-to-saved-filter-groups "default")))


;; ** Experimenting **
;; Configuration to improve lsp-mode performance
(setq gc-con-threshold-value 1000000) ; 1MB
;; (setq gc-con-threshold-value 100000000) ; 100MB
(setq gc-cons-threshold gc-con-threshold-value)
(setq read-process-output-max (* 1024 1024)) ;; 1mb


;; Emacs GC config for flx-ido
;; https://github.com/lewang/flx
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold gc-con-threshold-value))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; load custom themes
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Global config
(fset 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(global-auto-revert-mode t) ; auto refresh files
(global-display-line-numbers-mode 1); dispaly line numbers everywhere
(global-hl-line-mode 1) ; highlight current line where cursor is
(global-visual-line-mode 1) ; 1 for on, 0 for off
;; (modify-all-frames-parameters (list (cons 'cursor-type 'bar))) ; display cursor as a vertical bar (i-beam: I)
(setq auto-save-default nil) ; stop creating #autosave# files
(setq make-backup-files nil) ; stop creating backup~ files
(setq column-number-mode t) ; display column number in the mode line
(setq inhibit-startup-message t) ; no startup message
(setq-default indent-tabs-mode nil) ; indent by space
(setq-default tab-width 4) ; default tab width is 4 spaces
(setq scroll-conservatively 15) ; center cursor if a moving step is more than 15 lines, otherwise scroll just enough lines 
(delete-selection-mode t) ; text selection can be deleted or replaced by typing
(scroll-bar-mode -1) ;no scrool bar
(tool-bar-mode -1) ; no tool bar
(electric-pair-mode 1) ; auto insert matching paren
(recentf-mode 1) ;; turn on recent file feature

;; show matching paren immediately
(setq show-paren-delay 0)
(show-paren-mode 1)

(if (display-graphic-p)
    (setq initial-frame-alist
          '((fullscreen . maximized)      ;start emacs with window maximized
            (ns-transparent-titlebar . t) ;make titlebar same color as Emacs background
            ;; this will overwrite background in colour theme
            ;; (background-color . "#F4EEE6")  ;set background color (other options: #F1EAE2/#F4EEE6/#F8F3EB, #EBE3E1/#F8F5F4)
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

  (setq ido-everywhere t) ;; enable ido everywhere
  (setq ido-enable-flex-matching t) ;; show any name that has the chars you typed
  (setq ido-default-file-method 'selected-window) ;; use current pane for newly opened file
  (setq ido-default-buffer-method 'selected-window) ;; use current pane for newly switched buffer
  (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil) ;; stop ido from suggesting when naming new file
  )

;; big minibuffer height, for ido to show choices vertically
(setq max-mini-window-height 0.5)

;; enable downcase command
(put 'downcase-region 'disabled nil)

;; auto wrap line in text mode (default maximum line width is 70)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Show recent file list on top and start-dir at bottom on startup
(setq user-home (getenv "HOME"))
(setq start-dir (concat user-home "/prog"))
(setq project-home (concat user-home "/prog"))
(if (file-directory-p project-home)
    (progn (setq start-dir project-home)))
(dired start-dir)
(split-window-below)
(recentf-open-files)

;; Close *scratch* buffer
(kill-buffer "*scratch*")
