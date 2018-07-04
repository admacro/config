(load "~/prog/config/emacs/pkg.el")

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
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Global config
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
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
              (width . 155) ; chars
              (height . 45) ; lines
	      ;; (background-color . "WhiteSmoke")
              ))

      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 150)
              (height . 40)
	      ;; (background-color . "WhiteSmoke")
              )))
  )

;; Set cursor type
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))

;; highlight current line, auto insert matching paren, and display line numbers 
(global-hl-line-mode 1)
(electric-pair-mode 1)
(global-display-line-numbers-mode 1)

;; show matching paren
(setq show-paren-delay 0)
(show-paren-mode 1)

;; enable downcase command
(put 'downcase-region 'disabled nil)

;; Check if time is between 6 PM and 6 AM, aka night time
(defun nightp ()
  (set 'hour-str 
       (car (split-string
	     (nth 3
		  (split-string (current-time-string)))
	     ":")))
  (set 'hour (string-to-number hour-str))
  (message hour-str)
  (or (> hour 16) (< hour 6)))

;; use dark theme for night and light theme for day
(if (nightp)
    (progn
      (message "It's night. Loading dark theme")  
      (load-theme 'faff))
  (progn
    (message "It's day. Loading light theme")  
    (load-theme 'leuven)))


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
