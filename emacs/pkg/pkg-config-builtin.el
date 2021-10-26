;; make the Escape key do emacs's Ctrl+g.
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; minibuffer enchanced completion
(progn
  (require 'icomplete)
  (icomplete-mode 1)
  (setq icomplete-separator "\n")       ;show choices vertically
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-in-buffer t)
  (define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)
  (define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions)
  )

;; ido
(progn
  (require 'ido)
  (ido-mode 1)
  (setf (nth 2 ido-decorations) "\n") ;; show choices vertically
  (setq ido-everywhere t) ;; enable ido everywhere
  (setq ido-enable-flex-matching t)   ;; show any name that has the chars you typed
  (setq ido-default-file-method 'selected-window) ;; use current pane for newly opened file
  (setq ido-default-buffer-method 'selected-window) ;; use current pane for newly switched buffer
  (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil) ;; stop ido from suggesting when naming new file
  )

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
          (lambda ()
            (ibuffer-auto-mode 1) ; auto refresh ibuffer list
            (ibuffer-switch-to-saved-filter-groups "default")))

;; dired
(require 'dired)
(define-key dired-mode-map (kbd "s") 'xah-dired-sort)

;; Sh Mode
(add-to-list 'auto-mode-alist '("\\Procfile.*\\'" . sh-mode)) ; forego foreman/procfile
(add-to-list 'auto-mode-alist '("\\.*_profile\\'" . sh-mode)) ; .*_profile

;; major modes in variable pitch mode
(add-hook 'Info-mode-hook				; notice the mode name, it's Info, not info
		  (lambda ()
            (interactive)
            (display-line-numbers-mode 0)
            (variable-pitch-mode 1)))

