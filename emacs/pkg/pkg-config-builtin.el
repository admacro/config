;; make the Escape key do emacs's Ctrl+g.
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; fido
(fido-vertical-mode 1)

;; ibuffer
;; Filter buffers into groups
;; more at https://www.emacswiki.org/emacs/IbufferMode
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Programming" (and (not (name . "^magit"))
                             (not (name . "COMMIT_EDITMSG"))
                             (filename . "prog")))
         ("Coding" (and (not (name . "^magit"))
                             (not (name . "COMMIT_EDITMSG"))
                             (filename . "code")))
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

