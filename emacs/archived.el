(global-hl-line-mode 1) ; highlight current line where cursor is

;; default font for variable-pitch mode
(custom-set-faces '(variable-pitch ((t (:family "Baskerville")))))

;; mini-buffer
;; https://github.com/lewang/flx
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(add-hook 'minibuffer-setup-hook (lambda()(setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook (lambda()(setq gc-cons-threshold gc-con-threshold-value)))

;; startup
(setq user-home (getenv "HOME"))
(setq start-dir (concat user-home "/prog"))
(setq project-home (concat user-home "/prog"))
(if (file-directory-p project-home)
    (progn (setq start-dir project-home)))
(dired start-dir)

;; lsp-ui
(setq lsp-ui-doc-border (face-foreground 'default))
(setq lsp-ui-doc-border "black")
(setq lsp-ui-doc-max-width 120)
(setq lsp-ui-doc-max-height 80)

;; functions
;; Use variable width font faces in current buffer
(defun writing-mode ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "IM FELL English PRO" :height 220 :width regular))
  (buffer-face-mode)
  (global-display-line-numbers-mode 0)	; disable line numbers everywhere
  (turn-on-auto-fill))
(define-key xah-fly-dot-keymap (kbd "w") 'writing-mode)


;; unhighlight all hi-lock highlights in current buffer
(defun unhighlight-all-in-buffer()
  "Remove all highlights made by `hi-lock' from the current buffer."
  (interactive)
  (unhighlight-regexp t))
(define-key xah-fly-dot-keymap (kbd "u") 'unhighlight-all-in-buffer)

(defun monospace-brackets-mode ()
  "use monospace font for brackets: [][(){}]"
  (add-hook 'prog-mode-hook
            (lambda()
              (font-lock-add-keywords nil
                                      '(("[][(){}~=/\\;,.'<-_\|>\"!@#$%^&*]"
                                         (0 (add-face-text-property (match-beginning 0) (match-end 0)
                                                                    (list :family monospace-font-family)))))))))
