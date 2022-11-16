(global-hl-line-mode 1) ; highlight current line where cursor is

;; default font for variable-pitch mode
(custom-set-faces '(variable-pitch ((t (:family "Baskerville")))))

;; (delete-selection-mode t)        ; text selection can be deleted or replaced by typing
;; (put 'downcase-region 'disabled nil) ; enable downcase command

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

;; unhighlight all hi-lock highlights in current buffer
(defun unhighlight-all-in-buffer()
  "Remove all highlights made by `hi-lock' from the current buffer."
  (interactive)
  (unhighlight-regexp t))
(define-key xah-fly-leader-keymap (kbd ". u") 'unhighlight-all-in-buffer)

(defun monospace-brackets-mode ()
  "use monospace font for brackets: [][(){}]"
  (add-hook 'prog-mode-hook
            (lambda()
              (font-lock-add-keywords nil
                                      '(("[][(){}~=/\\;,.'<-_\|>\"!@#$%^&*]"
                                         (0 (add-face-text-property (match-beginning 0) (match-end 0)
                                                                    (list :family monospace-font-family)))))))))

;; ido, icomplete, and fido
(if (version< emacs-version "28.1")
    (progn
      (progn
        ;; make buffer switch command do suggestions, also for find-file command
        (require 'ido)
        (ido-mode 1)
        ;; show choices vertically
        (setf (nth 2 ido-decorations) "\n")
        ;; show any name that has the chars you typed
        (setq ido-enable-flex-matching t)
        ;; use current pane for newly opened file
        (setq ido-default-file-method 'selected-window)
        ;; use current pane for newly switched buffer
        (setq ido-default-buffer-method 'selected-window)
        )
      (progn
        ;; minibuffer enhanced completion icomplete
        (require 'icomplete)
        (icomplete-mode 1)
        ;; show choices vertically
        (setq icomplete-separator "\n")
        (setq icomplete-hide-common-prefix nil)
        (setq icomplete-in-buffer t)
        (define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)
        (define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions)))
  (fido-vertical-mode 1))

;; configurations
;; lsp-java
(config-pkg 'lsp-java
            (lambda()
              (require 'lsp-java) ; use 'lsp-java-boot for sprint boot projects

              (add-hook 'java-mode-hook 'lsp-deferred)
              (add-hook 'java-mode-hook 'lsp-reorganize-code-before-save)

              ;; organize imports when saving buffer
              (setq lsp-java-save-actions-organize-imports t)
              (setq lsp-java-vmargs
                    (list
                     "-noverify"
                     "-Xms1G"
                     "-Xmx2G"
                     "-XX:+UseG1GC"
                     "-XX:+UseStringDeduplication"))))

;; Ruby robe
(config-pkg 'robe
            (lambda()
              (setq ruby-insert-encoding-magic-comment nil)
              (setq inf-ruby-console-environment "development")
              (add-hook 'ruby-mode-hook 'robe-mode)
              (add-hook 'ruby-mode-hook 'robe-start)
              ;; register jump implmentation
              (puthash 'ruby-mode 'robe-jump jump-map)
              ;; ruby code navigation
              (global-set-key (kbd "s-r r") 'inf-ruby)
              (global-set-key (kbd "s-r c") 'inf-ruby-console-auto)))

;; Rest client
(config-pkg 'restclient
            (lambda()
              (autoload 'restclient "restclient")
              (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
              (add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))))

;; Web Mode
(config-pkg 'web-mode
            (lambda()
              (autoload 'web-mode "web-mode")
              (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
              (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))))

;; Apib-mode (major mode for editing API blueprint file)
(config-pkg 'apib-mode
            (lambda()
              (autoload 'apib-mode "apib-mode")
              (add-to-list 'auto-mode-alist '("\\.api\\'" . apib-mode))
              (add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))
              (add-to-list 'auto-mode-alist '("\\.blueprint\\'" . apib-mode))))
